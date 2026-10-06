"""Inverse of MergingEvent.hh in MMFT f6e155442, not an ID/order heuristic.

Consumed parents remain in the raw output with their event-time geometry. That
geometry, oriented boundaries, topology and volume must explain a unique binary
event. Ambiguity is an error; chemistry must never run on a guessed genealogy.
"""
from itertools import combinations
from collections import Counter
import math

ENGINE = "0.2.2.dev54+gf6e155442"
POSITION_TOL = 1e-9  # normalized channel position, NOT a time-coalescing tolerance
VOLUME_RTOL = 1e-9


class ReconstructionError(ValueError):
    pass


def boundary(b):
    return (int(b["position"]["channel"]), bool(b["volumeTowards1"]), float(b["position"]["position"]))


def equal_boundaries(a, b):
    a, b = sorted(a), sorted(b)
    return len(a) == len(b) and all(x[:2] == y[:2] and abs(x[2]-y[2]) <= POSITION_TOL for x, y in zip(a, b))


def single(d):
    bs = [boundary(b) for b in d["boundaries"]]
    return len(bs) == 2 and bs[0][0] == bs[1][0] and not d["channels"]


def occupies_node(d, node, topology):
    if single(d):
        return False  # MMFT bifurcation lookup excludes single-channel droplets
    return any(node in topology[c] for c in d["channels"]) or any(
        topology[c][0 if toward else 1] == node for c, toward, _ in map(boundary, d["boundaries"]))


def explanations(a, b, child, topology):
    if not math.isclose(a["volume"]+b["volume"], child["volume"], rel_tol=VOLUME_RTOL, abs_tol=0):
        return []
    ab, bb, cb = [[boundary(x) for x in d["boundaries"]] for d in (a, b, child)]
    result = []
    full = list(a["channels"]) + list(b["channels"])
    for i, x in enumerate(ab):
        for j, y in enumerate(bb):
            if x[0] != y[0] or x[1] == y[1] or abs(x[2]-y[2]) > POSITION_TOL:
                continue
            channels = full + ([x[0]] if not single(a) and not single(b) else [])
            if Counter(channels) == Counter(child["channels"]) and equal_boundaries(ab[:i]+ab[i+1:]+bb[:j]+bb[j+1:], cb):
                result.append({"kind": "channel", "channel": x[0], "position": x[2]})
    for arriving, other, arr_bs, other_bs in ((a, b, ab, bb), (b, a, bb, ab)):
        for i, (c, toward, pos) in enumerate(arr_bs):
            tip = 1 if toward else 0
            node = topology[c][tip]
            if abs(pos-tip) > POSITION_TOL or not occupies_node(other, node, topology):
                continue
            channels = full + ([c] if not single(arriving) else [])
            if Counter(channels) == Counter(child["channels"]) and equal_boundaries(arr_bs[:i]+arr_bs[i+1:]+other_bs, cb):
                result.append({"kind": "junction", "node": node, "channel": c})
    return result


def validate_raw(raw, topology, initial_ids):
    states = raw.get("network")
    if not isinstance(states, list) or len(states) < 2:
        raise ReconstructionError("Missing/truncated state sequence")
    last_time, last_ids, volumes = -1.0, set(), {}
    for i, state in enumerate(states):
        t = state.get("time")
        if not isinstance(t, (float, int)) or not math.isfinite(t) or t < last_time:
            raise ReconstructionError(f"Invalid/nonmonotonic time at state {i}")
        drops = state.get("bigDroplets")
        if not isinstance(drops, list):
            raise ReconstructionError(f"Missing droplets at state {i}")
        ids = [d.get("id") for d in drops]
        if any(type(v) is not int for v in ids) or len(set(ids)) != len(ids) or not last_ids <= set(ids):
            raise ReconstructionError(f"Duplicate, invalid or disappearing IDs at state {i}")
        if i == 0 and set(ids) != set(initial_ids):
            raise ReconstructionError("Initial droplet IDs do not match adapter manifest")
        for d in drops:
            v = d.get("volume", 0)
            if not math.isfinite(v) or v <= 0 or (d["id"] in volumes and volumes[d["id"]] != v):
                raise ReconstructionError("Invalid/changing droplet volume")
            volumes[d["id"]] = v
            if not isinstance(d.get("boundaries"), list) or not isinstance(d.get("channels"), list):
                raise ReconstructionError("Missing droplet geometry")
            for b in d["boundaries"]:
                if type(b.get("volumeTowards1")) is not bool:
                    raise ReconstructionError("Missing boundary orientation")
                c, _, p = boundary(b)
                if c not in topology or not math.isfinite(p) or not -POSITION_TOL <= p <= 1+POSITION_TOL:
                    raise ReconstructionError("Unknown channel or invalid boundary position")
            if any(c not in topology for c in d["channels"]):
                raise ReconstructionError("Unknown fully occupied channel")
        last_time, last_ids = t, set(ids)
    return states


def reconstruct(raw, topology, injections, sinks=()):
    """injections maps ENGINE ID -> {time, volume, ...}; ordered snapshots preserved."""
    states = validate_raw(raw, topology, injections)
    # A near-sink boundary can still move a few ULPs while another simultaneous
    # event is processed. Do not retire it early merely because it is within the
    # spatial tolerance. The final frozen geometry is required as well as contact.
    last_change, last_geometry = {}, {}
    def geometry(d):
        return (d["volume"], tuple(sorted(map(boundary, d["boundaries"]))), tuple(sorted(d["channels"])))
    for index, state in enumerate(states):
        for d in state["bigDroplets"]:
            value = geometry(d)
            if last_geometry.get(d["id"]) != value:
                last_change[d["id"]] = index
                last_geometry[d["id"]] = value
    seen, active, ended, events, lifecycle = set(injections), set(), {}, [], {}
    for index, state in enumerate(states):
        drops = {d["id"]: d for d in state["bigDroplets"]}
        t = state["time"]
        for did, frozen in ended.items():
            if geometry(drops[did]) != geometry(frozen):
                raise ReconstructionError(f"Consumed/exited droplet {did} changed after its terminal event")
        for did, spec in injections.items():
            if did not in lifecycle and drops[did]["boundaries"]:
                if not math.isclose(t, spec["time"], rel_tol=1e-12, abs_tol=1e-12):
                    raise ReconstructionError(f"Injection {did}: output time differs from manifest")
                if not math.isclose(drops[did]["volume"], spec["volume"], rel_tol=VOLUME_RTOL):
                    raise ReconstructionError(f"Injection {did}: volume differs from manifest")
                active.add(did)
                lifecycle[did] = {"birth": index, "birth_time": t, "parents": []}
                events.append({"type": "injection", "state": index, "time": t, "droplet": did})
        pending = set(drops)-seen
        # Enumerate complete assignments, not a greedy choice based on IDs. This
        # also handles independent groups and binary cascades within a snapshot.
        solutions = {}
        budget = [0]
        def solve(todo, live, chain):
            budget[0] += 1
            if budget[0] > 10000:
                raise ReconstructionError("Merge assignment search limit exceeded; simplify or inspect raw output")
            if not todo:
                key = tuple(sorted((c, tuple(sorted((a, b)))) for c, a, b, _ in chain))
                solutions[key] = chain
                return
            if len(solutions) > 1:
                return
            for child in sorted(todo):
                for a, b in combinations(sorted(live), 2):
                    evidence = explanations(drops[a], drops[b], drops[child], topology)
                    if evidence:
                        solve(todo-{child}, (live-{a, b}) | {child}, chain+[(child, a, b, evidence)])
        if pending:
            solve(pending, active, [])
            if len(solutions) != 1:
                raise ReconstructionError(f"State {index}, t={t}: {'ambiguous' if solutions else 'unexplained'} merges {sorted(pending)}; chemical results blocked")
            for child, a, b, evidence in next(iter(solutions.values())):
                for parent in (a, b):
                    lifecycle[parent].update(death=index, death_time=t, terminal="merge")
                    active.remove(parent)
                    ended[parent] = drops[parent]
                active.add(child)
                lifecycle[child] = {"birth": index, "birth_time": t, "parents": [a, b]}
                events.append({"type": "merge", "state": index, "time": t, "droplet": child,
                               "parents": [a, b], "evidence": evidence,
                               "volume_residual": drops[child]["volume"]-drops[a]["volume"]-drops[b]["volume"],
                               "confidence": "unique-under-pinned-event-model"})
        # Pinned MMFT removes the entire droplet when its leading boundary reaches
        # a sink. Frozen position alone is NEVER interpreted as an exit.
        for did in sorted(active.copy()):
            for c, toward, pos in map(boundary, drops[did]["boundaries"]):
                tip = 1 if toward else 0
                if abs(pos-tip) <= POSITION_TOL and topology[c][tip] in sinks and index >= last_change[did]:
                    active.remove(did)
                    ended[did] = drops[did]
                    lifecycle[did].update(death=index, death_time=t, terminal="sink")
                    events.append({"type": "exit", "state": index, "time": t, "droplet": did})
                    break
        seen = set(drops)
    missing = set(injections)-set(lifecycle)
    if missing:
        raise ReconstructionError(f"Scheduled injections absent from completed output: {sorted(missing)}")
    for did in active:
        lifecycle[did].update(death=len(states), death_time=states[-1]["time"], terminal="stalled")
    return {"events": events, "lifecycle": lifecycle, "tolerances": {"position": POSITION_TOL, "volume_relative": VOLUME_RTOL},
            "engine": ENGINE, "warnings": [f"Droplet {d} remains in network at engine termination" for d in sorted(active)]}
