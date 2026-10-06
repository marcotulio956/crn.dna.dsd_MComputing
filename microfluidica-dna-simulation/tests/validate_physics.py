"""Independent physical/chemical oracles against the real bundle, via localhost."""
from copy import deepcopy
import math
from pathlib import Path
import sys
import time
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from tests.integration import request
from backend.engine import atomic_json


def base():
    return {"schema_version": 2, "name": "Analytical validation", "nodes": [
        {"id": "in", "name": "in", "x": 0., "y": 0.},
        {"id": "out", "name": "out", "x": .01, "y": 0., "sink": True, "ground": True}],
        "channels": [{"id": "c", "source": "in", "target": "out", "width": 50e-6, "height": 30e-6}],
        "pumps": [{"id": "p", "source": "out", "target": "in", "value": 1.5e-13}],
        "species": [{"id": "A", "name": "A"}, {"id": "B", "name": "B"}],
        "reactions": [{"id": "r", "reactants": ["A"], "products": ["B"], "rate": .2}],
        "injections": [{"id": "a", "name": "a", "channel": "c", "concentrations": {"A": 2}, "time": .1}],
        "settings": {"sample_count": 501}}


def result(project):
    job = request("runs", project)
    for _ in range(240):
        state = request("runs/"+job["id"])
        if state["status"] == "completed":
            return request("runs/"+job["id"]+"/result")
        if state["status"] == "failed":
            raise AssertionError(state["error"])
        time.sleep(.25)
    raise AssertionError("Worker did not complete")


def fork_network(prefix, branches=2):
    project = base()
    project["nodes"] = [{"id": f"in{i}", "name": f"in{i}", "x": 0, "y": (i-(branches-1)/2)*.002} for i in range(branches)] + [
        {"id": "junction", "name": "junction", "x": .005, "y": 0},
        {"id": "out", "name": "out", "x": .02, "y": 0, "sink": True, "ground": True}]
    # Three equal-distance inlets on a circle for the triple collision oracle.
    if branches == 3:
        project["nodes"][0].update(x=0, y=0)
        project["nodes"][1].update(x=.005, y=-.005)
        project["nodes"][2].update(x=.005, y=.005)
    project["channels"] = [{"id": f"c{i}", "source": f"in{i}", "target": "junction", "width": 50e-6, "height": 30e-6} for i in range(branches)] + [
        {"id": "out-c", "source": "junction", "target": "out", "width": 50e-6, "height": 30e-6}]
    project["pumps"] = [{"id": f"p{i}", "source": "out", "target": f"in{i}", "value": 1.5e-13} for i in range(branches)]
    project["injections"] = [{"id": f"d{i}", "name": f"d{i}", "channel": f"c{i}", "concentrations": {"A": i+1}, "time": .1} for i in range(branches)]
    project["reactions"] = []
    for key in ("nodes", "channels", "pumps", "injections"):
        for item in project[key]:
            item["id"] = prefix+item["id"]
            for ref in ("source", "target", "channel"):
                if ref in item: item[ref] = prefix+item[ref]
    return project


if __name__ == "__main__":
    checks = []
    r = result(base())
    rows = r["chemistry"]["a"]
    errors = [abs(row["A"]-2*math.exp(-.2*(row["time"]-.1))) for row in rows]
    assert max(errors) < 2e-7, max(errors)
    checks.append({"test": "A -> B analytical decay", "max_sample_error_nM": max(errors)})
    reversible = base(); reversible["reactions"][0]["reverse_rate"] = .1
    reverse_result = result(reversible)
    reverse_error = max(abs(row["A"]-(2/3+4/3*math.exp(-.3*(row["time"]-.1)))) for row in reverse_result["chemistry"]["a"])
    assert reverse_error < 2e-7
    checks.append({"test": "reversible A <-> B", "max_error_nM": reverse_error})
    repeated = base(); repeated["reactions"][0]["reactants"] = ["A", "A"]
    repeated_result = result(repeated)
    repeated_error = max(abs(row["A"]-2/(1+.8*(row["time"]-.1))) for row in repeated_result["chemistry"]["a"])
    assert repeated_error < 2e-7
    checks.append({"test": "stoichiometry 2 A -> B", "max_error_nM": repeated_error})
    # Confirm a one-species constant trajectory is not skipped.
    constant = base(); constant["species"] = constant["species"][:1]; constant["reactions"] = []
    c = result(constant)
    assert all(row["A"] == 2 for row in c["chemistry"]["a"])
    checks.append({"test": "one species / zero reactions", "passed": True})
    left, right = fork_network("L"), fork_network("R")
    for n in right["nodes"]: n["y"] += .04
    for key in ("nodes", "channels", "pumps", "injections"): left[key] += right[key]
    groups = result(left)
    merges = [e for e in groups["events"] if e["type"] == "merge"]
    assert {frozenset(e["parents"]) for e in merges} == {frozenset(("Ld0", "Ld1")), frozenset(("Rd0", "Rd1"))}
    checks.append({"test": "two independent simultaneous groups", "merges": merges})
    triple = result(fork_network("T", 3))
    merges = [e for e in triple["events"] if e["type"] == "merge"]
    assert len(merges) == 2
    final_id = merges[-1]["droplet"]
    assert abs(triple["chemistry"][final_id][0]["A"]-2) < 1e-10
    checks.append({"test": "triple collision binary cascade", "merges": merges})
    unequal = fork_network("U")
    unequal["injections"][1]["volume"] = 4.5e-13
    unequal_result = result(unequal)
    event = next(e for e in unequal_result["events"] if e["type"] == "merge")
    assert abs(unequal_result["chemistry"][event["droplet"]][0]["A"]-5/3) < 1e-10
    checks.append({"test": "unequal volumes conserve amount", "concentration_nM": 5/3})
    unequal["settings"]["mixing"] = "legacy_sum"
    legacy = result(unequal)
    event = next(e for e in legacy["events"] if e["type"] == "merge")
    assert abs(legacy["chemistry"][event["droplet"]][0]["A"]-3) < 1e-10
    checks.append({"test": "explicit legacy comparison", "concentration_nM": 3})
    # Numerical mesh refinement: evaluate interpolation against the analytic oracle.
    def interpolation_error(count):
        p = base(); p["settings"]["sample_count"] = count
        r = result(p); rows = r["chemistry"]["a"]
        errors = []
        for a, b in zip(rows, rows[1:]):
            t = (a["time"]+b["time"])/2
            errors.append(abs((a["A"]+b["A"])/2-2*math.exp(-.2*(t-.1))))
        return max(errors)
    coarse, fine = interpolation_error(501), interpolation_error(2001)
    assert fine < coarse/5 and fine < 0.0001, (coarse, fine)
    checks.append({"test": "chemical interpolation refinement", "error_501_nM": coarse, "error_2001_nM": fine})
    target = Path(__file__).resolve().parents[1] / ".runs/physics-validation.json"
    atomic_json(target, checks)
    print(checks)
