from pathlib import Path
import subprocess
import math
from .engine import atomic_json, read_json


def sample_offsets(duration, count):
    """Blend linear coverage with log-spaced early-time samples (seconds).

    This is an output mesh, not a substitute for DNAr/deSolve adaptive integration.
    Refinement is configurable; the UI explicitly labels interpolation.
    """
    if duration == 0:
        return [0.0]
    if count < 4:
        return [duration*i/(count-1) for i in range(count)]
    linear_count = count//2
    log_count = count-linear_count+2
    tau = min(1e-6, duration)
    linear = [duration*i/(linear_count-1) for i in range(linear_count)]
    logarithmic = [tau*math.expm1(math.log1p(duration/tau)*i/(log_count-1)) for i in range(log_count)]
    # Explicit endpoints avoid cancellation/roundoff from expm1(log1p()).
    logarithmic[0], logarithmic[-1] = 0.0, duration
    return sorted(set(linear+logarithmic))


def mix(concentrations, volumes, mode="physical"):
    if len(concentrations) != len(volumes) or not volumes or any(v <= 0 for v in volumes):
        raise ValueError("Invalid mixing inputs")
    keys = set().union(*(c.keys() for c in concentrations))
    if mode == "legacy_sum":
        return {k: sum(c.get(k, 0) for c in concentrations) for k in keys}
    if mode != "physical":
        raise ValueError("Unknown mixing model")
    return {k: sum(c.get(k, 0)*v for c, v in zip(concentrations, volumes))/sum(volumes) for k in keys}


def run_chemistry(project, raw, genealogy, manifest, folder):
    initial = {int(k): v for k, v in manifest["injections"].items()}
    volumes = {d["id"]: d["volume"] for d in raw["network"][-1]["bigDroplets"]}
    lives = genealogy["lifecycle"]
    # Binary cascades are already topologically ordered, even at identical times.
    order = [e["droplet"] for e in genealogy["events"] if e["type"] in ("injection", "merge")]
    payload = {"species": [s.model_dump() for s in project.species],
               "reactions": [r.model_dump() for r in project.reactions],
               "settings": project.settings.model_dump(),
               "droplets": [{"id": str(d), "start": lives[d]["birth_time"], "end": lives[d]["death_time"],
                             "parents": [str(p) for p in lives[d]["parents"]], "volume": volumes[d],
                             "offsets": sample_offsets(lives[d]["death_time"]-lives[d]["birth_time"], project.settings.sample_count),
                             "concentrations": initial.get(d, {}).get("concentrations", {})} for d in order]}
    atomic_json(folder / "chemistry-input.json", payload)
    script = Path(__file__).with_name("react.R")
    with (folder / "dnar.log").open("w", encoding="utf-8") as log:
        subprocess.run(["Rscript", str(script), str(folder / "chemistry-input.json"), str(folder / "chemistry.json")],
                       check=True, stdout=log, stderr=subprocess.STDOUT, timeout=project.settings.timeout)
    data = read_json(folder / "chemistry.json")
    if set(data) != {str(d) for d in order}:
        raise ValueError("DNAr output missing droplet trajectories")
    for did, rows in data.items():
        if not rows or any(set(row) != {"time", *(s.id for s in project.species)} for row in rows):
            raise ValueError(f"DNAr output schema mismatch for {did}")
        for row in rows:
            for key, value in row.items():
                if key == "time":
                    continue
                if not math.isfinite(value):
                    raise ValueError("DNAr returned invalid concentrations")
                if value < 0:
                    row[key] = 0.0
        life = lives[int(did)]
        if not math.isclose(rows[0]["time"], life["birth_time"], rel_tol=1e-14, abs_tol=1e-14) or not math.isclose(rows[-1]["time"], life["death_time"], rel_tol=1e-14, abs_tol=1e-14):
            raise ValueError("DNAr output interval mismatch")
        # react.R uses the supplied RELATIVE schedule. jsonlite loses a few ULPs
        # when serializing large absolute times. Validate and restore the exact
        # schedule here, including sub-ULP duplicate samples, never coalescing
        # different MMFT events. The integration itself retains positive dt.
        start, end = life["birth_time"], life["death_time"]
        offsets = sample_offsets(end-start, project.settings.sample_count) if project.reactions and project.species else sorted(set([0.0, end-start]))
        if len(rows) != len(offsets):
            raise ValueError("DNAr output sample count mismatch")
        for row, offset in zip(rows, offsets):
            expected = start + offset
            if not math.isclose(row["time"], expected, rel_tol=1e-14, abs_tol=1e-14):
                raise ValueError("DNAr output sample time mismatch")
            row["time"] = expected
        rows[-1]["time"] = end
    return data
