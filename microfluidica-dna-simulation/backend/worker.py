import sys
import hashlib
from pathlib import Path
from .models import Project
from .engine import read_json, atomic_json, run_mmft
from .chemistry import run_chemistry


def run(folder):
    project = Project.model_validate(read_json(folder / "project.json"))
    project.simulation_ready()
    raw, genealogy, manifest = run_mmft(project, folder)
    chemistry = run_chemistry(project, raw, genealogy, manifest, folder)
    names = {int(k): v["id"] for k, v in manifest["injections"].items()}
    for d in genealogy["lifecycle"]:
        if d not in names:
            names[d] = f"merge:{d}"
    channels = {v: k for k, v in manifest["channels"].items()}
    lives = {names[k]: {**v, "parents": [names[p] for p in v["parents"]]} for k, v in genealogy["lifecycle"].items()}
    states = []
    for index, state in enumerate(raw["network"]):
        # Keep consumed parents' terminal geometry available for interpolation;
        # frontend visibility uses lifecycle, never presence in this dictionary.
        states.append({"time": state["time"], "ordinal": index,
                       "droplets": {names[d["id"]]: {"volume": d["volume"], "boundaries": [
                           {"channel": channels[b["position"]["channel"]], "position": b["position"]["position"],
                            "towardSource": b["volumeTowards1"]} for b in d["boundaries"]],
                           "channels": [channels[c] for c in d["channels"]]} for d in state["bigDroplets"]},
                       "flows": {channels[c]: state["channels"][c]["flowRate"] for c in channels},
                       "pressures": {n: state["nodes"][i]["pressure"] for n, i in manifest["nodes"].items()}})
    events = [{**e, "droplet": names[e["droplet"]], "parents": [names[p] for p in e.get("parents", [])]} for e in genealogy["events"]]
    result = {"schema_version": 2, "project": project.model_dump(), "states": states, "events": events,
              "lifecycle": lives, "chemistry": {names[int(k)]: v for k, v in chemistry.items()},
              "provenance": {"mmft": manifest, "detector": "inverse-events-v1", "tolerances": genealogy["tolerances"],
                             "mixing": project.settings.mixing, "sampling": "linear-plus-log-relative-v1",
                             "dnar_commit": "717b07068230935cc82503295e3741126f880a08",
                             "units": {"concentration": "nM", "time": "s", "volume": "m3"},
                             "project_sha256": hashlib.sha256((folder / "project.json").read_bytes()).hexdigest()},
              "warnings": genealogy["warnings"]}
    atomic_json(folder / "result.json", result)


if __name__ == "__main__":
    run(Path(sys.argv[1]).resolve())
