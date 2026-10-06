"""Run all six user circuits against the live v2 API and its real MMFT/DNAr worker.
Usage: .venv/Scripts/python.exe tests/integration.py
Writes a reproducible, local validation report (no network beyond localhost).
"""
import json
import time
from urllib.request import Request, urlopen
from pathlib import Path
import sys
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from backend.engine import atomic_json


def request(path, body=None):
    req = Request("http://127.0.0.1:8000/api/v2/"+path, data=None if body is None else json.dumps(body).encode(),
                  headers={"Content-Type": "application/json"})
    with urlopen(req, timeout=30) as response:
        return json.load(response)


if __name__ == "__main__":
    root = Path(__file__).resolve().parents[1]
    jobs = []
    for path in sorted((root / "examples").glob("*.json")):
        project = request("projects/import", json.loads(path.read_text(encoding="utf-8")))
        project["name"] = path.stem
        job = request("runs", project)
        jobs.append({"example": path.name, "id": job["id"]})
    deadline = time.monotonic()+1200
    while time.monotonic() < deadline:
        complete = True
        for entry in jobs:
            job = request(f"runs/{entry['id']}")
            entry.update(status=job["status"], error=job["error"])
            if job["status"] in ("queued", "running"):
                complete = False
            elif job["status"] == "completed" and "states" not in entry:
                result = request(f"runs/{entry['id']}/result")
                entry.update(states=len(result["states"]), merges=sum(e["type"] == "merge" for e in result["events"]),
                             droplets=len(result["lifecycle"]), duration=result["states"][-1]["time"], warnings=result["warnings"])
                for event in result["events"]:
                    if event["type"] != "merge":
                        continue
                    child = event["droplet"]
                    child_row = result["chemistry"][child][0]
                    state = result["states"][event["state"]]
                    cv = state["droplets"][child]["volume"]
                    for species in result["project"]["species"]:
                        sid = species["id"]
                        before = sum(result["chemistry"][p][-1][sid]*state["droplets"][p]["volume"] for p in event["parents"])
                        after = child_row[sid]*cv
                        if abs(before-after) > max(1e-25, abs(before)*1e-8):
                            raise AssertionError(f"Mass conservation failed {entry['example']} / {child} / {sid}")
                print(json.dumps(entry, ensure_ascii=True), flush=True)
        if complete:
            break
        time.sleep(1)
    report = root / ".runs/validation.json"
    atomic_json(report, jobs)
    print(json.dumps(jobs, indent=2, ensure_ascii=True))
    sys.exit(0 if all(j["status"] == "completed" for j in jobs) else 1)
