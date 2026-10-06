"""Real local API queue/cancellation checks; touches only jobs created here."""
from pathlib import Path
import subprocess
import sys
import time
from urllib.error import HTTPError

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from tests.integration import request
from backend.engine import atomic_json


def wait_terminal(jid):
    deadline = time.monotonic() + 90
    while time.monotonic() < deadline:
        job = request(f"runs/{jid}")
        if job["status"] not in ("queued", "running"):
            return job
        time.sleep(.1)
    raise AssertionError("Job did not terminate")


if __name__ == "__main__":
    project = request("examples/Medoids.json")
    project["name"] = "Cancellation validation"
    running = request("runs", project)["id"]
    queued = request("runs", project)["id"]
    assert request(f"runs/{queued}")["status"] == "queued"
    assert request(f"runs/{queued}/cancel", {})["status"] == "cancelled"
    assert request(f"runs/{running}")["status"] == "running"
    assert request(f"runs/{running}/cancel", {})["status"] == "cancelled"
    checks = []
    for jid in (running, queued):
        assert wait_terminal(jid)["status"] == "cancelled"
        try:
            request(f"runs/{jid}/result")
        except HTTPError as exc:
            assert exc.code == 409
        else:
            raise AssertionError("Cancelled result was published")
        containers = subprocess.check_output(
            ["docker", "ps", "-a", "--filter", f"name=fluidna-{jid}", "--format", "{{.Names}}"], text=True)
        assert not containers.strip(), containers
        checks.append({"id": jid, "status": "cancelled", "result_http": 409, "orphan_container": False})
    # The worker queue must remain usable after cancellation, and cannot reuse output.
    project["name"] = "After cancellation validation"
    success = request("runs", project)["id"]
    assert wait_terminal(success)["status"] == "completed"
    result = request(f"runs/{success}/result")
    assert result["project"]["name"] == project["name"]
    assert sum(e["type"] == "merge" for e in result["events"]) == 8
    assert all(request(f"runs/{jid}")["status"] == "cancelled" for jid in (running, queued))
    checks.append({"id": success, "status": "completed", "merges": 8})
    atomic_json(Path(__file__).resolve().parents[1] / ".runs/jobs-validation.json", checks)
    print(checks)
