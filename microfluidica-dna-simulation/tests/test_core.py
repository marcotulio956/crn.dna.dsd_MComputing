from copy import deepcopy
from pathlib import Path
import json
import math
import random

import pytest
from backend.models import Project, import_project
from backend.merges import reconstruct, ReconstructionError
from backend.chemistry import mix
from backend.engine import read_json

ROOT = Path(__file__).resolve().parents[1]


def b(c, p, toward):
    return {"position": {"channel": c, "position": p}, "volumeTowards1": toward}


def drop(i, c, start, end, volume=1e-13):
    return {"id": i, "fluid": 1, "volume": volume, "channels": [], "boundaries": [b(c, start, False), b(c, end, True)]}


def raw_sequence(initial, *snapshots):
    empty = [{**d, "boundaries": [], "channels": []} for d in initial]
    return {"network": [{"time": 0, "bigDroplets": empty}, {"time": .1, "bigDroplets": deepcopy(initial)},
                        *[{"time": t, "bigDroplets": deepcopy(d)} for t, d in snapshots]]}


def manifest(initial):
    return {d["id"]: {"time": .1, "volume": d["volume"]} for d in initial}


def test_two_simultaneous_independent_merges():
    initial = [drop(0, 0, .1, .3), drop(1, 0, .3, .5), drop(2, 1, .2, .4), drop(3, 1, .4, .6)]
    children = [drop(4, 0, .1, .5, 2e-13), drop(5, 1, .2, .6, 2e-13)]
    result = reconstruct(raw_sequence(initial, (1, initial+children)), {0: (0, 1), 1: (2, 3)}, manifest(initial))
    merges = [e for e in result["events"] if e["type"] == "merge"]
    assert {(e["droplet"], tuple(e["parents"])) for e in merges} == {(4, (0, 1)), (5, (2, 3))}


@pytest.mark.parametrize("dt", [0, 1e-12])
def test_three_droplet_cascade_preserves_binary_order_and_time(dt):
    initial = [drop(0, 0, .1, .3), drop(1, 0, .3, .5), drop(2, 0, .5, .7)]
    child = drop(3, 0, .1, .5, 2e-13)
    grandchild = drop(4, 0, .1, .7, 3e-13)
    raw = raw_sequence(initial, (1, initial+[child]), (1+dt, initial+[child, grandchild]))
    result = reconstruct(raw, {0: (0, 1)}, manifest(initial))
    merges = [e for e in result["events"] if e["type"] == "merge"]
    assert [e["parents"] for e in merges] == [[0, 1], [2, 3]]
    assert merges[1]["state"] > merges[0]["state"]
    assert merges[1]["time"]-merges[0]["time"] == (1+dt)-1
    assert result["lifecycle"][3]["death_time"] == 1+dt


def test_junction_merge_uses_all_boundaries_and_topology():
    a = drop(0, 0, .7, 1)
    other = {"id": 1, "fluid": 1, "volume": 1e-13, "channels": [], "boundaries": [b(1, .7, False), b(2, 0, True)]}
    child = {"id": 2, "fluid": 1, "volume": 2e-13, "channels": [], "boundaries": [b(0, .7, False), *other["boundaries"]]}
    initial = [a, other]
    for seed in range(10):
        raw = raw_sequence(initial, (1, initial+[child]))
        random.Random(seed).shuffle(raw["network"][-1]["bigDroplets"])
        for d in raw["network"][-1]["bigDroplets"]:
            random.Random(seed).shuffle(d["boundaries"])
        result = reconstruct(raw, {0: (0, 2), 1: (1, 2), 2: (2, 3)}, manifest(initial))
        assert result["lifecycle"][2]["parents"] == [0, 1]
        assert result["events"][-1]["evidence"][0]["kind"] == "junction"


def test_ambiguity_is_not_guessed():
    a, b1, b2 = drop(0, 0, .1, .3), drop(1, 0, .3, .5), drop(2, 0, .3, .5)
    initial = [a, b1, b2]
    child = drop(3, 0, .1, .5, 2e-13)
    with pytest.raises(ReconstructionError, match="ambiguous"):
        reconstruct(raw_sequence(initial, (1, initial+[child])), {0: (0, 1)}, manifest(initial))


def test_wrong_volume_blocks_chemistry():
    initial = [drop(0, 0, .1, .3), drop(1, 0, .3, .5)]
    with pytest.raises(ReconstructionError, match="unexplained"):
        reconstruct(raw_sequence(initial, (1, initial+[drop(2, 0, .1, .5, 3e-13)])), {0: (0, 1)}, manifest(initial))


def test_no_merge_for_stopped_or_exited_droplet():
    initial = [drop(0, 0, .1, .3)]
    stopped = reconstruct(raw_sequence(initial, (1, initial)), {0: (0, 1)}, manifest(initial), {1})
    assert stopped["lifecycle"][0]["terminal"] == "stalled"
    at_sink = [drop(0, 0, .8, 1)]
    result = reconstruct(raw_sequence(initial, (1, at_sink), (2, at_sink)), {0: (0, 1)}, manifest(initial), {1})
    assert result["lifecycle"][0]["terminal"] == "sink"
    assert result["lifecycle"][0]["death_time"] == 1


def test_close_simultaneous_sink_arrivals_not_coalesced():
    initial = [drop(0, 0, .1, .3)]
    near = [drop(0, 0, .8-2e-15, 1-2e-15)]
    final = [drop(0, 0, .8, 1)]
    result = reconstruct(raw_sequence(initial, (10, near), (10+1e-12, final)), {0: (0, 1)}, manifest(initial), {1})
    assert result["lifecycle"][0]["death"] == 3
    assert result["lifecycle"][0]["death_time"] == 10+1e-12


@pytest.mark.parametrize("corruption", ["duplicate", "disappeared", "nan", "backwards", "unknown_channel", "orientation"])
def test_corrupt_raw_rejected(corruption):
    initial = [drop(0, 0, .1, .3)]
    raw = raw_sequence(initial, (1, initial))
    state = raw["network"][-1]
    if corruption == "duplicate": state["bigDroplets"] *= 2
    if corruption == "disappeared": state["bigDroplets"] = []
    if corruption == "nan": state["time"] = float('nan')
    if corruption == "backwards": state["time"] = -.1
    if corruption == "unknown_channel": state["bigDroplets"][0]["channels"] = [88]
    if corruption == "orientation": del state["bigDroplets"][0]["boundaries"][0]["volumeTowards1"]
    with pytest.raises(ReconstructionError): reconstruct(raw, {0: (0, 1)}, manifest(initial))


def test_strict_json_reader(tmp_path):
    for content in ('{"a": NaN}', '{"a":1,"a":2}', '{"network":['):
        path = tmp_path / "invalid.json"
        path.write_text(content)
        with pytest.raises(ValueError): read_json(path)


def test_physical_mixing_conserves_amount_unequal_volumes():
    concentrations = [{"A": 2, "B": 1}, {"A": 4, "C": 3}]
    volumes = [1e-13, 3e-13]
    output = mix(concentrations, volumes)
    assert output == pytest.approx({"A": 3.5, "B": .25, "C": 2.25})
    for key in output:
        assert math.isclose(output[key]*sum(volumes), sum(c.get(key, 0)*v for c, v in zip(concentrations, volumes)))
    assert mix(concentrations, volumes, "legacy_sum")["A"] == 6


@pytest.mark.parametrize("path", sorted((ROOT / "examples").glob("*.json")), ids=lambda p:p.name)
def test_legacy_import_roundtrip(path):
    raw = read_json(path)
    project = import_project(raw)
    assert Project.model_validate_json(project.model_dump_json()) == project
    assert project.provenance["original"] == raw
    assert all(d.volume == 2.25e-13 for d in project.injections)
    assert len(project.reactions) == len(raw["reactionData"])
    assert len(project.species) == len(raw["speciesData"])
    assert project.channels[0].height == 30e-6
    assert project.channels[0].width == float(raw["edges"][0]["height"])*1e-5
    project.simulation_ready()


def test_unknown_species_rejected():
    project = import_project(read_json(ROOT / "examples/Simple Sum Circuit.json")).model_dump()
    project["reactions"][0]["reactants"] = ["unknown"]
    with pytest.raises(ValueError, match="unknown species"): Project.model_validate(project)


@pytest.mark.parametrize("target", ["time", "merge:", "canvas"])
def test_reserved_and_colliding_ids_rejected(target):
    project = import_project(read_json(ROOT / "examples/Simple Sum Circuit.json")).model_dump()
    if target == "time": project["species"][0]["id"] = "time"
    if target == "merge:": project["injections"][0]["id"] = "merge:9"
    if target == "canvas": project["pumps"][0]["id"] = project["nodes"][0]["id"]
    with pytest.raises(ValueError): Project.model_validate(project)
