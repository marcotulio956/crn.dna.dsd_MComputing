"""Only public APIs of the pinned, unmodified MMFT wheel are used here."""
from importlib.metadata import version
from pathlib import Path
import hashlib
import json
import os

from .merges import ENGINE, reconstruct


def read_json(path):
    def invalid(value):
        raise ValueError(f"Non-finite JSON constant: {value}")
    def unique(pairs):
        result = {}
        for k, v in pairs:
            if k in result:
                raise ValueError(f"Duplicate JSON key: {k}")
            result[k] = v
        return result
    return json.loads(Path(path).read_text(encoding="utf-8"), parse_constant=invalid, object_pairs_hook=unique)


def atomic_json(path, value):
    path = Path(path)
    tmp = path.with_suffix(path.suffix + ".tmp")
    with tmp.open("w", encoding="utf-8") as stream:
        json.dump(value, stream, ensure_ascii=False, allow_nan=False)
        stream.flush()
        os.fsync(stream.fileno())
    os.replace(tmp, path)


def run_mmft(project, folder):
    installed = version("mmft-simulator")
    if installed != ENGINE:
        raise RuntimeError(f"Unsupported MMFT {installed}; reconstruction requires {ENGINE}")
    from mmft.simulator import Network, ChannelType
    from mmft.simulator.pysimulator import AbstractDropletSimulation
    network = Network()
    nodes, channels, injections = {}, {}, {}
    for n in project.nodes:
        # The 4-argument binding always sets sink, even if passed False.
        nodes[n.id] = network.addNode(n.x, n.y, n.ground, True) if n.sink else network.addNode(n.x, n.y, n.ground)
    for c in project.channels:
        channels[c.id] = network.addChannel(nodes[c.source], nodes[c.target], c.width, c.height, ChannelType.normal)
    for p in project.pumps:
        fn = network.addFlowRatePump if p.kind == "flow" else network.addPressurePump
        fn(nodes[p.source], nodes[p.target], p.value)
    network.sort()
    network.valid()
    sim = AbstractDropletSimulation(network)
    carrier = sim.addFluid(project.settings.continuous.density, project.settings.continuous.viscosity, 1.0)
    dispersed = sim.addFluid(project.settings.dispersed.density, project.settings.dispersed.viscosity, 1.0)
    sim.setContinuousPhase(carrier)
    sim.setRectangularResistanceModel()
    for d in project.injections:
        did = sim.addDroplet(dispersed, d.volume)
        injections[did] = d.model_dump()
        sim.injectDroplet(did, d.time, channels[d.channel], d.position)
    # Unique run directory and successful return are both required. No shared JSON.
    sim.simulate()
    temporary = folder / "mmft.pending.json"
    sim.saveResult(str(temporary))
    raw = read_json(temporary)
    topology = {channels[c.id]: (nodes[c.source], nodes[c.target]) for c in project.channels}
    genealogy = reconstruct(raw, topology, injections, {nodes[n.id] for n in project.nodes if n.sink})
    os.replace(temporary, folder / "mmft.json")
    manifest = {"engine": installed, "nodes": nodes, "channels": channels, "injections": injections,
                "sha256": hashlib.sha256((folder / "mmft.json").read_bytes()).hexdigest()}
    atomic_json(folder / "manifest.json", manifest)
    return raw, genealogy, manifest
