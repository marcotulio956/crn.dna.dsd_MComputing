from __future__ import annotations

from typing import Literal
from pydantic import BaseModel, ConfigDict, Field, model_validator
import math
import re


class Model(BaseModel):
    model_config = ConfigDict(extra="forbid", allow_inf_nan=False)


class Node(Model):
    id: str
    name: str
    x: float
    y: float
    sink: bool = False
    ground: bool = False


class Channel(Model):
    id: str
    source: str
    target: str
    width: float = Field(default=50e-6, gt=0)
    height: float = Field(default=30e-6, gt=0)


class Pump(Model):
    id: str
    source: str
    target: str
    kind: Literal["flow", "pressure"] = "flow"
    value: float = 1.5e-16


class Species(Model):
    id: str
    name: str
    color: str = "#2dd4bf"
    visible: bool = True


class Reaction(Model):
    id: str
    reactants: list[str]
    products: list[str]
    rate: float = Field(default=0.0028, ge=0)
    reverse_rate: float | None = Field(default=None, ge=0)


class Injection(Model):
    id: str
    name: str
    channel: str
    time: float = Field(default=0.1, ge=0)
    position: float = Field(default=0.25, gt=0, lt=1)
    volume: float = Field(default=2.25e-13, gt=0)
    concentrations: dict[str, float] = Field(default_factory=dict)


class Fluid(Model):
    density: float = Field(default=1000, gt=0)
    viscosity: float = Field(default=0.001, gt=0)


class Settings(Model):
    mixing: Literal["physical", "legacy_sum"] = "physical"
    continuous: Fluid = Field(default_factory=Fluid)
    dispersed: Fluid = Field(default_factory=lambda: Fluid(viscosity=0.003))
    sample_count: int = Field(default=501, ge=2, le=20001)
    rtol: float = Field(default=1e-8, gt=0, le=0.01)
    atol: float = Field(default=1e-10, gt=0, le=0.01)
    timeout: int = Field(default=180, ge=5, le=3600)
    engine: Literal["desolve", "diffeqr"] = "desolve"
    stochastic: bool = False
    dna: bool = False
    volume: float = Field(default=10, gt=0)
    seed: int | None = None
    forcing: dict | None = None
    timing_enabled: bool = False
    timing_tolerance_percent: float = Field(default=1, ge=0)
    timing_stable_samples: int = Field(default=3, ge=1)
    timing_step: float = Field(default=0.1, gt=0)
    timing_max_time: float = Field(default=100, gt=0)

    @model_validator(mode="after")
    def simulation_constraints(self):
        if self.stochastic and self.engine == "diffeqr":
            raise ValueError("stochastic simulation does not use the diffeqr engine")
        if self.forcing is not None:
            allowed = {"saw_wave_input", "sinusoidal_input", "step_input", "pulse_input", "square_input"}
            name = self.forcing.get("name") or self.forcing.get("function")
            if name not in allowed:
                raise ValueError("Unsupported forcing function")
            if not isinstance(self.forcing.get("species"), str) or not self.forcing["species"]:
                raise ValueError("Forcing requires a target species id")
        if self.timing_max_time < self.timing_step:
            raise ValueError("timing_max_time must be at least timing_step")
        return self


class Annotation(Model):
    id: str
    x: float
    y: float
    text: str


class Project(Model):
    schema_version: Literal[2] = 2
    name: str = "Untitled"
    nodes: list[Node]
    channels: list[Channel]
    pumps: list[Pump] = Field(default_factory=list)
    species: list[Species] = Field(default_factory=list)
    reactions: list[Reaction] = Field(default_factory=list)
    injections: list[Injection] = Field(default_factory=list)
    annotations: list[Annotation] = Field(default_factory=list)
    settings: Settings = Field(default_factory=Settings)
    provenance: dict = Field(default_factory=dict)

    @model_validator(mode="after")
    def references(self):
        for field in ("nodes", "channels", "pumps", "species", "reactions", "injections", "annotations"):
            items = getattr(self, field)
            ids = [v.id for v in items]
            if len(ids) != len(set(ids)) or any(not i for i in ids):
                raise ValueError(f"{field}: duplicate or empty IDs")
        nodes = {n.id: n for n in self.nodes}
        channels = {c.id: c for c in self.channels}
        species = {s.id for s in self.species}
        canvas_ids = [v.id for collection in (self.nodes, self.channels, self.pumps, self.injections, self.annotations) for v in collection]
        if len(canvas_ids) != len(set(canvas_ids)):
            raise ValueError("Canvas object IDs must be unique across nodes, channels, pumps, injections and annotations")
        if "time" in species:
            raise ValueError("Species ID 'time' is reserved for the chemical time axis")
        if any(d.id.startswith("merge:") for d in self.injections):
            raise ValueError("Injection ID prefix 'merge:' is reserved for reconstructed descendants")
        names = [s.name for s in self.species]
        if len(names) != len(set(names)) or any(not re.fullmatch(r"[A-Za-z][A-Za-z0-9_]*", n) for n in names):
            raise ValueError("Species names must be unique DNAr identifiers (letters, digits, underscore)")
        for c in [*self.channels, *self.pumps]:
            if c.source not in nodes or c.target not in nodes or c.source == c.target:
                raise ValueError(f"{c.id}: invalid endpoints")
        for c in self.channels:
            a, b = nodes[c.source], nodes[c.target]
            if math.hypot(a.x-b.x, a.y-b.y) == 0:
                raise ValueError(f"{c.id}: zero-length channel")
        for r in self.reactions:
            if not r.reactants or not r.products:
                raise ValueError(f"{r.id}: nonempty reactants and products required")
            if not set(r.reactants + r.products) <= species:
                raise ValueError(f"{r.id}: unknown species")
        for d in self.injections:
            if d.channel not in channels or not set(d.concentrations) <= species:
                raise ValueError(f"{d.id}: unknown channel or species")
            if any(not math.isfinite(v) or v < 0 for v in d.concentrations.values()):
                raise ValueError(f"{d.id}: concentrations must be finite and nonnegative nM")
            c = channels[d.channel]
            a, b = nodes[c.source], nodes[c.target]
            fraction = d.volume / (math.hypot(a.x-b.x, a.y-b.y)*c.width*c.height)
            if fraction / 2 > min(d.position, 1-d.position):
                raise ValueError(f"{d.id}: droplet does not fit around its injection position")
        for i, a in enumerate(self.injections):
            for b in self.injections[i+1:]:
                if a.channel != b.channel or a.time != b.time:
                    continue
                c = channels[a.channel]
                n1, n2 = nodes[c.source], nodes[c.target]
                channel_volume = math.hypot(n1.x-n2.x, n1.y-n2.y)*c.width*c.height
                if abs(a.position-b.position) < (a.volume+b.volume)/(2*channel_volume)-1e-12:
                    raise ValueError(f"{a.id}, {b.id}: overlapping simultaneous injections")
        return self

    def simulation_ready(self):
        if not self.channels or not self.injections or not self.pumps or not any(n.sink for n in self.nodes):
            raise ValueError("Simulation requires channels, injections, a pump, and a sink")


def import_project(raw: dict) -> Project:
    if raw.get("schema_version") == 2:
        return Project.model_validate(raw)
    if not all(k in raw for k in ("nodes", "edges", "tableData", "speciesData", "reactionData")):
        raise ValueError("Not a fluiDNA legacy or v2 project")
    nodes = [Node(id=f"n{i}", name=n["name"], x=float(n["x"])*1e-5,
                  y=float(n["y"])*1e-5, sink=bool(n.get("isSink")), ground=bool(n.get("isSink")))
             for i, n in enumerate(raw["nodes"])]
    byname = {n.name: n.id for n in nodes}
    if len(byname) != len(nodes):
        raise ValueError("Legacy node names must be unique")
    channels = [Channel(id=f"c{i}", source=byname[e["from"]["name"]], target=byname[e["to"]["name"]],
                        width=float(e["height"])*1e-5, height=30e-6) for i, e in enumerate(raw["edges"])]
    colors = ["#2dd4bf", "#a78bfa", "#fb923c", "#38bdf8", "#fb7185", "#a3e635"]
    species = [Species(id=f"s{s['id']}", name=s["name"], visible=s.get("showInResults", True), color=colors[i % len(colors)])
               for i, s in enumerate(raw["speciesData"])]
    sid = {s.name: s.id for s in species}
    reactions = [Reaction(id=f"r{r['id']}", reactants=[sid[s] for s in r["reactants"]],
                          products=[sid[s] for s in r["products"]], rate=float(r["rates"])) for r in raw["reactionData"]]
    injections, pumps = [], []
    sink = next((n.id for n in nodes if n.sink), None)
    for d in raw["tableData"]:
        inlet = byname[d["pump"]]
        channel = next((c.id for c in channels if c.source == inlet), None)
        if channel is None or sink is None:
            raise ValueError(f"Legacy droplet {d['name']}: missing outgoing channel or sink")
        values = d.get("concentration", [])
        def concentration(key):
            v = values[key] if isinstance(values, list) and key < len(values) else values.get(str(key)) if isinstance(values, dict) else None
            return float(v or 0)
        injections.append(Injection(id=f"d{d['id']}", name=d["name"], channel=channel,
                                    concentrations={f"s{s['id']}": concentration(int(s['id'])) for s in raw["speciesData"]}))
        # Preserve the effective legacy circuit, including duplicate pumps, with an explicit warning.
        pumps.append(Pump(id=f"p{d['id']}", source=sink, target=inlet))
    return Project(name=raw.get("name", "Imported circuit"), nodes=nodes, channels=channels, pumps=pumps,
                   species=species, reactions=reactions, injections=injections,
                   provenance={"profile": "legacy-deployment-03dcb74", "original": raw, "warnings": [
                       "Legacy volume fields were ignored by the old backend; effective 0.225 nL retained. Original values preserved.",
                       "Legacy edge height mapped to effective WIDTH; physical height is 30 µm. Coordinates use 10 µm/unit.",
                       "One pump per legacy droplet preserved. Review repeated pumps before editing.",
                       "Concentrations interpreted as nM; rate constants use nM^(1-order)/s. Physical dilution is now default."]})
