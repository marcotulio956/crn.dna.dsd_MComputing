"""Validation and physical helpers for the FluidNar simulation contract."""

from __future__ import annotations

from copy import deepcopy
from math import isfinite
from typing import Any, Dict, Iterable, Mapping, Sequence

DEFAULT_DROPLET_VOLUME_M3 = 2.25e-13
ALLOWED_FORCING = {
    "saw_wave_input",
    "sinusoidal_input",
    "step_input",
    "pulse_input",
    "square_input",
}


class SimulationConfigError(ValueError):
    """Raised when a FluidNar simulation request is invalid."""


def _number(value: Any, field: str, *, minimum: float | None = None) -> float:
    try:
        number = float(value)
    except (TypeError, ValueError) as exc:
        raise SimulationConfigError(f"{field} must be numeric") from exc
    if not isfinite(number):
        raise SimulationConfigError(f"{field} must be finite")
    if minimum is not None and number < minimum:
        raise SimulationConfigError(f"{field} must be at least {minimum}")
    return number


def validate_forcing(forcing: Mapping[str, Any] | None) -> Dict[str, Any] | None:
    if forcing in (None, {}, {"enabled": False}):
        return None
    if not isinstance(forcing, Mapping):
        raise SimulationConfigError("forcing must be an object")

    name = forcing.get("name") or forcing.get("function")
    if name not in ALLOWED_FORCING:
        allowed = ", ".join(sorted(ALLOWED_FORCING))
        raise SimulationConfigError(f"forcing function must be one of: {allowed}")

    params = forcing.get("params", {})
    if not isinstance(params, Mapping):
        raise SimulationConfigError("forcing.params must be an object")

    species = forcing.get("species")
    if not isinstance(species, str) or not species.strip():
        raise SimulationConfigError("forcing.species must be a non-empty string")
    normalized = {"name": name, "species": species.strip(), "params": dict(params)}
    for key, value in normalized["params"].items():
        normalized["params"][key] = _number(value, f"forcing.params.{key}")
    return normalized


def normalize_request(data: Mapping[str, Any]) -> Dict[str, Any]:
    if not isinstance(data, Mapping):
        raise SimulationConfigError("request body must be an object")

    normalized = deepcopy(dict(data))
    droplets = normalized.get("goticulasIniciais")
    if not isinstance(droplets, list) or not droplets:
        raise SimulationConfigError("goticulasIniciais must contain at least one droplet")

    simulation = dict(normalized.get("simulation") or {})
    simulation.setdefault("engine", "desolve")
    simulation.setdefault("dna", False)
    simulation.setdefault("stochastic", False)
    simulation["engine"] = str(simulation["engine"])
    if simulation["engine"] not in {"desolve", "diffeqr"}:
        raise SimulationConfigError("simulation.engine must be desolve or diffeqr")
    simulation["dna"] = bool(simulation["dna"])
    simulation["stochastic"] = bool(simulation["stochastic"])
    simulation["volume"] = _number(
        simulation.get("volume", 10), "simulation.volume", minimum=0
    )
    if simulation.get("seed") is not None:
        simulation["seed"] = int(_number(simulation["seed"], "simulation.seed"))
    if simulation["stochastic"] and simulation["engine"] == "diffeqr":
        raise SimulationConfigError("stochastic simulation does not use the diffeqr engine")

    normalized["simulation"] = simulation
    normalized["forcing"] = validate_forcing(normalized.get("forcing"))

    timing = dict(normalized.get("timing") or {})
    timing.setdefault("enabled", False)
    timing.setdefault("tolerancePercent", 1.0)
    timing.setdefault("stableSamples", 3)
    timing.setdefault("step", 0.1)
    timing.setdefault("maxTime", 100.0)
    timing["tolerancePercent"] = _number(
        timing["tolerancePercent"], "timing.tolerancePercent", minimum=0
    )
    timing["stableSamples"] = int(_number(timing["stableSamples"], "timing.stableSamples", minimum=1))
    timing["step"] = _number(timing["step"], "timing.step", minimum=1e-12)
    timing["maxTime"] = _number(timing["maxTime"], "timing.maxTime", minimum=timing["step"])
    normalized["timing"] = timing

    for index, droplet in enumerate(droplets):
        if not isinstance(droplet, Mapping):
            raise SimulationConfigError(f"goticulasIniciais[{index}] must be an object")
        droplet["volume"] = _number(
            droplet.get("volume", DEFAULT_DROPLET_VOLUME_M3),
            f"goticulasIniciais[{index}].volume",
            minimum=1e-30,
        )
        droplet.setdefault("volumeUnit", "m3")

    return normalized


def weighted_mix(droplets: Iterable[Mapping[str, Any]]) -> Dict[str, Any]:
    """Mix concentrations by conserved amount, not by direct concentration sum."""
    items = list(droplets)
    if not items:
        raise SimulationConfigError("at least one droplet is required for mixing")

    total_volume = sum(_number(item.get("volume"), "droplet.volume", minimum=1e-30) for item in items)
    species_amounts: Dict[str, float] = {}
    for item in items:
        volume = float(item["volume"])
        concentrations = item.get("concentrations", {})
        if not isinstance(concentrations, Mapping):
            raise SimulationConfigError("droplet.concentrations must be an object")
        for species, concentration in concentrations.items():
            species_amounts[str(species)] = species_amounts.get(str(species), 0.0) + (
                _number(concentration, f"concentration.{species}") * volume
            )

    return {
        "volume": total_volume,
        "concentrations": {
            species: amount / total_volume for species, amount in species_amounts.items()
        },
    }


def settling_time(
    times: Sequence[float],
    values: Sequence[float],
    tolerance_percent: float = 1.0,
    stable_samples: int = 3,
) -> float | None:
    """Return the first sample with a stable trailing window around the final value."""
    if len(times) != len(values) or not times:
        raise SimulationConfigError("times and values must have equal non-zero length")
    if stable_samples < 1:
        raise SimulationConfigError("stable_samples must be positive")

    final_value = float(values[-1])
    scale = max(abs(final_value), 1e-12)
    tolerance = abs(float(tolerance_percent)) / 100.0 * scale
    for index in range(0, len(values) - stable_samples + 1):
        window = values[index : index + stable_samples]
        if all(abs(float(value) - final_value) <= tolerance for value in window):
            return float(times[index])
    return None


def has_cycle(nodes: Iterable[str], edges: Iterable[Mapping[str, Any]]) -> bool:
    adjacency = {str(node): [] for node in nodes}
    indegree = {str(node): 0 for node in nodes}
    for edge in edges:
        source = str(edge.get("from", {}).get("name"))
        target = str(edge.get("to", {}).get("name"))
        if source in adjacency and target in indegree:
            adjacency[source].append(target)
            indegree[target] += 1

    queue = [node for node, degree in indegree.items() if degree == 0]
    visited = 0
    while queue:
        current = queue.pop()
        visited += 1
        for target in adjacency[current]:
            indegree[target] -= 1
            if indegree[target] == 0:
                queue.append(target)
    return visited != len(indegree)
