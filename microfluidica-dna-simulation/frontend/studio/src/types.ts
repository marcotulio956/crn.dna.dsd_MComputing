export type Node = {
  id: string;
  name: string;
  x: number;
  y: number;
  sink: boolean;
  ground: boolean;
};
export type Channel = {
  id: string;
  source: string;
  target: string;
  width: number;
  height: number;
};
export type Pump = {
  id: string;
  source: string;
  target: string;
  kind: "flow" | "pressure";
  value: number;
};
export type Species = {
  id: string;
  name: string;
  color: string;
  visible: boolean;
};
export type Reaction = {
  id: string;
  reactants: string[];
  products: string[];
  rate: number;
  reverse_rate: number | null;
};
export type Injection = {
  id: string;
  name: string;
  channel: string;
  time: number;
  position: number;
  volume: number;
  concentrations: Record<string, number>;
};
export type Project = {
  schema_version: 2;
  name: string;
  nodes: Node[];
  channels: Channel[];
  pumps: Pump[];
  species: Species[];
  reactions: Reaction[];
  injections: Injection[];
  annotations: { id: string; x: number; y: number; text: string }[];
  settings: {
    mixing: "physical" | "legacy_sum";
    continuous: { density: number; viscosity: number };
    dispersed: { density: number; viscosity: number };
    sample_count: number;
    rtol: number;
    atol: number;
    timeout: number;
    engine: "desolve" | "diffeqr";
    stochastic: boolean;
    dna: boolean;
    volume: number;
    seed: number | null;
    forcing: {
      name: string;
      species: string;
      params: Record<string, number>;
    } | null;
    timing_enabled: boolean;
    timing_tolerance_percent: number;
    timing_stable_samples: number;
    timing_step: number;
    timing_max_time: number;
  };
  provenance: { warnings?: string[]; [key: string]: unknown };
};
export type Boundary = {
  channel: string;
  position: number;
  towardSource: boolean;
};
export type Drop = {
  volume: number;
  boundaries: Boundary[];
  channels: string[];
};
export type Row = { time: number; [species: string]: number };
export type Result = {
  schema_version: 2;
  project: Project;
  states: {
    time: number;
    ordinal: number;
    droplets: Record<string, Drop>;
    flows: Record<string, number>;
    pressures: Record<string, number>;
  }[];
  events: {
    type: "injection" | "merge" | "exit";
    state: number;
    time: number;
    droplet: string;
    parents: string[];
    confidence?: string;
  }[];
  lifecycle: Record<
    string,
    {
      birth: number;
      death: number;
      birth_time: number;
      death_time: number;
      parents: string[];
      terminal: string;
    }
  >;
  chemistry: Record<string, Row[]>;
  warnings: string[];
  provenance: Record<string, unknown>;
};
export type Selection = {
  kind: "node" | "channel" | "injection" | "pump" | "annotation";
  id: string;
};
export const uid = (prefix: string) =>
  `${prefix}-${crypto.randomUUID().slice(0, 8)}`;
export function emptyProject(): Project {
  return {
    schema_version: 2,
    name: "Novo circuito",
    nodes: [],
    channels: [],
    pumps: [],
    species: [],
    reactions: [],
    injections: [],
    annotations: [],
    settings: {
      mixing: "physical",
      continuous: { density: 1000, viscosity: 0.001 },
      dispersed: { density: 1000, viscosity: 0.003 },
      sample_count: 501,
      rtol: 1e-8,
      atol: 1e-10,
      timeout: 180,
      engine: "desolve",
      stochastic: false,
      dna: false,
      volume: 10,
      seed: null,
      forcing: null,
      timing_enabled: false,
      timing_tolerance_percent: 1,
      timing_stable_samples: 3,
      timing_step: 0.1,
      timing_max_time: 100,
    },
    provenance: {},
  };
}
export async function api<T>(path: string, body?: unknown): Promise<T> {
  const response = await fetch(
    `/api/v2/${path}`,
    body === undefined
      ? {}
      : {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(body),
        },
  );
  const data = await response.json();
  if (!response.ok)
    throw new Error(
      typeof data.detail === "string"
        ? data.detail
        : JSON.stringify(data.detail || data),
    );
  return data;
}
export function download(
  name: string,
  data: string,
  type = "application/json",
) {
  const url = URL.createObjectURL(new Blob([data], { type }));
  const a = document.createElement("a");
  a.href = url;
  a.download = name;
  a.click();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
}
export const fmt = (n: number) =>
  Number.isFinite(n)
    ? n === 0
      ? "0"
      : Math.abs(n) < 0.001 || Math.abs(n) >= 1e5
        ? n.toExponential(3)
        : Number(n.toPrecision(5)).toString()
    : "—";
