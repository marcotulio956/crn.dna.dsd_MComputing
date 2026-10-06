import { useMemo, useState } from "react";
import { type Project, type Result, fmt, download } from "./types";
import { concentrationsAt, visibleDrops } from "./playback";
import type { Translate } from "./Editors";

export function CRN({ project: p, t }: { project: Project; t: Translate }) {
  const [search, setSearch] = useState(""),
    [graph, setGraph] = useState(false);
  const reactions = p.reactions.filter((r) =>
    [...r.reactants, ...r.products].some((id) =>
      p.species
        .find((s) => s.id === id)
        ?.name.toLowerCase()
        .includes(search.toLowerCase()),
    ),
  );
  const used = new Set(
    reactions.flatMap((r) => [...r.reactants, ...r.products]),
  );
  const species = p.species.filter((s) => used.has(s.id));
  const graphHeight = Math.max(species.length, reactions.length) * 42 + 70;
  return (
    <div className="crn-view">
      <div className="view-heading">
        <div>
          <small>CHEMICAL REACTION NETWORK</small>
          <h2>{t("Da molécula ao circuito", "From molecule to circuit")}</h2>
          <p>
            {t(
              "Cada bloco representa uma reação. Os números preservam os coeficientes estequiométricos.",
              "Each block represents one reaction. Numbers preserve stoichiometric coefficients.",
            )}
          </p>
          <button
            className={graph ? "active" : ""}
            onClick={() => setGraph((v) => !v)}
          >
            {graph
              ? t("Ver equações", "Show equations")
              : t(
                  "Ver grafo de espécies e reações",
                  "Show species–reaction graph",
                )}
          </button>
        </div>
        <input
          placeholder={t("Filtrar por espécie", "Filter by species")}
          value={search}
          onChange={(e) => setSearch(e.target.value)}
        />
      </div>
      {graph ? (
        <svg
          className="crn-graph"
          viewBox={`0 0 800 ${graphHeight}`}
          role="img"
          aria-label={t(
            "Grafo bipartido de espécies e reações",
            "Bipartite species and reaction graph",
          )}
        >
          <defs>
            <marker
              id="crn-in"
              viewBox="0 0 10 10"
              refX="9"
              refY="5"
              markerWidth="6"
              markerHeight="6"
              orient="auto"
            >
              <path d="M 0 0 L 10 5 L 0 10 z" fill="#2dd4bf" />
            </marker>
            <marker
              id="crn-out"
              viewBox="0 0 10 10"
              refX="9"
              refY="5"
              markerWidth="6"
              markerHeight="6"
              orient="auto"
            >
              <path d="M 0 0 L 10 5 L 0 10 z" fill="#a78bfa" />
            </marker>
          </defs>
          {reactions.flatMap((r, i) =>
            ["reactants", "products"].flatMap((side) =>
              Object.entries(
                (side === "reactants" ? r.reactants : r.products).reduce<
                  Record<string, number>
                >((a, id) => ((a[id] = (a[id] || 0) + 1), a), {}),
              ).map(([id, count]) => {
                const sy = 45 + species.findIndex((s) => s.id === id) * 42,
                  ry = 45 + i * 42;
                return (
                  <g key={`${r.id}-${side}-${id}`}>
                    <path
                      d={
                        side === "reactants"
                          ? `M 235 ${sy - 4} C 420 ${sy - 4},420 ${ry - 4},575 ${ry - 4}`
                          : `M 575 ${ry + 4} C 435 ${ry + 4},435 ${sy + 4},235 ${sy + 4}`
                      }
                      stroke={side === "reactants" ? "#2dd4bf" : "#a78bfa"}
                      strokeOpacity={0.45}
                      fill="none"
                      markerEnd={`url(#crn-${side === "reactants" ? "in" : "out"})`}
                    />
                    {count > 1 && (
                      <text
                        x={side === "reactants" ? 260 : 540}
                        y={side === "reactants" ? sy - 10 : ry + 17}
                        fill="#cad8e5"
                        fontSize={10}
                      >
                        {count}
                      </text>
                    )}
                  </g>
                );
              }),
            ),
          )}
          {species.map((s, i) => (
            <g key={s.id}>
              <circle cx="225" cy={45 + i * 42} r="9" fill={s.color} />
              <text
                x="205"
                y={49 + i * 42}
                textAnchor="end"
                fill="#c6d6e5"
                fontSize={12}
              >
                {s.name}
              </text>
            </g>
          ))}
          {reactions.map((r, i) => (
            <g key={r.id}>
              <rect
                x="577"
                y={31 + i * 42}
                width="165"
                height="28"
                rx="5"
                fill="#1b3046"
                stroke="#526d86"
              />
              <text x="588" y={49 + i * 42} fill="#c6d6e5" fontSize={11}>
                {r.id} · k={fmt(r.rate)}
                {r.reverse_rate !== null ? " ⇄" : ""}
              </text>
            </g>
          ))}
        </svg>
      ) : (
        <div className="crn-list">
          {reactions.map((r, i) => {
            const side = (ids: string[]) =>
              Object.entries(
                ids.reduce<Record<string, number>>(
                  (a, k) => ((a[k] = (a[k] || 0) + 1), a),
                  {},
                ),
              ).map(([id, count]) => {
                const s = p.species.find((s) => s.id === id);
                return (
                  <span
                    className="molecule"
                    key={id}
                    style={{ borderColor: s?.color }}
                  >
                    <i style={{ background: s?.color }} />
                    {count > 1 ? count + " × " : ""}
                    {s?.name || id}
                  </span>
                );
              });
            return (
              <div className="crn-row" key={r.id}>
                <small>{String(i + 1).padStart(2, "0")}</small>
                <div className="reaction-side">{side(r.reactants)}</div>
                <div className="reaction-arrow">
                  <span>{r.reverse_rate === null ? "→" : "⇄"}</span>
                  <small>
                    k {fmt(r.rate)}
                    <br />
                    nM^{1 - r.reactants.length}/s
                  </small>
                </div>
                <div className="reaction-side">{side(r.products)}</div>
              </div>
            );
          })}
        </div>
      )}
      {!reactions.length && (
        <p>
          {t(
            "Adicione espécies e reações no painel Química.",
            "Add species and reactions in the Chemistry panel.",
          )}
        </p>
      )}
    </div>
  );
}

export function Charts({
  result: r,
  time,
  ordinal,
  drop,
  seek,
  t,
  baseline,
}: {
  result: Result;
  time: number;
  ordinal?: number;
  drop: string;
  seek: (n: number) => void;
  t: Translate;
  baseline?: Result | null;
}) {
  const [mode, setMode] = useState("droplet");
  const end = r.states[r.states.length - 1].time;
  const species = r.project.species.filter((s) => s.visible);
  const data = useMemo(() => {
    if (mode === "droplet") return r.chemistry[drop] || [];
    // Aggregate amounts and volume-weighted concentration only over active droplets.
    const times = [
      ...new Set([
        ...r.states.map((s) => s.time),
        ...Array.from({ length: 301 }, (_, i) => (i * end) / 300),
      ]),
    ].sort((a, b) => a - b);
    return times.map((time) => {
      const active = visibleDrops(r, time);
      const values: Record<string, number> = { time };
      const total = Object.values(active).reduce((sum, d) => sum + d.volume, 0);
      for (const s of r.project.species) {
        const amount = Object.entries(active).reduce(
          (sum, [id, d]) =>
            sum +
            (concentrationsAt(r.chemistry[id], time).values[s.id] || 0) *
              d.volume,
          0,
        );
        values[s.id] =
          mode === "amount" ? amount * 1e9 : total ? amount / total : 0;
      }
      return values;
    });
  }, [r, drop, mode, end]);
  const lineage = (result: Result, id: string): string =>
    result.lifecycle[id]?.parents.length
      ? result.lifecycle[id].parents
          .map((parent) => lineage(result, parent))
          .sort()
          .join("+")
      : id;
  const compatible =
    baseline &&
    species.every((s) =>
      baseline.project.species.some((v) => v.id === s.id && v.name === s.name),
    );
  const comparison =
    mode === "droplet" &&
    baseline &&
    compatible &&
    lineage(baseline, drop) === lineage(r, drop)
      ? baseline.chemistry[drop] || []
      : [];
  const max =
    Math.max(
      1e-12,
      ...data.flatMap((row) => species.map((s) => row[s.id] || 0)),
      ...comparison.flatMap((row) => species.map((s) => row[s.id] || 0)),
    ) * 1.08;
  const W = 900,
    H = 190,
    left = 62,
    right = 22,
    top = 15,
    bottom = 35;
  const x = (time: number) => left + (time / (end || 1)) * (W - left - right),
    y = (v: number) => H - bottom - (v / max) * (H - top - bottom);
  const exportCSV = () =>
    download(
      `${drop || mode}.csv`,
      [
        [
          "time_s",
          ...r.project.species.map(
            (s) => `${s.name}_${mode === "amount" ? "fmol" : "nM"}`,
          ),
        ].join(","),
        ...data.map((row) =>
          [row.time, ...r.project.species.map((s) => row[s.id] || 0)].join(","),
        ),
      ].join("\n"),
      "text/csv",
    );
  return (
    <div className="charts">
      <div className="chart-header">
        <strong>{t("Dinâmica química", "Chemical dynamics")}</strong>
        <select
          aria-label={t("Modo do gráfico", "Chart mode")}
          value={mode}
          onChange={(e) => setMode(e.target.value)}
        >
          <option value="droplet">
            {drop || t("Selecione uma gota", "Select a droplet")}
          </option>
          <option value="mean">
            {t(
              "Média ponderada · gotas ativas",
              "Weighted mean · active droplets",
            )}
          </option>
          <option value="amount">
            {t(
              "Quantidade total · gotas ativas",
              "Total amount · active droplets",
            )}
          </option>
        </select>
        <button onClick={exportCSV}>CSV</button>
        <span className="muted">{mode === "amount" ? "fmol" : "nM"}</span>
      </div>
      {baseline && (
        <small className="muted">
          {comparison.length
            ? `${t("Tracejado", "Dashed")}: ${baseline.project.name} · ${baseline.project.settings.mixing}`
            : t(
                "Sem trajetória compatível para comparar nesta seleção.",
                "No compatible trajectory to compare for this selection.",
              )}
        </small>
      )}
      <svg
        viewBox={`0 0 ${W} ${H}`}
        role="img"
        aria-label={t(
          "Concentrações ao longo do tempo. Clique para navegar.",
          "Concentrations over time. Click to seek.",
        )}
        onClick={(e) => {
          const rect = e.currentTarget.getBoundingClientRect();
          seek(
            Math.max(
              0,
              Math.min(
                end,
                ((((e.clientX - rect.left) / rect.width) * W - left) /
                  (W - left - right)) *
                  end,
              ),
            ),
          );
        }}
      >
        {[0, 0.25, 0.5, 0.75, 1].map((f) => (
          <g key={f}>
            <line
              x1={left}
              x2={W - right}
              y1={y(f * max)}
              y2={y(f * max)}
              stroke="#263446"
            />
            <text x={left - 8} y={y(f * max) + 4} textAnchor="end">
              {fmt(f * max)}
            </text>
            <text x={x(f * end)} y={H - 9} textAnchor="middle">
              {fmt(f * end)} s
            </text>
          </g>
        ))}
        {species.map((s) => (
          <polyline
            key={s.id}
            points={data
              .map((row) => `${x(row.time)},${y(row[s.id] || 0)}`)
              .join(" ")}
            stroke={s.color}
            fill="none"
            strokeWidth={2}
          />
        ))}
        {species.map((s) => (
          <polyline
            key={`comparison-${s.id}`}
            points={comparison
              .filter((row) => row.time <= end)
              .map((row) => `${x(row.time)},${y(row[s.id] || 0)}`)
              .join(" ")}
            stroke={s.color}
            fill="none"
            strokeDasharray="5 4"
            opacity={0.6}
            strokeWidth={1.5}
          />
        ))}
        <line
          x1={x(time)}
          x2={x(time)}
          y1={top}
          y2={H - bottom}
          stroke="#fff"
          strokeDasharray="3 4"
        />
      </svg>
      <div className="legend">
        {species.map((s) => (
          <span key={s.id}>
            <i style={{ background: s.color }} />
            {s.name}
          </span>
        ))}
      </div>
      {ordinal !== undefined && (
        <small className="muted">
          {t("Estado", "State")} {ordinal}
        </small>
      )}
    </div>
  );
}
