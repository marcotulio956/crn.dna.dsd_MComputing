import { useEffect, useRef, useState } from "react";
import { Stage, Layer, Line, Circle, Text, Group, Arrow } from "react-konva";
import Konva from "konva";
import {
  MousePointer2,
  Hand,
  Plus,
  GitBranch,
  StickyNote,
  Scan,
  ZoomIn,
  ZoomOut,
  ImageDown,
} from "lucide-react";
import {
  type Project,
  type Selection,
  type Result,
  fmt,
  uid,
  download,
} from "./types";
import { visibleDrops, occupied, concentrationsAt, stateAt } from "./playback";
type Props = {
  project: Project;
  edit: (fn: (p: Project) => void) => void;
  selection: Selection[];
  select: (s: Selection[], add?: boolean) => void;
  result: Result | null;
  time: number;
  ordinal?: number;
  inspect: (id: string) => void;
  fitToken: number;
  t: (pt: string, en: string) => string;
};
const UM = 1e6;
export default function Canvas({
  project: p,
  edit,
  selection,
  select,
  result,
  time,
  ordinal,
  inspect,
  fitToken,
  t,
}: Props) {
  const host = useRef<HTMLDivElement>(null),
    stage = useRef<Konva.Stage>(null);
  const [size, setSize] = useState({ width: 800, height: 600 });
  const [camera, setCamera] = useState({ x: 80, y: 80, scale: 0.18 });
  const [tool, setTool] = useState("select"),
    [grid, setGrid] = useState(true),
    [snap, setSnap] = useState(true),
    [labels, setLabels] = useState(true),
    [overlay, setOverlay] = useState(false),
    [showPumps, setShowPumps] = useState(false);
  const [start, setStart] = useState<string | null>(null),
    [hover, setHover] = useState<{ id: string; x: number; y: number } | null>(
      null,
    );
  useEffect(() => {
    const el = host.current;
    if (!el) return;
    const observer = new ResizeObserver(([e]) =>
      setSize({ width: e.contentRect.width, height: e.contentRect.height }),
    );
    observer.observe(el);
    return () => observer.disconnect();
  }, []);
  const fit = () => {
    if (!p.nodes.length) {
      setCamera({ x: 80, y: 80, scale: 0.18 });
      return;
    }
    const xs = p.nodes.map((n) => n.x * UM),
      ys = p.nodes.map((n) => n.y * UM);
    const minX = Math.min(...xs),
      maxX = Math.max(...xs),
      minY = Math.min(...ys),
      maxY = Math.max(...ys);
    const scale = Math.max(
      0.015,
      Math.min(
        (size.width - 110) / Math.max(maxX - minX, 400),
        (size.height - 115) / Math.max(maxY - minY, 400),
        0.6,
      ),
    );
    setCamera({
      x: (size.width - (maxX + minX) * scale) / 2,
      y: (size.height + 55 - (maxY + minY) * scale) / 2,
      scale,
    });
  };
  useEffect(() => {
    fit();
  }, [fitToken, size.width, size.height]); // camera is not project geometry and never changes simulation
  const zoom = (factor: number, x = size.width / 2, y = size.height / 2) =>
    setCamera((c) => {
      const scale = Math.max(0.015, Math.min(5, c.scale * factor));
      return {
        scale,
        x: x - ((x - c.x) * scale) / c.scale,
        y: y - ((y - c.y) * scale) / c.scale,
      };
    });
  const coordinate = (n: number) => (snap ? Math.round(n / 100) * 100 : n);
  const nodes = Object.fromEntries(p.nodes.map((n) => [n.id, n]));
  const selected = (kind: Selection["kind"], id: string) =>
    selection.some((s) => s.kind === kind && s.id === id);
  const chooseNode = (id: string, add: boolean) => {
    if (tool === "channel") {
      if (start && start !== id) {
        edit((d) =>
          d.channels.push({
            id: uid("c"),
            source: start,
            target: id,
            width: 50e-6,
            height: 30e-6,
          }),
        );
        setStart(null);
      } else setStart(id);
    } else select([{ kind: "node", id }], add);
  };
  const addAt = () => {
    const point = stage.current?.getRelativePointerPosition();
    if (!point) return;
    if (tool === "node") {
      const id = uid("n");
      edit((d) =>
        d.nodes.push({
          id,
          name: `N${d.nodes.length + 1}`,
          x: coordinate(point.x) / UM,
          y: coordinate(point.y) / UM,
          sink: false,
          ground: false,
        }),
      );
      select([{ kind: "node", id }]);
    } else if (tool === "note") {
      const id = uid("a");
      edit((d) =>
        d.annotations.push({
          id,
          x: point.x / UM,
          y: point.y / UM,
          text: t("Anotação", "Annotation"),
        }),
      );
      select([{ kind: "annotation", id }]);
    } else {
      select([]);
      setStart(null);
    }
  };
  const active = result ? visibleDrops(result, time, ordinal) : {};
  const state = result?.states[ordinal ?? stateAt(result, time)];
  const color = (id: string) => {
    const i = (
      result ? Object.keys(result.lifecycle) : p.injections.map((d) => d.id)
    ).indexOf(id);
    return ["#2dd4bf", "#c4b5fd", "#fbbf24", "#38bdf8", "#fb7185", "#a3e635"][
      Math.max(0, i) % 6
    ];
  };
  const intervals = Object.entries(active).flatMap(([id, drop]) =>
    occupied(drop).map((v) => ({ ...v, id, volume: drop.volume })),
  );
  const exportSVG = () => {
    const escape = (s: string) =>
      s.replace(
        /[&<>"']/g,
        (c) =>
          ({
            "&": "&amp;",
            "<": "&lt;",
            ">": "&gt;",
            '"': "&quot;",
            "'": "&apos;",
          })[c]!,
      );
    const lines = p.channels.map((c) => {
      const a = nodes[c.source],
        b = nodes[c.target];
      return `<line x1="${a.x * UM}" y1="${a.y * UM}" x2="${b.x * UM}" y2="${b.y * UM}" stroke="#486079" stroke-width="${c.width * UM}"/>`;
    });
    const labels = p.nodes.map(
      (n) =>
        `<text x="${n.x * UM + 40}" y="${n.y * UM - 40}" fill="#fff" font-size="60">${escape(n.name)}</text>`,
    );
    download(
      `${p.name}.svg`,
      `<svg xmlns="http://www.w3.org/2000/svg" width="${size.width}" height="${size.height}"><rect width="100%" height="100%" fill="#0d1623"/><g transform="translate(${camera.x},${camera.y}) scale(${camera.scale})">${lines.join("")}${labels.join("")}</g></svg>`,
      "image/svg+xml",
    );
  };
  const gridSize = 100 * camera.scale;
  const dots: JSX.Element[] = [];
  if (grid && gridSize >= 8) {
    for (
      let x = ((camera.x % gridSize) + gridSize) % gridSize;
      x < size.width;
      x += gridSize
    )
      for (
        let y = ((camera.y % gridSize) + gridSize) % gridSize;
        y < size.height;
        y += gridSize
      )
        dots.push(
          <Circle
            key={`${x}-${y}`}
            x={x}
            y={y}
            radius={0.7}
            fill="#29384b"
            listening={false}
          />,
        );
  }
  const tip =
    hover && result && active[hover.id] ? concentrationsAt(result.chemistry[hover.id], time) : null;
  return (
    <div className="canvas-wrap" ref={host}>
      <div className="canvas-tools">
        {[
          {
            id: "select",
            icon: MousePointer2,
            label: t("Selecionar / mover", "Select / move"),
          },
          { id: "pan", icon: Hand, label: t("Mover câmera", "Pan") },
          { id: "node", icon: Plus, label: t("Adicionar nó", "Add node") },
          {
            id: "channel",
            icon: GitBranch,
            label: t("Conectar dois nós", "Connect two nodes"),
          },
          { id: "note", icon: StickyNote, label: t("Anotação", "Annotation") },
        ].map((v) => (
          <button
            key={v.id}
            title={v.label}
            aria-label={v.label}
            className={tool === v.id ? "active" : ""}
            onClick={() => {
              setTool(v.id);
              setStart(null);
            }}
          >
            <v.icon size={18} />
          </button>
        ))}
        <span className="divider" />
        <button
          title={t("Enquadrar circuito", "Fit circuit")}
          aria-label={t("Enquadrar circuito", "Fit circuit")}
          onClick={fit}
        >
          <Scan size={18} />
        </button>
        <button aria-label="Zoom in" onClick={() => zoom(1.25)}>
          <ZoomIn size={18} />
        </button>
        <button aria-label="Zoom out" onClick={() => zoom(0.8)}>
          <ZoomOut size={18} />
        </button>
        <button
          title="PNG"
          aria-label="Export PNG"
          onClick={() => {
            const a = document.createElement("a");
            a.download = p.name + ".png";
            a.href = stage.current!.toDataURL({ pixelRatio: 2 });
            a.click();
          }}
        >
          <ImageDown size={18} />
        </button>
        <button onClick={exportSVG}>SVG</button>
      </div>
      <div className="canvas-options">
        <label>
          <input
            type="checkbox"
            checked={grid}
            onChange={(e) => setGrid(e.target.checked)}
          />
          {t("Grade", "Grid")}
        </label>
        <label>
          <input
            type="checkbox"
            checked={snap}
            onChange={(e) => setSnap(e.target.checked)}
          />
          Snap
        </label>
        <label>
          <input
            type="checkbox"
            checked={labels}
            onChange={(e) => setLabels(e.target.checked)}
          />
          {t("Nomes", "Labels")}
        </label>
        <label>
          <input
            type="checkbox"
            checked={showPumps}
            onChange={(e) => setShowPumps(e.target.checked)}
          />
          {t("Bombas", "Pumps")}
        </label>
        <label>
          <input
            type="checkbox"
            checked={overlay}
            onChange={(e) => setOverlay(e.target.checked)}
          />
          {t("Fluxo / pressão", "Flow / pressure")}
        </label>
      </div>
      <Stage
        ref={stage}
        width={size.width}
        height={size.height}
        x={camera.x}
        y={camera.y}
        scaleX={camera.scale}
        scaleY={camera.scale}
        draggable={tool === "pan"}
        onDragEnd={(e) => {
          if (e.target === stage.current)
            setCamera((c) => ({ ...c, x: e.target.x(), y: e.target.y() }));
        }}
        onWheel={(e) => {
          e.evt.preventDefault();
          const at = stage.current?.getPointerPosition();
          if (at) zoom(e.evt.deltaY > 0 ? 0.9 : 1.1, at.x, at.y);
        }}
        onClick={(e) => {
          if (e.target === stage.current) addAt();
        }}
        onTap={(e) => {
          if (e.target === stage.current) addAt();
        }}
      >
        <Layer
          listening={false}
          x={-camera.x / camera.scale}
          y={-camera.y / camera.scale}
          scaleX={1 / camera.scale}
          scaleY={1 / camera.scale}
        >
          {dots}
        </Layer>
        <Layer>
          {p.channels.map((c) => {
            const a = nodes[c.source],
              b = nodes[c.target];
            if (!a || !b) return null;
            const points = [a.x * UM, a.y * UM, b.x * UM, b.y * UM];
            return (
              <Group key={c.id}>
                <Line
                  points={points}
                  stroke={selected("channel", c.id) ? "#2dd4bf" : "#32465d"}
                  strokeWidth={
                    Math.max(c.width * UM, 8 / camera.scale) + 6 / camera.scale
                  }
                  lineCap="round"
                  hitStrokeWidth={22 / camera.scale}
                  onClick={(e) =>
                    select([{ kind: "channel", id: c.id }], e.evt.shiftKey)
                  }
                  onTap={() => select([{ kind: "channel", id: c.id }])}
                />
                <Line
                  listening={false}
                  points={points}
                  stroke="#162638"
                  strokeWidth={Math.max(c.width * UM, 8 / camera.scale)}
                  lineCap="round"
                />
                {overlay && state && (
                  <Text
                    listening={false}
                    x={((a.x + b.x) * UM) / 2}
                    y={((a.y + b.y) * UM) / 2 - 22 / camera.scale}
                    text={`${fmt(state.flows[c.id] * 1e12)} nL/s`}
                    fill="#7dd3fc"
                    fontSize={11 / camera.scale}
                  />
                )}
              </Group>
            );
          })}
          {p.pumps
            .filter((pump) => showPumps || selected("pump", pump.id))
            .map((pump) => {
              const a = nodes[pump.source],
                b = nodes[pump.target];
              if (!a || !b) return null;
              return (
                <Arrow
                  key={pump.id}
                  points={[
                    a.x * UM,
                    a.y * UM,
                    ((a.x + b.x) * UM) / 2,
                    ((a.y + b.y) * UM) / 2 - 150,
                    b.x * UM,
                    b.y * UM,
                  ]}
                  tension={0.3}
                  stroke={selected("pump", pump.id) ? "#fbbf24" : "#7c668e"}
                  fill="#a78bfa"
                  opacity={0.65}
                  strokeWidth={1 / camera.scale}
                  dash={[5 / camera.scale, 5 / camera.scale]}
                  pointerLength={7 / camera.scale}
                  pointerWidth={7 / camera.scale}
                  hitStrokeWidth={14 / camera.scale}
                  onClick={() => select([{ kind: "pump", id: pump.id }])}
                />
              );
            })}
          {!result &&
            p.injections.map((d) => {
              const c = p.channels.find((c) => c.id === d.channel);
              if (!c) return null;
              const a = nodes[c.source],
                b = nodes[c.target];
              return (
                <Circle
                  key={d.id}
                  x={(a.x + (b.x - a.x) * d.position) * UM}
                  y={(a.y + (b.y - a.y) * d.position) * UM}
                  radius={7 / camera.scale}
                  fill={color(d.id)}
                  stroke={selected("injection", d.id) ? "#fff" : "#0c1522"}
                  strokeWidth={2 / camera.scale}
                  onClick={() => select([{ kind: "injection", id: d.id }])}
                  onTap={() => select([{ kind: "injection", id: d.id }])}
                />
              );
            })}
          {intervals.map((v, i) => {
            const c = p.channels.find((c) => c.id === v.channel);
            if (!c) return null;
            const a = nodes[c.source],
              b = nodes[c.target];
            return (
              <Line
                key={`${v.id}-${i}`}
                points={[
                  (a.x + (b.x - a.x) * v.start) * UM,
                  (a.y + (b.y - a.y) * v.start) * UM,
                  (a.x + (b.x - a.x) * v.end) * UM,
                  (a.y + (b.y - a.y) * v.end) * UM,
                ]}
                stroke={color(v.id)}
                strokeWidth={Math.max(c.width * UM, 8 / camera.scale)}
                lineCap="round"
                shadowColor={color(v.id)}
                shadowBlur={5 / camera.scale}
                shadowOpacity={0.25}
                hitStrokeWidth={24 / camera.scale}
                onMouseMove={() => {
                  const at = stage.current?.getPointerPosition();
                  if (at) setHover({ id: v.id, ...at });
                }}
                onMouseLeave={() => setHover(null)}
                onClick={() => inspect(v.id)}
                onTap={() => inspect(v.id)}
              />
            );
          })}
          {p.nodes.map((n) => (
            <Group
              key={n.id}
              x={n.x * UM}
              y={n.y * UM}
              draggable={tool === "select" && !result}
              onDragEnd={(e) => {
                const x = coordinate(e.target.x()) / UM,
                  y = coordinate(e.target.y()) / UM;
                edit((d) => {
                  const node = d.nodes.find((v) => v.id === n.id)!;
                  node.x = x;
                  node.y = y;
                });
              }}
              onClick={(e) => chooseNode(n.id, e.evt.shiftKey)}
              onTap={() => chooseNode(n.id, false)}
            >
              <Circle
                radius={5 / camera.scale}
                fill={
                  n.sink ? "#fb923c" : start === n.id ? "#fbbf24" : "#101c2b"
                }
                stroke={
                  selected("node", n.id)
                    ? "#2dd4bf"
                    : n.sink
                      ? "#fdba74"
                      : "#9bb0c7"
                }
                strokeWidth={2 / camera.scale}
                hitStrokeWidth={14 / camera.scale}
              />
              {labels && (
                <Text
                  x={10 / camera.scale}
                  y={-21 / camera.scale}
                  text={n.name + (n.sink ? " ↧" : "")}
                  fontSize={11 / camera.scale}
                  fill="#bed0e5"
                />
              )}
              {overlay && state && (
                <Text
                  x={10 / camera.scale}
                  y={0}
                  text={`${fmt(state.pressures[n.id])} Pa`}
                  fontSize={10 / camera.scale}
                  fill="#bca6ed"
                />
              )}
            </Group>
          ))}
          {p.annotations.map((a) => (
            <Text
              key={a.id}
              x={a.x * UM}
              y={a.y * UM}
              text={a.text}
              fontSize={13 / camera.scale}
              fill="#e2c989"
              draggable={tool === "select"}
              onClick={() => select([{ kind: "annotation", id: a.id }])}
              onDragEnd={(e) =>
                edit((d) => {
                  const note = d.annotations.find((n) => n.id === a.id)!;
                  note.x = e.target.x() / UM;
                  note.y = e.target.y() / UM;
                })
              }
            />
          ))}
        </Layer>
      </Stage>
      {!p.nodes.length && (
        <div className="canvas-empty">
          <GitBranch size={36} />
          <h2>
            {t("Seu laboratório, sem limites", "Your lab, without limits")}
          </h2>
          <p>
            {t(
              "Importe um circuito ou use + para adicionar nós.",
              "Import a circuit or use + to add nodes.",
            )}
          </p>
          <p>
            {t(
              "Conecte os nós, configure as gotas e explore a química.",
              "Connect nodes, configure droplets and explore the chemistry.",
            )}
          </p>
        </div>
      )}
      <div className="canvas-caption">
        <span>
          {tool === "channel"
            ? start
              ? t("Escolha o nó de destino", "Choose target node")
              : t("Escolha o nó de origem", "Choose source node")
            : t(
                "Scroll: zoom · Shift: multisseleção",
                "Scroll: zoom · Shift: multiselect",
              )}
        </span>
        <span>{fmt(100 / camera.scale)} µm / 100 px</span>
      </div>
      {hover && tip && (
        <div
          className="drop-tooltip"
          style={{
            left: Math.min(hover.x + 18, size.width - 260),
            top: Math.max(60, Math.min(hover.y + 18, size.height - 260)),
          }}
        >
          <strong>{hover.id}</strong>
          <small>
            {fmt(time)} s · {fmt((active[hover.id]?.volume || 0) * 1e12)} nL
          </small>
          <small>
            {tip.interpolated
              ? t("Concentração interpolada", "Interpolated concentration")
              : t("Amostra calculada", "Computed sample")}
          </small>
          {[...p.species]
            .sort((a,b)=>(tip.values[b.id]||0)-(tip.values[a.id]||0))
            .slice(0, 8)
            .map((s) => (
              <div key={s.id}>
                <span style={{ color: s.color }}>{s.name}</span>
                <b>{fmt(tip.values[s.id] || 0)} nM</b>
              </div>
            ))}
          {p.species.length > 8 && (
            <small>
              {t("Clique para todas as espécies", "Click for all species")}
            </small>
          )}
        </div>
      )}
    </div>
  );
}
