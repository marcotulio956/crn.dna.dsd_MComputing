import { useCallback, useEffect, useRef, useState } from "react";
import {
  Dna,
  Upload,
  Download,
  Play,
  Pause,
  Square,
  SkipBack,
  SkipForward,
  Undo2,
  Redo2,
  PanelLeftClose,
  PanelRightClose,
  Maximize2,
  FlaskConical,
  GitBranch,
  Settings2,
  Droplets,
  Plus,
  Copy,
  Trash2,
  ChevronRight,
  CheckCircle2,
  LoaderCircle,
  AlertTriangle,
  Repeat2,
  FilePlus2,
} from "lucide-react";
import Canvas from "./Canvas";
import {
  ChemistryEditor,
  DropInspector,
  Field,
  Inspector,
  SettingsEditor,
} from "./Editors";
import { CRN, Charts } from "./Results";
import { useProject } from "./useProject";
import {
  api,
  download,
  emptyProject,
  fmt,
  uid,
  type Project,
  type Result,
  type Selection,
} from "./types";
import { stateAt } from "./playback";
type Job = { id: string; status: string; error?: string };
type RunEntry = Job & { name: string; mixing: string; created: string };

export default function App() {
  const { project, edit, replace, undo, redo, canUndo, canRedo } = useProject();
  const [language, setLanguage] = useState<"pt" | "en">("pt");
  const t = useCallback(
    (pt: string, en: string) => (language === "pt" ? pt : en),
    [language],
  );
  const [selection, setSelection] = useState<Selection[]>([]),
    [panel, setPanel] = useState("circuit"),
    [view, setView] = useState("canvas");
  const [left, setLeft] = useState(true),
    [right, setRight] = useState(true),
    [chart, setChart] = useState(false);
  const [examples, setExamples] = useState<string[]>([]),
    [error, setError] = useState(""),
    [notice, setNotice] = useState(""),
    [fitToken, setFitToken] = useState(0);
  const [job, setJob] = useState<Job | null>(null),
    [result, setResult] = useState<Result | null>(null),
    [resultProject, setResultProject] = useState("");
  const [time, setTime] = useState(0),
    [ordinal, setOrdinal] = useState<number | undefined>(undefined),
    [playing, setPlaying] = useState(false),
    [speed, setSpeed] = useState(100),
    [loop, setLoop] = useState(false),
    [drop, setDrop] = useState("");
  const [runLog, setRunLog] = useState(""),
    [showLog, setShowLog] = useState(false);
  const [history, setHistory] = useState<RunEntry[] | null>(null),
    [baseline, setBaseline] = useState<Result | null>(null);
  const file = useRef<HTMLInputElement>(null),
    reactionFile = useRef<HTMLInputElement>(null);
  const busy = job?.status === "queued" || job?.status === "running";
  const stale = !!result && resultProject !== JSON.stringify(project);
  const duration = result?.states[result.states.length - 1].time || 0;
  const shownProject = result && !stale ? result.project : project;
  useEffect(() => {
    document.documentElement.lang = language === "pt" ? "pt-BR" : "en";
  }, [language]);
  useEffect(() => {
    api<string[]>("examples")
      .then(setExamples)
      .catch((e) => setError(String(e)));
  }, []);
  useEffect(() => {
    if (stale) setPlaying(false);
  }, [stale]);
  useEffect(() => {
    if (!job || !busy) return;
    let cancelled = false,
      inFlight = false;
    const timer = setInterval(async () => {
      if (inFlight) return;
      inFlight = true;
      try {
        const update = await api<Job>(`runs/${job.id}`);
        if (cancelled) return;
        if (update.status === "completed") {
          const data = await api<Result>(`runs/${job.id}/result`);
          if (cancelled) return;
          setResult(data);
          setResultProject(JSON.stringify(data.project));
          setTime(0);
          setOrdinal(0);
          setDrop(Object.keys(data.lifecycle)[0] || "");
          setNotice(
            t(
              "Simulação concluída · merges verificados",
              "Simulation completed · merges verified",
            ),
          );
        } else if (update.status === "failed")
          setError(update.error || "Simulation failed");
        setJob(update);
      } catch (e) {
        if (!cancelled) setError(String(e));
      } finally {
        inFlight = false;
      }
    }, 800);
    return () => {
      cancelled = true;
      clearInterval(timer);
    };
  }, [job?.id, busy, t]);
  useEffect(() => {
    if (!playing || !result) return;
    let handle = 0,
      last = performance.now();
    const tick = (now: number) => {
      const elapsed = Math.min(0.1, (now - last) / 1000);
      last = now;
      setOrdinal(undefined);
      setTime((current) => {
        const next = current + elapsed * speed;
        if (next >= duration) {
          if (loop) return duration ? next % duration : 0;
          setPlaying(false);
          return duration;
        }
        return next;
      });
      handle = requestAnimationFrame(tick);
    };
    handle = requestAnimationFrame(tick);
    return () => cancelAnimationFrame(handle);
  }, [playing, speed, duration, loop, result]);
  const select = (items: Selection[], add = false) => {
    setSelection((old) =>
      add
        ? [...old.filter((s) => !items.some((v) => v.id === s.id)), ...items]
        : items,
    );
    setDrop("");
    setRight(true);
  };
  const remove = useCallback(() => {
    edit((d) => {
      for (const s of selection) {
        if (s.kind === "node") {
          d.nodes = d.nodes.filter((n) => n.id !== s.id);
          d.channels = d.channels.filter(
            (c) => c.source !== s.id && c.target !== s.id,
          );
          d.pumps = d.pumps.filter(
            (p) => p.source !== s.id && p.target !== s.id,
          );
        }
        if (s.kind === "channel")
          d.channels = d.channels.filter((c) => c.id !== s.id);
        if (s.kind === "injection")
          d.injections = d.injections.filter((v) => v.id !== s.id);
        if (s.kind === "pump") d.pumps = d.pumps.filter((v) => v.id !== s.id);
        if (s.kind === "annotation")
          d.annotations = d.annotations.filter((v) => v.id !== s.id);
      }
      d.injections = d.injections.filter((v) =>
        d.channels.some((c) => c.id === v.channel),
      );
    });
    setSelection([]);
  }, [edit, selection]);
  const duplicate = () =>
    edit((d) => {
      const map: Record<string, string> = {};
      const chosen = new Set(selection.map((s) => s.id));
      for (const node of [...d.nodes])
        if (chosen.has(node.id)) {
          const id = uid("n");
          map[node.id] = id;
          d.nodes.push({
            ...node,
            id,
            name: node.name + " copy",
            x: node.x + 200e-6,
            y: node.y + 200e-6,
          });
        }
      for (const channel of [...d.channels])
        if (map[channel.source] && map[channel.target])
          d.channels.push({
            ...channel,
            id: uid("c"),
            source: map[channel.source],
            target: map[channel.target],
          });
      for (const inj of [...d.injections])
        if (chosen.has(inj.id))
          d.injections.push({
            ...structuredClone(inj),
            id: uid("d"),
            name: inj.name + " copy",
            time: inj.time + 1,
          });
    });
  useEffect(() => {
    const handler = (e: KeyboardEvent) => {
      if ((e.target as HTMLElement).closest("input,textarea,select")) return;
      if ((e.ctrlKey || e.metaKey) && e.key === "z") {
        e.preventDefault();
        e.shiftKey ? redo() : undo();
      }
      if (e.key === "Delete" && selection.length) {
        e.preventDefault();
        remove();
      }
      if (e.code === "Space" && result && !stale) {
        e.preventDefault();
        setPlaying((v) => !v);
      }
    };
    window.addEventListener("keydown", handler);
    return () => window.removeEventListener("keydown", handler);
  }, [undo, redo, remove, selection, result, stale]);
  async function loadProject(p: Project) {
    replace(p);
    setSelection([]);
    setResult(null);
    setResultProject("");
    setPlaying(false);
    setTime(0);
    setDrop("");
    setFitToken((n) => n + 1);
    setError("");
    setNotice(
      t(
        "Circuito carregado. Revise os avisos de importação.",
        "Circuit loaded. Review import warnings.",
      ),
    );
  }
  async function importFile(f: File) {
    try {
      const raw = JSON.parse(await f.text());
      const p = await api<Project>("projects/import", raw);
      await loadProject({
        ...p,
        name:
          p.name === "Imported circuit"
            ? f.name.replace(/\.json$/i, "")
            : p.name,
      });
    } catch (e) {
      setError(String(e));
    }
  }
  async function run() {
    try {
      setError("");
      setNotice("");
      setPlaying(false);
      const update = await api<Job>("runs", project);
      setJob(update);
    } catch (e) {
      setError(String(e));
    }
  }
  async function restoreRun(entry: RunEntry, compare = false) {
    try {
      const data = await api<Result>(`runs/${entry.id}/result`);
      if (compare) {
        setBaseline(data);
        setChart(true);
      } else {
        replace(data.project);
        setResult(data);
        setResultProject(JSON.stringify(data.project));
        setDrop(Object.keys(data.lifecycle)[0]);
        setTime(0);
        setOrdinal(0);
        setPlaying(false);
        setJob(entry);
        setFitToken((n) => n + 1);
      }
      setHistory(null);
    } catch (e) {
      setError(String(e));
    }
  }
  const seek = (value: number) => {
    setTime(value);
    setOrdinal(undefined);
    setPlaying(false);
  };
  const step = (delta: number) => {
    if (!result) return;
    const i = Math.max(
      0,
      Math.min(
        result.states.length - 1,
        (ordinal ?? stateAt(result, time)) + delta,
      ),
    );
    setTime(result.states[i].time);
    setOrdinal(i);
    setPlaying(false);
  };
  const inspect = (id: string) => {
    setDrop(id);
    setSelection([]);
    setRight(true);
  };
  const addPump = () => {
    if (project.nodes.length < 2) {
      setError(t("Adicione pelo menos dois nós.", "Add at least two nodes."));
      return;
    }
    const id = uid("p");
    edit((d) =>
      d.pumps.push({
        id,
        source:
          d.nodes.find((n) => n.sink)?.id || d.nodes[d.nodes.length - 1].id,
        target: d.nodes[0].id,
        kind: "flow",
        value: 1.5e-16,
      }),
    );
    select([{ kind: "pump", id }]);
  };
  const resize = (event: React.PointerEvent, side: "left" | "right") => {
    const start = event.clientX;
    const root = document.documentElement;
    const initial =
      parseFloat(
        getComputedStyle(root).getPropertyValue(
          side === "left" ? "--left-width" : "--right-width",
        ),
      ) || 280;
    const move = (e: PointerEvent) =>
      root.style.setProperty(
        side === "left" ? "--left-width" : "--right-width",
        `${Math.max(220, Math.min(550, initial + (e.clientX - start) * (side === "left" ? 1 : -1)))}px`,
      );
    const end = () => {
      window.removeEventListener("pointermove", move);
      window.removeEventListener("pointerup", end);
    };
    window.addEventListener("pointermove", move);
    window.addEventListener("pointerup", end);
  };
  return (
    <div className="app">
      <header className="topbar">
        <div className="brand">
          <div className="brand-mark">
            <Dna size={25} />
          </div>
          <strong>
            flui<span>DNA</span>
          </strong>
          <small>STUDIO</small>
        </div>
        <div className="project-title">
          <ChevronRight size={14} />
          <input
            aria-label={t("Nome do projeto", "Project name")}
            key={project.name}
            defaultValue={project.name}
            onBlur={(e) => {
              if (e.target.value !== project.name)
                edit((d) => {
                  d.name = e.target.value;
                });
            }}
          />
          <span
            className="draft-dot"
            title={t("Rascunho local automático", "Automatic local draft")}
          />
        </div>
        <div className="top-actions">
          <button
            title={t("Novo projeto", "New project")}
            aria-label={t("Novo projeto", "New project")}
            onClick={() => loadProject(emptyProject())}
          >
            <FilePlus2 size={17} />
          </button>
          <button onClick={() => file.current?.click()}>
            <Upload size={16} />
            <span>{t("Importar", "Import")}</span>
          </button>
          <button
            onClick={() =>
              download(`${project.name}.json`, JSON.stringify(project, null, 2))
            }
          >
            <Download size={16} />
            <span>{t("Salvar", "Save")}</span>
          </button>
          <select
            aria-label="Language"
            value={language}
            onChange={(e) => setLanguage(e.target.value as "pt" | "en")}
          >
            <option value="pt">PT</option>
            <option value="en">EN</option>
          </select>
          <button className="primary" disabled={busy} onClick={run}>
            {busy ? (
              <LoaderCircle size={16} className="spin" />
            ) : (
              <Play size={16} />
            )}{" "}
            {busy ? t("Simulando", "Simulating") : t("Simular", "Simulate")}
          </button>
        </div>
      </header>
      <input
        ref={file}
        type="file"
        accept=".json,application/json"
        hidden
        onChange={(e) => {
          if (e.target.files?.[0]) importFile(e.target.files[0]);
          e.target.value = "";
        }}
      />
      <input
        ref={reactionFile}
        type="file"
        accept=".json"
        hidden
        onChange={async (e) => {
          const input = e.currentTarget,
            f = input.files?.[0];
          if (!f) return;
          try {
            const raw = JSON.parse(await f.text());
            const imported = await api<Project>("projects/import", raw);
            edit((d) => {
              const map: Record<string, string> = {};
              for (const s of imported.species) {
                const existing = d.species.find((v) => v.name === s.name);
                const id = existing?.id || uid("s");
                map[s.id] = id;
                if (!existing) d.species.push({ ...s, id });
              }
              for (const r of imported.reactions)
                d.reactions.push({
                  ...r,
                  id: uid("r"),
                  reactants: r.reactants.map((s) => map[s]),
                  products: r.products.map((s) => map[s]),
                });
            });
          } catch (e) {
            setError(String(e));
          }
          input.value = "";
        }}
      />
      <div className="workspace">
        {left && (
          <>
            <aside className="left-panel">
              <nav className="panel-tabs">
                {[
                  {
                    id: "circuit",
                    label: t("Circuito", "Circuit"),
                    icon: GitBranch,
                  },
                  {
                    id: "chemistry",
                    label: t("Química", "Chemistry"),
                    icon: FlaskConical,
                  },
                  {
                    id: "settings",
                    label: t("Ajustes", "Settings"),
                    icon: Settings2,
                  },
                ].map((v) => (
                  <button
                    key={v.id}
                    className={panel === v.id ? "active" : ""}
                    onClick={() => setPanel(v.id)}
                  >
                    <v.icon size={16} />
                    {v.label}
                  </button>
                ))}
              </nav>
              <div className="panel-scroll">
                {panel === "circuit" && (
                  <>
                    <h3>{t("Biblioteca de circuitos", "Circuit library")}</h3>
                    <select
                      aria-label={t("Abrir exemplo", "Open example")}
                      value=""
                      onChange={async (e) => {
                        try {
                          const name = e.target.value;
                          if (name) {
                            const p = await api<Project>(
                              `examples/${encodeURIComponent(name)}`,
                            );
                            await loadProject({
                              ...p,
                              name: name.replace(".json", ""),
                            });
                          }
                        } catch (e) {
                          setError(String(e));
                        }
                      }}
                    >
                      <option value="">
                        {t("Escolha um exemplo…", "Choose an example…")}
                      </option>
                      {examples.map((n) => (
                        <option key={n}>{n}</option>
                      ))}
                    </select>
                    <div className="section-title">
                      <h3>{t("Rede microfluídica", "Microfluidic network")}</h3>
                      <span className="count">{project.nodes.length}</span>
                    </div>
                    <div className="tree">
                      {project.nodes.map((n) => (
                        <button
                          key={n.id}
                          className={
                            selection.some((s) => s.id === n.id)
                              ? "selected"
                              : ""
                          }
                          onClick={(e) =>
                            select([{ kind: "node", id: n.id }], e.shiftKey)
                          }
                        >
                          <span
                            className={`node-dot ${n.sink ? "sink" : ""}`}
                          />
                          {n.name}
                          <small>{n.sink ? "OUT" : "NODE"}</small>
                        </button>
                      ))}
                    </div>
                    <details>
                      <summary>
                        {project.channels.length} {t("canais", "channels")}
                      </summary>
                      <div className="tree">
                        {project.channels.map((c) => (
                          <button
                            key={c.id}
                            onClick={() =>
                              select([{ kind: "channel", id: c.id }])
                            }
                          >
                            <GitBranch size={13} />
                            {
                              project.nodes.find((n) => n.id === c.source)?.name
                            }{" "}
                            →{" "}
                            {project.nodes.find((n) => n.id === c.target)?.name}
                          </button>
                        ))}
                      </div>
                    </details>
                    <div className="section-title">
                      <h3>{t("Bombas", "Pumps")}</h3>
                      <button
                        aria-label={t("Adicionar bomba", "Add pump")}
                        onClick={addPump}
                      >
                        <Plus size={15} />
                      </button>
                    </div>
                    <div className="tree">
                      {project.pumps.map((p) => (
                        <button
                          key={p.id}
                          onClick={() => select([{ kind: "pump", id: p.id }])}
                        >
                          <span className="pump-dot" />
                          {project.nodes.find((n) => n.id === p.target)?.name}
                          <small>
                            {p.kind === "flow"
                              ? fmt(p.value * 1e12) + " nL/s"
                              : fmt(p.value) + " Pa"}
                          </small>
                        </button>
                      ))}
                    </div>
                    <div className="section-title">
                      <h3>{t("Injeções de gotas", "Droplet injections")}</h3>
                      <Droplets size={17} />
                    </div>
                    <p className="muted">
                      {t(
                        "Selecione um canal para adicionar uma injeção.",
                        "Select a channel to add an injection.",
                      )}
                    </p>
                    <div className="tree">
                      {project.injections.map((d) => (
                        <button
                          key={d.id}
                          className={
                            selection.some((s) => s.id === d.id)
                              ? "selected"
                              : ""
                          }
                          onClick={() =>
                            select([{ kind: "injection", id: d.id }])
                          }
                        >
                          <Droplets size={14} />
                          {d.name}
                          <small>{fmt(d.volume * 1e12)} nL</small>
                        </button>
                      ))}
                    </div>
                    {!!project.provenance.warnings?.length && (
                      <details className="import-warnings">
                        <summary>
                          <AlertTriangle size={14} />
                          {t("Avisos da migração", "Migration warnings")} (
                          {project.provenance.warnings.length})
                        </summary>
                        {project.provenance.warnings.map((w, i) => (
                          <p key={i}>{w}</p>
                        ))}
                      </details>
                    )}
                  </>
                )}
                {panel === "chemistry" && (
                  <>
                    <button onClick={() => reactionFile.current?.click()}>
                      <Upload size={14} />
                      {t("Importar CRN de circuito", "Import CRN from circuit")}
                    </button>
                    <ChemistryEditor p={project} edit={edit} t={t} />
                  </>
                )}
                {panel === "settings" && (
                  <>
                    <SettingsEditor p={project} edit={edit} t={t} />
                    <h3>{t("Resultados e cenários", "Results & scenarios")}</h3>
                    <button
                      onClick={async () => {
                        try {
                          setHistory(await api<RunEntry[]>("runs"));
                        } catch (e) {
                          setError(String(e));
                        }
                      }}
                    >
                      {t("Abrir histórico de execuções", "Open run history")}
                    </button>
                    {baseline && (
                      <button onClick={() => setBaseline(null)}>
                        {t("Remover comparação", "Remove comparison")}
                      </button>
                    )}
                  </>
                )}
              </div>
              <div className="panel-footer">
                <span className="status-dot" />
                {t("Salvo neste navegador", "Saved in this browser")}
              </div>
            </aside>
            <div
              className="resize-handle"
              role="separator"
              aria-label="Resize left panel"
              onPointerDown={(e) => resize(e, "left")}
            />
          </>
        )}
        <main>
          <div className="workspace-toolbar">
            <div className="toolbar-group">
              <button
                aria-label={t("Alternar painel esquerdo", "Toggle left panel")}
                onClick={() => setLeft((v) => !v)}
              >
                <PanelLeftClose size={17} />
              </button>
              <button
                disabled={!canUndo}
                title="Ctrl+Z"
                aria-label="Undo"
                onClick={undo}
              >
                <Undo2 size={17} />
              </button>
              <button
                disabled={!canRedo}
                title="Ctrl+Shift+Z"
                aria-label="Redo"
                onClick={redo}
              >
                <Redo2 size={17} />
              </button>
              <button
                disabled={!selection.length}
                aria-label={t("Duplicar seleção", "Duplicate selection")}
                onClick={duplicate}
              >
                <Copy size={16} />
              </button>
              <button
                disabled={!selection.length}
                aria-label={t("Excluir seleção", "Delete selection")}
                onClick={remove}
              >
                <Trash2 size={16} />
              </button>
            </div>
            <div className="view-tabs">
              <button
                className={view === "canvas" ? "active" : ""}
                onClick={() => setView("canvas")}
              >
                {t("Circuito", "Circuit")}
              </button>
              <button
                className={view === "crn" ? "active" : ""}
                onClick={() => setView("crn")}
              >
                CRN
              </button>
            </div>
            <div className="toolbar-group">
              <button
                title={t("Modo foco", "Focus mode")}
                aria-label={t("Modo foco", "Focus mode")}
                onClick={() => {
                  setLeft((v) => !v);
                  setRight((v) => !v);
                }}
              >
                <Maximize2 size={16} />
              </button>
              <button
                aria-label={t("Alternar painel direito", "Toggle right panel")}
                onClick={() => setRight((v) => !v)}
              >
                <PanelRightClose size={17} />
              </button>
            </div>
          </div>
          {error && (
            <div className="banner error" role="alert">
              <AlertTriangle size={16} />
              <span>{error}</span>
              <button onClick={() => setError("")}>×</button>
            </div>
          )}
          {stale && (
            <div className="banner warning">
              <AlertTriangle size={16} />
              {t(
                "Projeto alterado. Simule novamente para atualizar os resultados.",
                "Project changed. Run again to update results.",
              )}
              <button
                onClick={() => {
                  setResult(null);
                  setDrop("");
                }}
              >
                {t("Voltar à edição", "Return to editing")}
              </button>
            </div>
          )}
          {busy && (
            <div className="banner">
              <LoaderCircle className="spin" size={16} />
              {t(
                "Executando MMFT e DNAr em ambiente isolado…",
                "Running MMFT and DNAr in an isolated environment…",
              )}
              <button
                onClick={async () => {
                  if (job) setJob(await api<Job>(`runs/${job.id}/cancel`, {}));
                }}
              >
                {t("Cancelar", "Cancel")}
              </button>
            </div>
          )}
          <div className="workspace-view">
            {view === "canvas" ? (
              <Canvas
                project={shownProject}
                edit={edit}
                selection={selection}
                select={select}
                result={stale ? null : result}
                time={time}
                ordinal={ordinal}
                inspect={inspect}
                fitToken={fitToken}
                t={t}
              />
            ) : (
              <CRN project={project} t={t} />
            )}
          </div>
          <div className="transport">
            <div className="transport-row">
              <div className="transport-buttons">
                <button
                  disabled={!result || stale}
                  aria-label={t("Reiniciar", "Reset")}
                  onClick={() => {
                    setTime(0);
                    setOrdinal(0);
                    setPlaying(false);
                  }}
                >
                  <Square size={14} />
                </button>
                <button
                  disabled={!result || stale}
                  aria-label={t("Estado anterior", "Previous state")}
                  onClick={() => step(-1)}
                >
                  <SkipBack size={17} />
                </button>
                <button
                  className="play-button"
                  disabled={!result || stale}
                  aria-label={playing ? "Pause" : "Play"}
                  onClick={() => {
                    if (time >= duration) {
                      setTime(0);
                      setOrdinal(0);
                    }
                    setPlaying((v) => !v);
                  }}
                >
                  {playing ? <Pause size={20} /> : <Play size={20} />}
                </button>
                <button
                  disabled={!result || stale}
                  aria-label={t("Próximo estado", "Next state")}
                  onClick={() => step(1)}
                >
                  <SkipForward size={17} />
                </button>
                <button
                  aria-label="Loop"
                  className={loop ? "active" : ""}
                  onClick={() => setLoop((v) => !v)}
                >
                  <Repeat2 size={16} />
                </button>
              </div>
              <div className="time-display">
                <strong>{fmt(time)}</strong>
                <span>/ {fmt(duration)} s</span>
              </div>
              <select
                aria-label={t("Velocidade de reprodução", "Playback speed")}
                value={speed}
                onChange={(e) => setSpeed(+e.target.value)}
              >
                {[0.25, 1, 10, 100, 500, 1000].map((v) => (
                  <option key={v} value={v}>
                    {v}×
                  </option>
                ))}
              </select>
              <button disabled={!result} onClick={() => setChart((v) => !v)}>
                {t("Gráficos", "Charts")}
              </button>
              {result && (
                <button
                  onClick={() =>
                    download("fluidna-result.json", JSON.stringify(result))
                  }
                >
                  JSON
                </button>
              )}
            </div>
            <div className="timeline">
              <input
                aria-label={t("Tempo de simulação", "Simulation time")}
                type="range"
                min={0}
                max={duration || 1}
                step="any"
                disabled={!result || stale}
                value={time}
                onChange={(e) => seek(+e.target.value)}
              />
              <div className="event-markers">
                {result?.events
                  .filter((e) => e.type === "merge")
                  .map((e, i) => (
                    <button
                      key={i}
                      title={`${e.parents.join(" + ")} → ${e.droplet} · ${fmt(e.time)} s`}
                      style={{
                        left: `${duration ? (e.time / duration) * 100 : 0}%`,
                      }}
                      onClick={() => {
                        setTime(e.time);
                        setOrdinal(e.state);
                        setPlaying(false);
                        inspect(e.droplet);
                      }}
                    />
                  ))}
              </div>
            </div>
            <div className="timeline-labels">
              <span>0 s</span>
              <small>
                {result
                  ? `${result.states.length} ${t("estados", "states")} · ${result.events.filter((e) => e.type === "merge").length} merges`
                  : t(
                      "Simule um circuito para explorar a trajetória e a química",
                      "Simulate a circuit to explore its path and chemistry",
                    )}
              </small>
              <span>{fmt(duration)} s</span>
            </div>
          </div>
          {result && chart && !stale && (
            <Charts
              result={result}
              time={time}
              ordinal={ordinal}
              drop={drop || Object.keys(result.lifecycle)[0]}
              seek={seek}
              t={t}
              baseline={baseline}
            />
          )}
        </main>
        {right && (
          <>
            <div
              className="resize-handle"
              role="separator"
              aria-label="Resize right panel"
              onPointerDown={(e) => resize(e, "right")}
            />
            <aside className="right-panel">
              <div className="inspector-heading">
                <span>
                  {drop && result
                    ? t("GOTA · QUÍMICA", "DROPLET · CHEMISTRY")
                    : t("PROPRIEDADES", "PROPERTIES")}
                </span>
                <Settings2 size={15} />
              </div>
              <div className="panel-scroll">
                {result && <label className="field"><span>{t('Inspecionar gota','Inspect droplet')}</span><select aria-label={t('Inspecionar gota','Inspect droplet')} value={drop} onChange={e=>inspect(e.target.value)}><option value="">—</option>{Object.keys(result.lifecycle).map(id=><option key={id} value={id}>{result.project.injections.find(d=>d.id===id)?.name||id}</option>)}</select></label>}
                {drop && result ? (
                  <DropInspector
                    id={drop}
                    result={result}
                    time={time}
                    edit={edit}
                    t={t}
                  />
                ) : (
                  <Inspector
                    p={project}
                    edit={edit}
                    selection={selection}
                    remove={remove}
                    t={t}
                  />
                )}
                {result && (
                  <>
                    <h3>{t("Eventos e linhagem", "Events & lineage")}</h3>
                    <div className="events-list">
                      {result.events.map((e, i) => (
                        <button
                          key={i}
                          onClick={() => {
                            setTime(e.time);
                            setOrdinal(e.state);
                            setPlaying(false);
                            inspect(e.droplet);
                          }}
                        >
                          <span className={`event-dot ${e.type}`} />
                          <span>
                            <strong>
                              {e.type === "merge"
                                ? e.parents.join(" + ") + " → "
                                : ""}
                              {e.droplet}
                            </strong>
                            <small>
                              {e.type} · {fmt(e.time)} s · #{e.state}
                            </small>
                          </span>
                        </button>
                      ))}
                    </div>
                    {result.warnings.map((w) => (
                      <p className="warning" key={w}>
                        {w}
                      </p>
                    ))}
                  </>
                )}
                {job && (
                  <button
                    onClick={async () => {
                      try {
                        const v = await api<{ log: string }>(
                          `runs/${job.id}/log`,
                        );
                        setRunLog(v.log);
                        setShowLog(true);
                      } catch (e) {
                        setError(String(e));
                      }
                    }}
                  >
                    {t("Abrir log da execução", "Open run log")}
                  </button>
                )}
              </div>
            </aside>
          </>
        )}
      </div>
      <footer className="statusbar">
        <span>
          <CheckCircle2 size={12} />
          {notice || t("Pronto para explorar", "Ready to explore")}
        </span>
        <span>
          MMFT + DNAr <b>·</b>{" "}
          {project.settings.mixing === "physical"
            ? t("Diluição física", "Physical dilution")
            : t("Soma legada", "Legacy sum")}{" "}
          <b>·</b> nM / s
        </span>
      </footer>
      {showLog && (
        <div className="modal-backdrop">
          <section className="log-modal">
            <div className="section-title">
              <h3>{t("Log da execução", "Run log")}</h3>
              <button onClick={() => setShowLog(false)}>×</button>
            </div>
            <pre>{runLog}</pre>
            <button
              onClick={() => download("worker.log", runLog, "text/plain")}
            >
              {t("Baixar log", "Download log")}
            </button>
          </section>
        </div>
      )}
      {history && (
        <div className="modal-backdrop">
          <section
            className="log-modal"
            role="dialog"
            aria-label={t("Histórico de execuções", "Run history")}
          >
            <div className="section-title">
              <h3>{t("Histórico de execuções", "Run history")}</h3>
              <button
                aria-label={t("Fechar histórico", "Close history")}
                onClick={() => setHistory(null)}
              >
                ×
              </button>
            </div>
            {history.map((entry) => (
              <div key={entry.id} className="history-row">
                <div>
                  <strong>{entry.name}</strong>
                  <small>
                    {entry.created} UTC · {entry.mixing} · {entry.status}
                  </small>
                </div>
                <button
                  disabled={entry.status !== "completed"}
                  onClick={() => restoreRun(entry)}
                >
                  {t("Abrir", "Open")}
                </button>
                <button
                  disabled={entry.status !== "completed" || !result}
                  onClick={() => restoreRun(entry, true)}
                >
                  {t("Comparar", "Compare")}
                </button>
              </div>
            ))}
          </section>
        </div>
      )}
    </div>
  );
}
