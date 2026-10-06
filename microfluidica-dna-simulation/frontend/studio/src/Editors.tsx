import { useState, type ReactNode } from "react";
import { Plus, Trash2, Copy, ArrowLeftRight } from "lucide-react";
import { type Project, type Selection, type Result, uid, fmt } from "./types";
import { concentrationsAt } from "./playback";
export type Edit = (fn: (p: Project) => void) => void;
export type Translate = (pt: string, en: string) => string;
export function Field({
  label,
  value,
  onChange,
  unit,
  type = "number",
}: {
  label: string;
  value: string | number;
  onChange: (v: string) => void;
  unit?: string;
  type?: string;
}) {
  return (
    <label className="field">
      <span>
        {label}
        {unit && <small>{unit}</small>}
      </span>
      <input
        key={`${label}-${value}`}
        aria-label={label}
        type={type}
        step="any"
        defaultValue={value}
        onBlur={(e) => {
          if (
            e.target.value !== String(value) &&
            (type !== "number" || Number.isFinite(Number(e.target.value)))
          )
            onChange(e.target.value);
        }}
        onKeyDown={(e) => {
          if (e.key === "Enter") e.currentTarget.blur();
        }}
      />
    </label>
  );
}
export function Select({
  label,
  value,
  onChange,
  children,
}: {
  label: string;
  value: string;
  onChange: (v: string) => void;
  children: ReactNode;
}) {
  return (
    <label className="field">
      <span>{label}</span>
      <select
        aria-label={label}
        value={value}
        onChange={(e) => onChange(e.target.value)}
      >
        {children}
      </select>
    </label>
  );
}
export function Inspector({
  p,
  edit,
  selection,
  remove,
  t,
}: {
  p: Project;
  edit: Edit;
  selection: Selection[];
  remove: () => void;
  t: Translate;
}) {
  const s = selection[0];
  if (!s)
    return (
      <div className="empty-inspector">
        <h3>{t("Explore o circuito", "Explore the circuit")}</h3>
        <p>
          {t(
            "Selecione um nó, canal, bomba ou gota para editar suas propriedades.",
            "Select a node, channel, pump or droplet to edit its properties.",
          )}
        </p>
        <p>
          {t(
            "As coordenadas alteram a geometria física. Pan e zoom alteram apenas a visualização.",
            "Coordinates change physical geometry. Pan and zoom only change the view.",
          )}
        </p>
      </div>
    );
  const n = p.nodes.find((n) => n.id === s.id),
    c = p.channels.find((c) => c.id === s.id),
    d = p.injections.find((d) => d.id === s.id),
    pump = p.pumps.find((v) => v.id === s.id),
    note = p.annotations.find((v) => v.id === s.id);
  const patch = (key: string, value: unknown) =>
    edit((draft) => {
      const list =
        s.kind === "node"
          ? draft.nodes
          : s.kind === "channel"
            ? draft.channels
            : s.kind === "injection"
              ? draft.injections
              : s.kind === "pump"
                ? draft.pumps
                : draft.annotations;
      const item = list.find((v) => v.id === s.id);
      if (item) Object.assign(item, { [key]: value });
    });
  return (
    <>
      <div className="section-title">
        <h3>
          {selection.length > 1
            ? `${selection.length} ${t("selecionados", "selected")}`
            : s.id}
        </h3>
        <button
          className="danger"
          aria-label={t("Excluir seleção", "Delete selection")}
          onClick={remove}
        >
          <Trash2 size={16} />
        </button>
      </div>
      {n && (
        <>
          <Field
            label={t("Nome", "Name")}
            value={n.name}
            type="text"
            onChange={(v) => patch("name", v)}
          />
          <div className="two">
            <Field
              label="X"
              unit="µm"
              value={n.x * 1e6}
              onChange={(v) => patch("x", +v / 1e6)}
            />
            <Field
              label="Y"
              unit="µm"
              value={n.y * 1e6}
              onChange={(v) => patch("y", +v / 1e6)}
            />
          </div>
          <label className="check">
            <input
              type="checkbox"
              checked={n.sink}
              onChange={(e) => {
                patch("sink", e.target.checked);
                if (e.target.checked) patch("ground", true);
              }}
            />
            {t("Saída / sink", "Outlet / sink")}
          </label>
          <label className="check">
            <input
              type="checkbox"
              checked={n.ground}
              onChange={(e) => patch("ground", e.target.checked)}
            />
            {t("Pressão de referência", "Ground pressure")}
          </label>
        </>
      )}
      {(c || pump) && (
        <>
          <Select
            label={t("Origem", "Source")}
            value={(c || pump)!.source}
            onChange={(v) => patch("source", v)}
          >
            {p.nodes.map((n) => (
              <option key={n.id} value={n.id}>
                {n.name}
              </option>
            ))}
          </Select>
          <Select
            label={t("Destino", "Target")}
            value={(c || pump)!.target}
            onChange={(v) => patch("target", v)}
          >
            {p.nodes.map((n) => (
              <option key={n.id} value={n.id}>
                {n.name}
              </option>
            ))}
          </Select>
          <button
            onClick={() => {
              const item = (c || pump)!;
              edit((draft) => {
                const v = (c ? draft.channels : draft.pumps).find(
                  (v) => v.id === item.id,
                )!;
                [v.source, v.target] = [v.target, v.source];
                if (c)
                  for (const inj of draft.injections)
                    if (inj.channel === c.id) inj.position = 1 - inj.position;
              });
            }}
          >
            <ArrowLeftRight size={15} />
            {t("Inverter orientação", "Reverse orientation")}
          </button>
        </>
      )}
      {c && (
        <>
          <div className="two">
            <Field
              label={t("Largura", "Width")}
              unit="µm"
              value={c.width * 1e6}
              onChange={(v) => patch("width", +v / 1e6)}
            />
            <Field
              label={t("Altura", "Height")}
              unit="µm"
              value={c.height * 1e6}
              onChange={(v) => patch("height", +v / 1e6)}
            />
          </div>
          <button
            className="primary"
            onClick={() =>
              edit((draft) =>
                draft.injections.push({
                  id: uid("d"),
                  name: `Drop ${draft.injections.length + 1}`,
                  channel: c.id,
                  time: 0.1,
                  position: 0.25,
                  volume: 2.25e-13,
                  concentrations: {},
                }),
              )
            }
          >
            <Plus size={15} />
            {t("Injetar gota neste canal", "Inject droplet in channel")}
          </button>
        </>
      )}
      {pump && (
        <>
          <Select
            label={t("Tipo de bomba", "Pump type")}
            value={pump.kind}
            onChange={(v) => patch("kind", v)}
          >
            <option value="flow">{t("Vazão", "Flow rate")}</option>
            <option value="pressure">{t("Pressão", "Pressure")}</option>
          </Select>
          <Field
            label={t("Valor", "Value")}
            unit={pump.kind === "flow" ? "nL/s" : "Pa"}
            value={pump.value * (pump.kind === "flow" ? 1e12 : 1)}
            onChange={(v) =>
              patch("value", +v / (pump.kind === "flow" ? 1e12 : 1))
            }
          />
        </>
      )}
      {d && (
        <>
          <Field
            label={t("Nome", "Name")}
            type="text"
            value={d.name}
            onChange={(v) => patch("name", v)}
          />
          <Select
            label={t("Canal", "Channel")}
            value={d.channel}
            onChange={(v) => patch("channel", v)}
          >
            {p.channels.map((c) => (
              <option key={c.id} value={c.id}>
                {p.nodes.find((n) => n.id === c.source)?.name} →{" "}
                {p.nodes.find((n) => n.id === c.target)?.name}
              </option>
            ))}
          </Select>
          <div className="two">
            <Field
              label={t("Injeção", "Injection")}
              unit="s"
              value={d.time}
              onChange={(v) => patch("time", +v)}
            />
            <Field
              label={t("Volume", "Volume")}
              unit="nL"
              value={d.volume * 1e12}
              onChange={(v) => patch("volume", +v / 1e12)}
            />
          </div>
          <Field
            label={t("Posição relativa", "Relative position")}
            unit="0–1"
            value={d.position}
            onChange={(v) => patch("position", +v)}
          />
          <button
            onClick={() =>
              edit((draft) =>
                draft.injections.push({
                  ...structuredClone(d),
                  id: uid("d"),
                  name: d.name + " copy",
                  time: d.time + 1,
                }),
              )
            }
          >
            <Copy size={15} />
            {t("Repetir injeção (+1 s)", "Repeat injection (+1 s)")}
          </button>
          <h4>{t("Composição inicial", "Initial composition")} · nM</h4>
          {p.species.map((species) => (
            <Field
              key={species.id}
              label={species.name}
              value={d.concentrations[species.id] || 0}
              onChange={(v) =>
                patch("concentrations", {
                  ...d.concentrations,
                  [species.id]: +v,
                })
              }
            />
          ))}
          {!p.species.length && (
            <p>
              {t(
                "Adicione espécies no menu Química.",
                "Add species in the Chemistry menu.",
              )}
            </p>
          )}
        </>
      )}
      {note && (
        <Field
          label={t("Texto", "Text")}
          type="text"
          value={note.text}
          onChange={(v) => patch("text", v)}
        />
      )}
    </>
  );
}

export function ChemistryEditor({
  p,
  edit,
  t,
}: {
  p: Project;
  edit: Edit;
  t: Translate;
}) {
  const [search, setSearch] = useState(""),
    [batch, setBatch] = useState(""),
    [error, setError] = useState("");
  const label = (ids: string[]) =>
    ids.map((id) => p.species.find((s) => s.id === id)?.name || id).join(" + ");
  function parse(text: string) {
    return text
      .split("+")
      .map((s) => s.trim())
      .filter(Boolean)
      .flatMap((name) => {
        const match = name.match(/^(\d+)\s+(.+)$/);
        const count = match ? +match[1] : 1;
        const target = match ? match[2] : name;
        const species = p.species.find((s) => s.name === target);
        if (!species || count < 1 || count > 100)
          throw Error(`Unknown species / coefficient: ${name}`);
        return Array(count).fill(species.id) as string[];
      });
  }
  return (
    <>
      <div className="section-title">
        <h3>
          {t("Espécies", "Species")} <em>{p.species.length}</em>
        </h3>
        <button
          aria-label={t("Adicionar espécie", "Add species")}
          onClick={() =>
            edit((d) => {
              let i = d.species.length + 1;
              while (d.species.some((s) => s.name === `S${i}`)) i++;
              d.species.push({
                id: uid("s"),
                name: `S${i}`,
                color: "#2dd4bf",
                visible: true,
              });
            })
          }
        >
          <Plus size={16} />
        </button>
      </div>
      <input
        className="search"
        placeholder={t("Buscar espécie…", "Search species…")}
        value={search}
        onChange={(e) => setSearch(e.target.value)}
      />
      {p.species
        .filter((s) => s.name.toLowerCase().includes(search.toLowerCase()))
        .map((s) => (
          <div className="species-editor" key={s.id}>
            <input
              aria-label={`Color ${s.name}`}
              type="color"
              value={s.color}
              onChange={(e) =>
                edit((d) => {
                  d.species.find((v) => v.id === s.id)!.color = e.target.value;
                })
              }
            />
            <input
              key={s.name}
              aria-label={`Name ${s.name}`}
              defaultValue={s.name}
              onBlur={(e) => {
                if (e.target.value !== s.name)
                  edit((d) => {
                    d.species.find((v) => v.id === s.id)!.name = e.target.value;
                  });
              }}
            />
            <input
              aria-label={`Show ${s.name}`}
              title={t("Mostrar nos gráficos", "Show in charts")}
              type="checkbox"
              checked={s.visible}
              onChange={(e) =>
                edit((d) => {
                  d.species.find((v) => v.id === s.id)!.visible =
                    e.target.checked;
                })
              }
            />
            <button
              aria-label={`Delete ${s.name}`}
              onClick={() =>
                edit((d) => {
                  d.species = d.species.filter((v) => v.id !== s.id);
                  d.reactions = d.reactions.filter(
                    (r) => ![...r.reactants, ...r.products].includes(s.id),
                  );
                  d.injections.forEach((v) => delete v.concentrations[s.id]);
                })
              }
            >
              <Trash2 size={13} />
            </button>
          </div>
        ))}
      <h3>
        {t("Reações", "Reactions")} <em>{p.reactions.length}</em>
      </h3>
      <p className="muted">
        {t(
          "Concentrações em nM. k em nM^(1−ordem)/s. Repetir espécies preserva a estequiometria.",
          "Concentrations in nM. k in nM^(1−order)/s. Repeated species preserve stoichiometry.",
        )}
      </p>
      {p.reactions.map((r) => (
        <div className="reaction-card" key={r.id}>
          <div className="section-title">
            <strong>{r.id}</strong>
            <button
              aria-label={`Delete ${r.id}`}
              onClick={() =>
                edit((d) => {
                  d.reactions = d.reactions.filter((v) => v.id !== r.id);
                })
              }
            >
              <Trash2 size={13} />
            </button>
          </div>
          <Field
            label={t("Reagentes", "Reactants")}
            type="text"
            value={label(r.reactants)}
            onChange={(text) => {
              try {
                const reactants = parse(text);
                edit((d) => {
                  d.reactions.find((v) => v.id === r.id)!.reactants = reactants;
                });
                setError("");
              } catch (e) {
                setError(String(e));
              }
            }}
          />
          <Field
            label={t("Produtos", "Products")}
            type="text"
            value={label(r.products)}
            onChange={(text) => {
              try {
                const products = parse(text);
                edit((d) => {
                  d.reactions.find((v) => v.id === r.id)!.products = products;
                });
                setError("");
              } catch (e) {
                setError(String(e));
              }
            }}
          />
          <Field
            label="k"
            unit={
              r.reactants.length === 1
                ? "s⁻¹"
                : `nM^${1 - r.reactants.length}/s`
            }
            value={r.rate}
            onChange={(v) =>
              edit((d) => {
                d.reactions.find((v) => v.id === r.id)!.rate = +v;
              })
            }
          />
          <label className="check">
            <input
              type="checkbox"
              checked={r.reverse_rate !== null}
              onChange={(e) =>
                edit((d) => {
                  d.reactions.find((v) => v.id === r.id)!.reverse_rate = e
                    .target.checked
                    ? 0.0028
                    : null;
                })
              }
            />
            {t("Reversível", "Reversible")}
          </label>
          {r.reverse_rate !== null && (
            <Field
              label="k reverse"
              unit={`nM^${1 - r.products.length}/s`}
              value={r.reverse_rate}
              onChange={(v) =>
                edit((d) => {
                  d.reactions.find((v) => v.id === r.id)!.reverse_rate = +v;
                })
              }
            />
          )}
        </div>
      ))}
      <h4>{t("Adicionar reações em lote", "Batch-add reactions")}</h4>
      <textarea
        aria-label={t("Reações em lote", "Batch reactions")}
        placeholder={"A + B -> C ; 0.0028\n2 A -> B ; 0.001"}
        value={batch}
        onChange={(e) => setBatch(e.target.value)}
      />
      <button
        onClick={() => {
          try {
            const reactions = batch
              .split("\n")
              .filter((l) => l.trim())
              .map((line) => {
                const [expression, k] = line.split(";");
                const sides = expression.split("->");
                if (
                  sides.length !== 2 ||
                  k === undefined ||
                  !Number.isFinite(+k) ||
                  +k < 0
                )
                  throw Error("Use A + B -> C ; 0.0028");
                return {
                  id: uid("r"),
                  reactants: parse(sides[0]),
                  products: parse(sides[1]),
                  rate: +k,
                  reverse_rate: null,
                };
              });
            edit((d) => d.reactions.push(...reactions));
            setBatch("");
            setError("");
          } catch (e) {
            setError(String(e));
          }
        }}
      >
        <Plus size={15} />
        {t("Adicionar", "Add")}
      </button>
      {error && (
        <p className="error" role="alert">
          {error}
        </p>
      )}
    </>
  );
}

export function SettingsEditor({
  p,
  edit,
  t,
}: {
  p: Project;
  edit: Edit;
  t: Translate;
}) {
  return (
    <>
      <h3>{t("Modelo físico", "Physical model")}</h3>
      <p className="muted">MMFT · AbstractDroplet · f6e155442</p>
      <Select
        label={t("Mistura química", "Chemical mixing")}
        value={p.settings.mixing}
        onChange={(v) =>
          edit((d) => {
            d.settings.mixing = v as Project["settings"]["mixing"];
          })
        }
      >
        <option value="physical">
          {t("Diluição por volume (físico)", "Volume dilution (physical)")}
        </option>
        <option value="legacy_sum">
          {t(
            "Soma legada (não conserva matéria)",
            "Legacy sum (not mass-conserving)",
          )}
        </option>
      </Select>
      {p.settings.mixing === "legacy_sum" && (
        <p className="warning">
          {t(
            "Apenas comparação histórica. Soma concentrações, não quantidades.",
            "Historical comparison only. Adds concentrations, not amounts.",
          )}
        </p>
      )}
      {(["continuous", "dispersed"] as const).map((phase) => (
        <div key={phase}>
          <h4>
            {phase === "continuous"
              ? t("Fase contínua", "Continuous phase")
              : t("Fase dispersa", "Dispersed phase")}
          </h4>
          <Field
            label={`${t("Densidade", "Density")} · ${phase}`}
            unit="kg/m³"
            value={p.settings[phase].density}
            onChange={(v) =>
              edit((d) => {
                d.settings[phase].density = +v;
              })
            }
          />
          <Field
            label={`${t("Viscosidade", "Viscosity")} · ${phase}`}
            unit="Pa·s"
            value={p.settings[phase].viscosity}
            onChange={(v) =>
              edit((d) => {
                d.settings[phase].viscosity = +v;
              })
            }
          />
        </div>
      ))}
      <h3>{t("Integração DNAr", "DNAr integration")}</h3>
      <Select
        label={t("Motor", "Engine")}
        value={p.settings.engine}
        onChange={(v) => edit((d) => { d.settings.engine = v as Project["settings"]["engine"]; })}
      >
        <option value="desolve">deSolve</option>
        <option value="diffeqr" disabled={p.settings.stochastic}>diffeqR</option>
      </Select>
      <Select
        label={t("Estocástico", "Stochastic")}
        value={String(p.settings.stochastic)}
        onChange={(v) => edit((d) => { d.settings.stochastic = v === "true"; })}
      >
        <option value="false">{t("Não", "No")}</option>
        <option value="true">{t("Sim", "Yes")}</option>
      </Select>
      <Select
        label={t("DNA 4-domínios", "DNA 4-domain")}
        value={String(p.settings.dna)}
        onChange={(v) => edit((d) => { d.settings.dna = v === "true"; })}
      >
        <option value="false">{t("Não", "No")}</option>
        <option value="true">{t("Sim", "Yes")}</option>
      </Select>
      <Field
        label={t("Volume estocástico", "Stochastic volume")}
        value={p.settings.volume}
        onChange={(v) => edit((d) => { d.settings.volume = +v; })}
      />
      <Field
        label={t("Seed aleatória", "Random seed")}
        value={p.settings.seed ?? ""}
        onChange={(v) => edit((d) => { d.settings.seed = v === "" ? null : +v; })}
      />
      <Select
        label={t("Forçamento", "Forced concentration")}
        value={p.settings.forcing?.name || "none"}
        onChange={(v) => edit((d) => {
          d.settings.forcing = v === "none" ? null : {
            name: v,
            species: d.settings.forcing?.species || d.species[0]?.id || "",
            params: d.settings.forcing?.params || {},
          };
        })}
      >
        <option value="none">None</option>
        <option value="step_input">Step</option>
        <option value="pulse_input">Pulse</option>
        <option value="sinusoidal_input">Sinusoidal</option>
        <option value="saw_wave_input">Saw wave</option>
        <option value="square_input">Square wave</option>
      </Select>
      {p.settings.forcing && (
        <>
          <Select
            label={t("Espécie forçada", "Forced species")}
            value={p.settings.forcing.species}
            onChange={(v) => edit((d) => { if (d.settings.forcing) d.settings.forcing.species = v; })}
          >
            {p.species.map((s) => <option key={s.id} value={s.id}>{s.name}</option>)}
          </Select>
          <Field
            label={t("Amplitude", "Amplitude")}
            value={p.settings.forcing.params.amplitude ?? ""}
            onChange={(v) => edit((d) => { if (d.settings.forcing) d.settings.forcing.params.amplitude = +v; })}
          />
          <Field
            label={t("Tempo", "Time")}
            value={p.settings.forcing.params.time ?? ""}
            onChange={(v) => edit((d) => { if (d.settings.forcing) d.settings.forcing.params.time = +v; })}
          />
        </>
      )}
      <Select
        label={t("Timing automático", "Automatic timing")}
        value={String(p.settings.timing_enabled)}
        onChange={(v) => edit((d) => { d.settings.timing_enabled = v === "true"; })}
      >
        <option value="false">{t("Não", "No")}</option>
        <option value="true">{t("Sim", "Yes")}</option>
      </Select>
      <Field
        label={t("Tolerância de estabilidade", "Settling tolerance")}
        unit="%"
        value={p.settings.timing_tolerance_percent}
        onChange={(v) => edit((d) => { d.settings.timing_tolerance_percent = +v; })}
      />
      <Field
        label={t("Amostras estáveis", "Stable samples")}
        value={p.settings.timing_stable_samples}
        onChange={(v) => edit((d) => { d.settings.timing_stable_samples = +v; })}
      />
      <Field
        label={t("Amostras por gota", "Samples per droplet")}
        value={p.settings.sample_count}
        onChange={(v) =>
          edit((d) => {
            d.settings.sample_count = +v;
          })
        }
      />
      <Field
        label="rtol"
        value={p.settings.rtol}
        onChange={(v) =>
          edit((d) => {
            d.settings.rtol = +v;
          })
        }
      />
      <Field
        label="atol"
        unit="nM"
        value={p.settings.atol}
        onChange={(v) =>
          edit((d) => {
            d.settings.atol = +v;
          })
        }
      />
      <Field
        label="Timeout"
        unit="s"
        value={p.settings.timeout}
        onChange={(v) =>
          edit((d) => {
            d.settings.timeout = +v;
          })
        }
      />
      <p className="muted">
        {t(
          "A reação começa na injeção e termina no merge ou na saída. A interpolação visual é limitada pela resolução das amostras.",
          "Reaction starts at injection and ends at merge or exit. Visual interpolation is limited by sample resolution.",
        )}
      </p>
    </>
  );
}

export function DropInspector({
  id,
  result,
  time,
  edit,
  t,
}: {
  id: string;
  result: Result;
  time: number;
  edit: Edit;
  t: Translate;
}) {
  const life = result.lifecycle[id],
    p = result.project;
  const [target, setTarget] = useState(p.injections[0]?.id || "");
  const sampleTime = Math.max(life.birth_time, Math.min(life.death_time, time));
  const { values, interpolated } = concentrationsAt(
    result.chemistry[id],
    sampleTime,
  );
  return (
    <>
      <h3>{id}</h3>
      <div className="badge">
        {sampleTime !== time
          ? t("Fora do circuito neste instante", "Outside circuit at this time")
          : interpolated
            ? t("Interpolado", "Interpolated")
            : t("Amostra calculada", "Computed sample")}
      </div>
      <p className="muted">
        {fmt(sampleTime)} s · {life.terminal}
        <br />
        {t("Pais", "Parents")}: {life.parents.join(" + ") || "—"}
        <br />
        {fmt(life.birth_time)} → {fmt(life.death_time)} s
      </p>
      <h4>{t("Espécies nesta gota", "Species in this droplet")}</h4>
      <div className="concentrations">
        {p.species.map((s) => (
          <div key={s.id}>
            <span>
              <i style={{ background: s.color }} />
              {s.name}
            </span>
            <b>
              {fmt(values[s.id] || 0)} <small>nM</small>
            </b>
          </div>
        ))}
      </div>
      <h4>{t("Reciclar composição", "Recycle composition")}</h4>
      <Select
        label={t("Injeção de destino", "Target injection")}
        value={target}
        onChange={setTarget}
      >
        {p.injections.map((d) => (
          <option key={d.id} value={d.id}>
            {d.name}
          </option>
        ))}
      </Select>
      <button
        disabled={!target}
        onClick={() =>
          edit((d) => {
            const injection = d.injections.find((v) => v.id === target);
            if (injection) injection.concentrations = { ...values };
          })
        }
      >
        {t("Usar composição deste instante", "Use composition at this time")}
      </button>
      <p className="muted">
        {t(
          "Cria uma edição no projeto; não altera este resultado.",
          "Edits the project; does not change this result.",
        )}
      </p>
    </>
  );
}
