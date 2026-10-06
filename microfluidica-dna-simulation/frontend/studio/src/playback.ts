import type { Result, Drop, Row } from "./types";

// Upper-bound search preserves equal-time event ordinals. Explicit state stepping
// overrides the time lookup, so a zero-time binary cascade remains inspectable.
export function stateAt(result: Result, time: number): number {
  let lo = 0,
    hi = result.states.length;
  while (lo < hi) {
    const mid = (lo + hi) >>> 1;
    if (result.states[mid].time <= time) lo = mid + 1;
    else hi = mid;
  }
  return Math.max(0, lo - 1);
}
export function concentrationsAt(
  rows: Row[],
  time: number,
): { values: Record<string, number>; interpolated: boolean } {
  if (!rows?.length) return { values: {}, interpolated: false };
  let lo = 0,
    hi = rows.length;
  while (lo < hi) {
    const mid = (lo + hi) >>> 1;
    if (rows[mid].time <= time) lo = mid + 1;
    else hi = mid;
  }
  const a = rows[Math.max(0, lo - 1)],
    b = rows[Math.min(rows.length - 1, lo)];
  const fraction =
    b.time > a.time
      ? Math.max(0, Math.min(1, (time - a.time) / (b.time - a.time)))
      : 0;
  return {
    values: Object.fromEntries(
      Object.keys(a)
        .filter((k) => k !== "time")
        .map((k) => [k, Math.max(0, a[k] + fraction * (b[k] - a[k]))]),
    ),
    interpolated: fraction > 0 && fraction < 1,
  };
}
export function visibleDrops(
  result: Result,
  time: number,
  ordinal?: number,
): Record<string, Drop> {
  const index = ordinal ?? stateAt(result, time),
    current = result.states[index],
    next = result.states[index + 1];
  const fraction =
    next && next.time > current.time
      ? Math.max(
          0,
          Math.min(1, (time - current.time) / (next.time - current.time)),
        )
      : 0;
  const drops: Record<string, Drop> = {};
  for (const [id, life] of Object.entries(result.lifecycle)) {
    if (index < life.birth || index >= life.death) continue;
    const a = current.droplets[id],
      b = next?.droplets[id];
    if (!a) continue;
    const boundaries = a.boundaries.map((edge) => {
      const candidates =
        b?.boundaries.filter(
          (v) =>
            v.channel === edge.channel && v.towardSource === edge.towardSource,
        ) || [];
      const sourceCount = a.boundaries.filter(
        (v) =>
          v.channel === edge.channel && v.towardSource === edge.towardSource,
      ).length;
      // A boundary crossing a node must not be interpolated as a shortcut in XY.
      // Move to the endpoint in its original channel; topology switches at event.
      let destination = edge.position;
      if (candidates.length === 1 && sourceCount === 1)
        destination = candidates[0].position;
      else if (candidates.length === 0 && b) {
        const flow = current.flows[edge.channel] || 0;
        destination = flow > 0 ? 1 : flow < 0 ? 0 : edge.position;
      }
      return {
        ...edge,
        position: edge.position + fraction * (destination - edge.position),
      };
    });
    drops[id] = { ...a, boundaries };
  }
  return drops;
}
// Complete occupied intervals, including fully occupied channels and branching
// droplets. Rendering the last boundary as a circle loses both volume and shape.
export function occupied(
  drop: Drop,
): { channel: string; start: number; end: number }[] {
  const intervals = drop.channels.map((channel) => ({
    channel,
    start: 0,
    end: 1,
  }));
  const grouped: Record<string, typeof drop.boundaries> = {};
  for (const b of drop.boundaries) (grouped[b.channel] ??= []).push(b);
  for (const [channel, bounds] of Object.entries(grouped)) {
    const sorted = [...bounds].sort((a, b) => a.position - b.position);
    let start: number | null = sorted[0]?.towardSource ? 0 : null;
    for (const b of sorted) {
      if (b.towardSource) {
        intervals.push({ channel, start: start ?? 0, end: b.position });
        start = null;
      } else start = b.position;
    }
    if (start !== null) intervals.push({ channel, start, end: 1 });
  }
  return intervals;
}
