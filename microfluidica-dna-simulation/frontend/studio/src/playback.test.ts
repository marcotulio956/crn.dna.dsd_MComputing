import { describe, it, expect } from "vitest";
import { concentrationsAt, occupied, stateAt, visibleDrops } from "./playback";
import type { Result } from "./types";
const drop = {
  volume: 1,
  boundaries: [
    { channel: "c", position: 0.1, towardSource: false },
    { channel: "c", position: 0.3, towardSource: true },
  ],
  channels: [],
};
const result = {
  states: [
    { time: 0, droplets: { a: drop }, flows: { c: 1 } },
    { time: 1, droplets: { a: drop, b: drop }, flows: { c: 1 } },
    { time: 1, droplets: { a: drop, b: drop, c: drop }, flows: { c: 1 } },
    { time: 2, droplets: { a: drop, b: drop, c: drop }, flows: { c: 1 } },
  ],
  lifecycle: {
    a: { birth: 0, death: 1 },
    b: { birth: 1, death: 2 },
    c: { birth: 2, death: 3 },
  },
} as unknown as Result;
describe("one simulation clock", () => {
  it("preserves equal-time ordinal events", () => {
    expect(stateAt(result, 1)).toBe(2);
    expect(Object.keys(visibleDrops(result, 1, 1))).toEqual(["b"]);
    expect(Object.keys(visibleDrops(result, 1))).toEqual(["c"]);
  });
  it("seek backwards restores parents, not consumed ghosts", () => {
    expect(Object.keys(visibleDrops(result, 1.5))).toEqual(["c"]);
    expect(Object.keys(visibleDrops(result, 0.5))).toEqual(["a"]);
    expect(Object.keys(visibleDrops(result, 2))).toEqual([]);
  });
  it("interpolates chemical samples within one droplet", () => {
    expect(
      concentrationsAt(
        [
          { time: 1, A: 1 },
          { time: 3, A: 5 },
        ],
        2,
      ),
    ).toEqual({ values: { A: 3 }, interpolated: true });
    expect(
      concentrationsAt(
        [
          { time: 1, A: 1 },
          { time: 3, A: 5 },
        ],
        0,
      ).values.A,
    ).toBe(1);
  });
  it("renders occupied segments instead of only the last boundary", () => {
    expect(occupied(drop)).toEqual([{ channel: "c", start: 0.1, end: 0.3 }]);
    expect(
      occupied({
        ...drop,
        channels: ["full"],
        boundaries: [
          { channel: "left", position: 0.7, towardSource: false },
          { channel: "right", position: 0.1, towardSource: true },
        ],
      }),
    ).toEqual([
      { channel: "full", start: 0, end: 1 },
      { channel: "left", start: 0.7, end: 1 },
      { channel: "right", start: 0, end: 0.1 },
    ]);
  });
});
