import { useCallback, useState } from "react";
import { emptyProject, type Project } from "./types";
const KEY = "fluidna-studio-v2-draft";
export function useProject() {
  const [history, setHistory] = useState<{
    past: Project[];
    current: Project;
    future: Project[];
  }>(() => {
    let current = emptyProject();
    try {
      const saved = JSON.parse(localStorage.getItem(KEY) || "null");
      if (saved?.schema_version === 2) current = saved;
    } catch {
      /* leave corrupt drafts untouched until next explicit edit */
    }
    return { past: [], current, future: [] };
  });
  const save = (p: Project) => {
    try {
      localStorage.setItem(KEY, JSON.stringify(p));
    } catch {
      /* export remains available when storage quota is exhausted */
    }
  };
  const edit = useCallback(
    (fn: (draft: Project) => void) =>
      setHistory((h) => {
        const current = structuredClone(h.current);
        fn(current);
        save(current);
        return { past: [...h.past.slice(-99), h.current], current, future: [] };
      }),
    [],
  );
  const replace = useCallback(
    (current: Project) =>
      setHistory((h) => {
        save(current);
        return { past: [...h.past.slice(-99), h.current], current, future: [] };
      }),
    [],
  );
  const undo = useCallback(
    () =>
      setHistory((h) => {
        if (!h.past.length) return h;
        const current = h.past[h.past.length - 1];
        save(current);
        return {
          past: h.past.slice(0, -1),
          current,
          future: [h.current, ...h.future],
        };
      }),
    [],
  );
  const redo = useCallback(
    () =>
      setHistory((h) => {
        if (!h.future.length) return h;
        const current = h.future[0];
        save(current);
        return {
          past: [...h.past, h.current],
          current,
          future: h.future.slice(1),
        };
      }),
    [],
  );
  return {
    project: history.current,
    edit,
    replace,
    undo,
    redo,
    canUndo: !!history.past.length,
    canRedo: !!history.future.length,
  };
}
