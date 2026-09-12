type Arc = [number, number];
const norm = (x: number) => ((x % 1) + 1) % 1;
function arcsFor(i: number, a: number, open: boolean): Arc[] {
  const lo = norm(-i * a), hi = norm(-i * a + a);
  const A: Arc[] = lo < hi ? [[lo, hi]] : [[0, hi], [lo, 1]];
  if (open) return A;
  const out: Arc[] = []; let cur = 0;
  for (const [l, h] of A.sort((x, y) => x[0] - y[0])) { if (l > cur) out.push([cur, l]); cur = Math.max(cur, h); }
  if (cur < 1) out.push([cur, 1]);
  return out;
}
function inter(X: Arc[], Y: Arc[]): Arc[] {
  const out: Arc[] = [];
  for (const [a1, b1] of X) for (const [a2, b2] of Y) { const l = Math.max(a1, a2), h = Math.min(b1, b2); if (h - l > 1e-13) out.push([l, h]); }
  return out;
}
export type Cand = { a: number; arcs: Arc[] };
function sweep(hist: number[], aMin: number, aMax: number, steps: number): Cand[] {
  const cands: Cand[] = [];
  for (let k = 0; k <= steps; k++) {
    const a = aMin + (aMax - aMin) * k / steps;
    if (a <= 0 || a >= 1) continue;
    let arcs: Arc[] = [[0, 1]];
    let ok = true;
    for (let i = 0; i < hist.length; i++) { arcs = inter(arcs, arcsFor(i, a, hist[i] === 0)); if (!arcs.length) { ok = false; break; } }
    if (ok) cands.push({ a, arcs });
  }
  return cands;
}
/** Adaptive: zoom the a-grid so the true slope is never squeezed out. */
export function fit(hist: number[]): Cand[] {
  let lo = 1e-4, hi = 1 - 1e-4, use = Math.min(10, hist.length);
  let c = sweep(hist.slice(0, use), lo, hi, 20000);
  while (use < hist.length) {
    if (!c.length) return [];
    const pad = (hi - lo) / 20000 * 2;
    lo = Math.max(1e-4, c[0].a - pad); hi = Math.min(1 - 1e-4, c[c.length - 1].a + pad);
    use = Math.min(hist.length, use + 15);
    c = sweep(hist.slice(0, use), lo, hi, 20000);
  }
  return c;
}
export function predict(cands: Cand[], j: number): "OPEN" | "STRIKE" | "?" {
  let sawOpen = false, sawStrike = false;
  for (const { a, arcs } of cands) {
    const base = norm(j * a);
    for (const [l, h] of arcs) {
      const lo = base + l, hi = base + h;
      const pts: number[] = [lo + 1e-13, hi - 1e-13];
      for (let w = Math.floor(lo); w <= Math.ceil(hi) + 1; w++) for (const cut of [w, w + a]) if (cut > lo && cut < hi) pts.push(cut - 1e-11, cut + 1e-11);
      for (const p of pts) { if (p <= lo || p >= hi) continue; if (norm(p) < a) sawOpen = true; else sawStrike = true; }
      if (sawOpen && sawStrike) return "?";
    }
  }
  return sawOpen ? "OPEN" : sawStrike ? "STRIKE" : "?";
}
