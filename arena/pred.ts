// Rotation-word predictor. OPEN at i  <=>  frac(i*a+b) < a.
// For a fixed a, the feasible set of b is an exact union of arcs on [0,1).
type Arc = [number, number]; // [lo,hi) non-wrapping, within [0,1)
const norm = (x: number) => ((x % 1) + 1) % 1;
function arcsFor(i: number, a: number, open: boolean): Arc[] {
  const lo = norm(-i * a), hi = norm(-i * a + a);
  let A: Arc[] = lo < hi ? [[lo, hi]] : [[0, hi], [lo, 1]];
  if (open) return A;
  // complement
  const out: Arc[] = []; let cur = 0;
  for (const [l, h] of A.sort((x, y) => x[0] - y[0])) { if (l > cur) out.push([cur, l]); cur = Math.max(cur, h); }
  if (cur < 1) out.push([cur, 1]);
  return out;
}
function inter(X: Arc[], Y: Arc[]): Arc[] {
  const out: Arc[] = [];
  for (const [a1, b1] of X) for (const [a2, b2] of Y) { const l = Math.max(a1, a2), h = Math.min(b1, b2); if (h - l > 1e-12) out.push([l, h]); }
  return out;
}
export function fit(hist: number[], aMin = 0.02, aMax = 0.98, steps = 60000) {
  const cands: { a: number; arcs: Arc[] }[] = [];
  for (let k = 0; k <= steps; k++) {
    const a = aMin + (aMax - aMin) * k / steps;
    let arcs: Arc[] = [[0, 1]];
    for (let i = 0; i < hist.length; i++) {
      arcs = inter(arcs, arcsFor(i, a, hist[i] === 0));
      if (!arcs.length) break;
    }
    if (arcs.length) cands.push({ a, arcs });
  }
  return cands;
}
/** returns 'OPEN' | 'STRIKE' | '?' for beat j, unanimity over all candidates */
export function predict(cands: { a: number; arcs: Arc[] }[], j: number): string {
  let sawOpen = false, sawStrike = false;
  for (const { a, arcs } of cands) {
    const base = norm(j * a);
    for (const [l, h] of arcs) {
      // frac(j*a + b) < a  for b in [l,h) ?  value ranges over [base+l, base+h)
      const lo = base + l, hi = base + h;
      // sample the interval for both outcomes: check endpoints & crossing points
      let anyOpen = false, anyStrike = false;
      const pts: number[] = [lo + 1e-12, hi - 1e-12];
      for (let w = Math.floor(lo); w <= Math.ceil(hi); w++) { for (const c of [w, w + a]) if (c > lo && c < hi) { pts.push(c - 1e-9, c + 1e-9); } }
      for (const p of pts) { if (p <= lo || p >= hi) continue; (norm(p) < a ? (anyOpen = true) : (anyStrike = true)); }
      if (anyOpen) sawOpen = true; if (anyStrike) sawStrike = true;
      if (sawOpen && sawStrike) return "?";
    }
  }
  if (sawOpen && !sawStrike) return "OPEN";
  if (sawStrike && !sawOpen) return "STRIKE";
  return "?";
}
if (import.meta.main) {
  const hist = Deno.args[0].split("").map(c => c === "H" ? 1 : 0);
  const horizon = Number(Deno.args[1] ?? 30);
  const c = fit(hist);
  console.log(`candidates: ${c.length} a-values, a range [${c.length?c[0].a.toFixed(4):"-"}, ${c.length?c[c.length-1].a.toFixed(4):"-"}]`);
  let s = "";
  for (let j = hist.length; j < hist.length + horizon; j++) { const p = predict(c, j); s += p === "OPEN" ? "." : p === "STRIKE" ? "H" : "?"; }
  console.log("forecast:", s);
}
