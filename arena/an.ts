// Pattern analyzer for a monster tape prefix. Usage: deno run an.ts ".HH.H..."
const h = Deno.args[0].split("").map(c => c === "H" ? 1 : 0);
const n = h.length;
console.log(`len=${n} H=${h.filter(x=>x).length} open=${h.filter(x=>!x).length}`);
// exact periods: p such that h[i]==h[i-p] for all i>=p (allow a warmup offset)
for (let start = 0; start <= Math.min(12, n - 6); start++) {
  for (let p = 1; p <= Math.floor((n - start) / 2); p++) {
    let ok = true;
    for (let i = start + p; i < n; i++) if (h[i] !== h[i - p]) { ok = false; break; }
    if (ok) console.log(`  EXACT period ${p} from index ${start}: [${h.slice(start,start+p).map(x=>x?"H":".").join("")}]`);
  }
}
// best lag correlations (mismatch counts)
const scores: [number,number,number][] = [];
for (let p = 1; p <= Math.min(40, n - 3); p++) {
  let m = 0, t = 0;
  for (let i = p; i < n; i++) { t++; if (h[i] !== h[i-p]) m++; }
  scores.push([p, m, t]);
}
scores.sort((a,b)=> a[1]/a[2] - b[1]/b[2]);
console.log("best lags (mismatch/total):", scores.slice(0,6).map(([p,m,t])=>`p${p}:${m}/${t}`).join(" "));
// order-k markov determinism check
for (let k = 1; k <= 6; k++) {
  const map = new Map<string, Set<number>>();
  for (let i = k; i < n; i++) {
    const key = h.slice(i-k, i).join("");
    if (!map.has(key)) map.set(key, new Set());
    map.get(key)!.add(h[i]);
  }
  const amb = [...map.values()].filter(s => s.size > 1).length;
  console.log(`  order-${k} markov: ${map.size} contexts, ${amb} ambiguous`);
  if (amb === 0 && map.size < n - k) {
    console.log(`    -> deterministic order-${k}:`, [...map.entries()].map(([kk,v])=>`${kk.replace(/1/g,"H").replace(/0/g,".")}->${[...v][0]?"H":"."}`).join(" "));
  }
}
