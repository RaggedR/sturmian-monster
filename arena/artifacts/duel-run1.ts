// duel.ts — plays one fight using only information the tool legitimately exposes.
//
// Model learned from fights 1-2:
//   * two OPEN beats never occur back to back  -> after '.', next is 'H' (certain)
//   * a run of STRIKES is never longer than 2  -> after 'HH', next is '.' (certain)
//   * otherwise the tape is strongly periodic  -> fit a period to the prefix
//
// It attacks on certainties for free, and only gambles when the clock forces it.

const PORT = 8811;
const ask = async (m: unknown) =>
  await (await fetch(`http://localhost:${PORT}`, {
    method: "POST", headers: { "content-type": "application/json" },
    body: JSON.stringify(m),
  })).json();

const sym = (b: number) => (b === 1 ? "H" : ".");

/** Every period that explains the whole history so far, smallest first. */
function periods(h: string): number[] {
  const ok: number[] = [];
  for (let p = 2; p <= Math.floor(h.length / 2); p++) {
    let fits = true;
    for (let i = p; i < h.length; i++) if (h[i] !== h[i - p]) { fits = false; break; }
    if (fits) ok.push(p);
  }
  return ok;
}

/** Start beat of every stutter (HH) seen so far, 0-indexed. */
function stutters(h: string): number[] {
  const out: number[] = [];
  for (let i = 0; i + 1 < h.length; i++) if (h[i] === "H" && h[i + 1] === "H") out.push(i);
  return out;
}

/** "open" | "strike" | "unknown", plus whether we'd stake our life on it. */
function predict(h: string): { open: boolean | null; certain: boolean } {
  if (h.endsWith(".")) return { open: false, certain: true };        // no ".."
  if (h.endsWith("HH")) return { open: true, certain: true };        // no "HHH"
  const ps = periods(h);
  if (ps.length) {
    const votes = new Set(ps.map((p) => h[h.length - p]));
    if (votes.size === 1) return { open: [...votes][0] === ".", certain: false };
  }
  return { open: null, certain: false };
}

/** How safe an uncertain "due to be open" beat looks, given the stutter rhythm.
 *  Stutters are roughly evenly spaced, so the calm water is just after one. */
function earlyInCycle(h: string): boolean {
  const st = stutters(h);
  if (!st.length) return false;                       // never gamble blind
  const gaps = st.slice(1).map((s, i) => s - st[i]).filter((g) => g > 1);
  const g = gaps.length ? Math.min(...gaps) : 7;      // 7 = tightest cycle seen so far
  return h.length - st[st.length - 1] < g - 2;
}

const PROBE = Number(Deno.args[0] ?? 10);   // free beats spent purely on looking
let h = "";
for (let beat = 1; beat <= 60; beat++) {
  const st = await ask({ tag: "State" });
  if (st.tag === "Error" || st.over) { console.log("done:", st.over ?? st.error); break; }
  h = (st.history as number[]).map(sym).join("");
  const hp = st.yourHP as number, mhp = st.monsterHP as number, left = st.beatsLeft as number;

  const p = predict(h);
  const hitsNeeded = Math.ceil(mhp / 2);
  // Opens arrive about every other beat; if the clock is tight we must gamble.
  const pressed = left < hitsNeeded * 2 + 4;
  const safeOnly = hp <= 5 && !pressed;          // one whiff from death: certainties only
  // Certain swings are free, so they are allowed even while probing.
  const worthIt = earlyInCycle(h) && !safeOnly;
  const swing = p.open === true && (p.certain || (beat > PROBE && (worthIt || pressed)));

  const r = await ask({ tag: "Move", attack: swing });
  const was = r.blow === 1 ? "H" : ".";
  const tag = swing ? "ATTACK" : "block ";
  console.log(`${String(beat).padStart(2)} ${tag} pred=${p.open === null ? "?" : p.open ? "." : "H"}`
    + `${p.certain ? "!" : " "} actual=${was} ${r.outcome.padEnd(8)} you ${r.yourHP} it ${r.monsterHP}`
    + `  ${h}${was}`);
  if (r.over) { console.log("FIGHT OVER:", r.over); break; }
}
