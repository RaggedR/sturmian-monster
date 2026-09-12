import { fit, predict } from "./engine.ts";
const TAPE = Deno.args[0].split("").map(c => c === "H" ? 1 : 0);
const seed = Number(Deno.args[1] ?? 12);
let hp = 20, mhp = 60, hits = 0, whiffs = 0, misses = 0, unknown = 0;
const hist: number[] = [];
for (let i = 0; i < TAPE.length && hp > 0 && mhp > 0; i++) {
  let act = "block";
  if (i >= seed) { const p = predict(fit(hist), i); if (p === "OPEN") act = "attack"; else if (p === "?") unknown++; }
  const blow = TAPE[i]; hist.push(blow);
  if (act === "attack") { if (blow === 0) { hits++; mhp -= 2; } else { whiffs++; hp -= 5; } }
  else if (blow === 0 && i >= seed) misses++;
}
console.log(`seed=${seed} hits=${hits} whiffs=${whiffs} openMissed=${misses} unknownBeats=${unknown} finalHP=${hp} monsterHP=${mhp} => ${mhp<=0?"WIN":"loss"}`);
