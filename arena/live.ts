import { fit, predict } from "./engine.ts";
async function tool(...args: string[]) {
  const c = new Deno.Command("deno", { args: ["run", "--quiet", "--allow-net", "play.ts", ...args], stdout: "piped" });
  return new TextDecoder().decode((await c.output()).stdout).trim();
}
const SEED = Number(Deno.args[0] ?? 12);
const n = await tool("new");
console.log(n);
const [, hp0, mhp0, beats, hit, whiff] = n.match(/you (\d+) HP, monster (\d+) HP, (\d+) beats.*takes (\d+).*take (\d+)/)!.map(Number);
const need = Math.ceil(mhp0 / hit), lives = Math.ceil(hp0 / whiff) - 1;
console.log(`need ${need} hits in ${beats} beats; can survive ${lives} whiffs`);
const hist: number[] = [];
let hits = 0, whiffs = 0, unk = 0, missed = 0;
for (let i = 0; i < beats; i++) {
  let act = "block", pred = "seed";
  if (i >= SEED) { pred = predict(fit(hist), i); if (pred === "OPEN") act = "attack"; }
  const out = await tool(act);
  if (out.includes("no fight") || out.includes("already over")) { console.log(out); break; }
  const blow = out.includes("was OPEN") ? 0 : 1;
  hist.push(blow);
  if (act === "attack") { blow === 0 ? hits++ : whiffs++; } else if (blow === 0 && i >= SEED) missed++;
  if (pred === "?") unk++;
  if (pred !== "seed" && pred !== "?" && ((pred === "OPEN") !== (blow === 0))) console.log(`!! WRONG at ${i}: predicted ${pred}, was ${blow ? "STRIKE" : "OPEN"}`);
  if (out.includes("FIGHT OVER")) { console.log(`beat ${i}: ${out.split("\n").pop()}`); break; }
  if (i % 20 === 19) console.log(`  [${i}] hits=${hits} whiffs=${whiffs} unk=${unk} missed=${missed} | ${out.split("   ")[1]?.trim() ?? ""}`);
}
console.log(await tool("state"));
console.log(`summary: hits=${hits} whiffs=${whiffs} unknownBeats=${unk} missedOpens=${missed}`);
console.log(await tool("reveal"));
