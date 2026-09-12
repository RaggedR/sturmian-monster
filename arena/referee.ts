// referee.ts — the loop, over several fights.
//
//   deno run --allow-net referee.ts [fights] [baseSeed]
//
// One monster per fight, a fresh slope each time. The player keeps its session
// across all of them, and is shown the true tape after each fight.

import { ask } from "./wire.ts";

const MONSTER = 8811, PLAYER = 8812;
const sym = (b: number) => (b === 1 ? "H" : ".");
const nums = Deno.args.filter((a) => /^\d+$/.test(a)).map(Number);
const fights = nums[0] ?? 3;
const baseSeed = nums[1] ?? 1000;

const first = await ask(MONSTER, { tag: "New", seed: baseSeed });
await ask(PLAYER, { tag: "Rules", hp: first.hp, beats: first.beats, hit: first.hit, whiff: first.whiff });

const tally: string[] = [];

for (let f = 0; f < fights; f++) {
  const ack = f === 0 ? first : await ask(MONSTER, { tag: "New", seed: baseSeed + f * 7919 });
  console.log(`\n\x1b[1m  FIGHT ${f + 1} of ${fights}\x1b[0m — seed ${ack.seed}\n`);
  let beat = 0;
  while (true) {
    const st = await ask(MONSTER, { tag: "State" });
    if (st.over) break;
    const mv = await ask(PLAYER, {
      tag: "Decide", history: st.history, yourHP: st.yourHP,
      monsterHP: st.monsterHP, beatsLeft: st.beatsLeft, hit: ack.hit, whiff: ack.whiff,
    });
    const res = await ask(MONSTER, { tag: "Move", attack: mv.attack });
    beat += 1;
    const mark = { CONNECT: "\x1b[33mCONNECT\x1b[0m", CLASH: "\x1b[31mCLASH  \x1b[0m",
                   BLOCKED: "\x1b[94mBLOCKED\x1b[0m", MISSED: "\x1b[2mmissed \x1b[0m" }[res.outcome as string];
    console.log(`  ${String(beat).padStart(2)}  ${(st.history as number[]).map(sym).join("").padEnd(41)}`
      + ` ${mv.attack ? "ATTACK" : "block "} -> ${sym(res.blow as number)}  ${mark}`
      + `  you ${String(res.yourHP).padStart(2)}  it ${String(res.monsterHP).padStart(2)}`
      + `  \x1b[2m${mv.why}\x1b[0m`);
  }
  const fin = await ask(MONSTER, { tag: "State" });
  const rev = await ask(MONSTER, { tag: "Reveal" });
  tally.push(fin.over as string);
  console.log(`\n  \x1b[1m${fin.over}\x1b[0m   (alpha ${(rev.alpha as number).toFixed(4)})`);
  const les = await ask(PLAYER, {
    tag: "Result", outcome: fin.over, tape: rev.tape,
    yourHP: fin.yourHP, monsterHP: fin.monsterHP,
  });
  console.log(`  \x1b[36mlesson:\x1b[0m ${les.lesson}`);
}

console.log(`\n\x1b[1m  TALLY\x1b[0m  ${tally.map((t, i) => `${i + 1}:${t}`).join("   ")}`);
console.log(`  won ${tally.filter((t) => t === "player wins").length} of ${fights}\n`);
