// play.ts — the tool the agent drives.
//
//   deno run --allow-net play.ts new        start a fight
//   deno run --allow-net play.ts attack     one beat, swinging
//   deno run --allow-net play.ts block      one beat, guarding
//   deno run --allow-net play.ts state      history, HP, beats left
//   deno run --allow-net play.ts reveal     the whole tape (only once it is over)
//
// One line of output per call, meant to be read by something that is deciding
// what to do next. The monster's future is not obtainable through any of these.

const PORT = 8811;
const sym = (b: number) => (b === 1 ? "H" : ".");

async function ask(msg: unknown): Promise<Record<string, unknown>> {
  const r = await fetch(`http://localhost:${PORT}`, {
    method: "POST", headers: { "content-type": "application/json" },
    body: JSON.stringify(msg),
  });
  return await r.json();
}

const strip = (h: number[] | undefined) => (h ?? []).map(sym).join("") || "(empty)";

/** Anything the server refuses should read as a sentence, not a stack trace. */
function guard(r: Record<string, unknown>): boolean {
  if (r.tag === "Error") {
    const e = String(r.error);
    console.log(e.includes("no fight in progress")
      ? "no fight in progress — run `play.ts new` first"
      : e.replace(/^Error: /, ""));
    return true;
  }
  return false;
}
const cmd = Deno.args[0] ?? "state";

if (cmd === "new") {
  const a = await ask({ tag: "New" });
  if (guard(a)) Deno.exit(0);
  console.log(`NEW FIGHT. you ${a.yourHP} HP, monster ${a.monsterHP} HP, ${a.beats} beats.`
    + ` attack an OPEN beat: it takes ${a.hit}. attack a STRIKE: you take ${a.whiff}.`);
} else if (cmd === "attack" || cmd === "block") {
  const r = await ask({ tag: "Move", attack: cmd === "attack" });
  if (guard(r)) Deno.exit(0);
  if (r.tag === "Over") console.log(`already over: ${r.result}`);
  else {
    const st = await ask({ tag: "State" });
    console.log(`${cmd.toUpperCase()} -> it was ${r.blow === 1 ? "STRIKING" : "OPEN"}`
      + `  ${r.outcome}   you ${r.yourHP} HP, it ${r.monsterHP} HP, ${r.beatsLeft} beats left`
      + `   history: ${strip(st.history as number[])}`
      + (r.over ? `\nFIGHT OVER: ${r.over}` : ""));
  }
} else if (cmd === "reveal") {
  const r = await ask({ tag: "Reveal" });
  if (r.tag === "Error") console.log(String(r.error).replace(/^Error: /, ""));
  else console.log(`the whole tape was: ${strip(r.tape as number[])}`);
} else {
  const st = await ask({ tag: "State" });
  if (guard(st)) Deno.exit(0);
  console.log(`you ${st.yourHP} HP, it ${st.monsterHP} HP, ${st.beatsLeft} beats left`
    + `   history: ${strip(st.history as number[])}${st.over ? `   OVER: ${st.over}` : ""}`);
}
