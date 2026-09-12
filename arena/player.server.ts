// player.server.ts (:8812) — the player, answered by claude.
//
// ONE claude process for the whole run, fed on stdin. So within a fight it is not a
// pure coKleisli map -- it remembers earlier beats -- and across fights it
// remembers earlier monsters. Each fight is a DIFFERENT monster (a fresh
// irrational slope), so nothing about a particular tape transfers. Only a
// strategy can.
//
// STREAM_PLAYER=afterone | afterhh runs a local heuristic instead, free.

import { serve, decodeJson, isRecord, trace } from "./wire.ts";

const PORT = 8812;
const WHO = Deno.env.get("STREAM_PLAYER") ?? "claude";

function brief(p: Record<string, unknown>): string {
  const hist = (p.history as number[]).map((b) => (b === 1 ? "H" : ".")).join(" ");
  return [
    "BEAT. Reply with ONE json object, nothing before or after it, no reasoning:",
    `  {"attack": true, "why": "<=8 words"}`,
    "",
    "Everything THIS monster has done so far, oldest first (. = open, H = strike):",
    `  ${hist || "(nothing yet — first beat of this fight)"}`,
    "",
    `  your HP ${p.yourHP}      its HP ${p.monsterHP}      beats left ${p.beatsLeft}`,
  ].join("\n");
}

function rules(p: Record<string, unknown>): string {
  return [
    "You are fighting a series of monsters, one beat at a time.",
    "",
    "Each beat the monster either STRIKES or is OPEN. You cannot see which until",
    "after you have committed. You pick one action:",
    "",
    `  BLOCK  (attack:false) — you take nothing, whatever it does, and deal nothing.`,
    `  ATTACK (attack:true)  — if it is OPEN you deal ${p.hit}. If it is STRIKING you take ${p.whiff}.`,
    "",
    `You and the monster start on ${p.hp} HP. Reduce it to 0 within ${p.beats} beats or you lose.`,
    "",
    "Each fight is a NEW monster with a different pattern, so what you learn about",
    "one tape will not transfer. Anything you learn about how such monsters behave",
    "in general might.",
  ].join("\n");
}

function debrief(p: Record<string, unknown>): string {
  const tape = (p.tape as number[]).map((b) => (b === 1 ? "H" : ".")).join(" ");
  return [
    `FIGHT OVER — ${p.outcome}.  You ended on ${p.yourHP} HP, it ended on ${p.monsterHP}.`,
    "",
    "Here is what that monster was going to do, the whole tape, which you could",
    "not see at the time:",
    `  ${tape}`,
    "",
    "Look at it. Say in one short sentence what you will do differently next time.",
    "Reply as plain text, no JSON.",
  ].join("\n");
}

// ── ONE claude process for the entire run ─────────────────────────────
// Not one per beat. The session is a single conversation, so it should be a
// single process: messages go in on stdin, replies come back on stdout, and
// the transcript never has to be reloaded because it never left.
//
// --safe-mode disables CLAUDE.md, skills, plugins, hooks and MCP servers, so
// the player is a clean agent rather than one carrying Robin's setup around.
const enc = new TextEncoder();
let child: Deno.ChildProcess | null = null;
let stdin: WritableStreamDefaultWriter<Uint8Array>;
let lines: AsyncIterator<string>;

async function* lineIter(rs: ReadableStream<Uint8Array>) {
  const dec = new TextDecoder();
  let buf = "";
  for await (const chunk of rs) {
    buf += dec.decode(chunk, { stream: true });
    let i: number;
    while ((i = buf.indexOf("\n")) >= 0) { yield buf.slice(0, i); buf = buf.slice(i + 1); }
  }
}

function start() {
  child = new Deno.Command("claude", {
    // --tools "" drops the built-in tool definitions: the agent system prompt
    // goes from ~22k tokens to a fraction of that, and it is prefilled on every
    // single beat. We are asking a yes/no question; it needs no tools at all.
    args: ["-p", "--safe-mode", "--tools", "", "--verbose",
    "--input-format", "stream-json", "--output-format", "stream-json"],
    stdin: "piped", stdout: "piped", stderr: "null",
  }).spawn();
  stdin = child.stdin.getWriter();
  lines = lineIter(child.stdout)[Symbol.asyncIterator]();
  trace("player", "one claude process, streaming");
}

async function askClaude(prompt: string): Promise<string> {
  if (!child) start();
  const t0 = performance.now();
  await stdin.write(enc.encode(JSON.stringify({
    type: "user",
    message: { role: "user", content: [{ type: "text", text: prompt }] },
  }) + "\n"));
  let out = "";
  // next() by hand: a for-await would close the iterator on the first reply.
  while (true) {
    const { value, done } = await lines.next();
    if (done) throw new Error("claude stream ended");
    let m: Record<string, unknown>;
    try { m = JSON.parse(value as string); } catch { continue; }
    if (m.type === "assistant") {
      const content = (m.message as { content?: { type: string; text?: string }[] })?.content ?? [];
      for (const c of content) if (c.type === "text") out += c.text ?? "";
    }
    if (m.type === "result") {
      const ms = performance.now() - t0;
      const u = (m as Record<string, unknown>).usage as Record<string, number> | undefined;
      trace("player", `${ms.toFixed(0)}ms  in=${u?.input_tokens ?? "?"}`
        + ` cacheRead=${u?.cache_read_input_tokens ?? 0} out=${u?.output_tokens ?? "?"}`);
      return out.trim();
    }
  }
}

/** The decoder is the audit. An unreadable answer blocks, which is the safe move. */
function decodeMove(text: string): { attack: boolean; why: string } {
  const v = decodeJson(text);
  if (!isRecord(v) || typeof v.attack !== "boolean") throw new Error(`not a move: ${text.slice(0, 120)}`);
  return { attack: v.attack, why: typeof v.why === "string" ? v.why : "" };
}

function heuristic(history: number[]) {
  const last = history[history.length - 1], prev = history[history.length - 2];
  return WHO === "afterhh"
    ? { attack: last === 1 && prev === 1, why: "swing only after two strikes" }
    : { attack: last === 1, why: "swing after a strike" };
}

serve(PORT, "player", async (msg) => {
  if (msg.tag === "Rules") {
    if (WHO !== "claude") return { tag: "Ack", who: WHO };
    await askClaude(rules(msg) + "\n\nReply with just: ready");
    return { tag: "Ack", who: "claude" };
  }
  if (msg.tag === "Decide") {
    if (WHO !== "claude") return { tag: "Move", ...heuristic(msg.history as number[]) };
    try { return { tag: "Move", ...decodeMove(await askClaude(brief(msg))) }; }
    catch (e) { trace("player", `unreadable, blocking — ${e}`); return { tag: "Move", attack: false, why: "(blocked)" }; }
  }
  if (msg.tag === "Result") {
    if (WHO !== "claude") return { tag: "Lesson", lesson: "(heuristic; no reflection)" };
    try { return { tag: "Lesson", lesson: (await askClaude(debrief(msg))).split("\n")[0].slice(0, 200) }; }
    catch (e) { return { tag: "Lesson", lesson: `(no reflection: ${e})` }; }
  }
  throw new Error(`unknown tag ${JSON.stringify(msg.tag)}`);
});
