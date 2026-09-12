// monster.server.ts (:8811) — the monster, behind a socket.
//
// The whole point of putting it in its own process: the player cannot see the
// future because the monster never sends it. In the single-process Haskell
// version "you may only look at the past" was a convention I could have broken
// by typing `right` instead of `left`. Here it is a property of the wire.
//
// `State` returns the history and nothing else. The next blow exists only in
// this process's memory until the beat resolves.

import { serve, decodeJson } from "./wire.ts";

const PORT = 8811;

// Tuned by sweep. The monster's HP sets the clock pressure, yours sets the
// mistake budget. 60 HP is 30 hits, well past the ~17 certain hits a 120-beat
// fight offers, so "wait for HH" cannot win on time. 20 HP is four whiffs.
export const PLAYER_HP = 20;
export const MONSTER_HP = 60;
export const BEATS = 120;
export const HIT = 2;      // you connect on an open beat
export const WHIFF = 5;    // you swing into a strike

type Fight = {
  tape: number[]; alpha: number; hist: number[];
  php: number; mhp: number; left: number; over: string;
};

let fight: Fight | null = null;

// ── the monster itself ────────────────────────────────────────────────
// Sturmian with slope alpha: s_k = floor((k+1)a + rho) - floor(ka + rho).
// alpha is drawn in [0.52, 0.62], always above 1/2 so that it can strike twice
// running, and always below 2/3 so it can never strike three times running.
// alpha comes from the CSPRNG and is never written down anywhere the player
// could reach. There is no seed to return, so reading this file tells you
// nothing: the slope exists only in this process's memory until Reveal.
function rand(): number {
  const b = new Uint32Array(1);
  crypto.getRandomValues(b);
  return b[0] / 4294967296;
}

function newFight(): Fight {
  const alpha = 0.52 + 0.10 * rand();
  const rho = rand();
  const tape = Array.from({ length: BEATS + 2 }, (_, k) =>
    Math.floor((k + 1) * alpha + rho) - Math.floor(k * alpha + rho));
  return { tape, alpha, hist: [], php: PLAYER_HP, mhp: MONSTER_HP, left: BEATS, over: "" };
}

function cf(x: number, n = 10): number[] {
  const out: number[] = [];
  for (let i = 0; i < n && x > 1e-9; i++) {
    const r = 1 / x, a = Math.floor(r);
    out.push(a); x = r - a;
  }
  return out;
}

// ── the four things it will answer ────────────────────────────────────
serve(PORT, "monster", (msg: Record<string, unknown>) => {
  const tag = msg.tag;

  if (tag === "New") {
    fight = newFight();
    return { tag: "Ack", beats: BEATS, yourHP: PLAYER_HP, monsterHP: MONSTER_HP, hit: HIT, whiff: WHIFF };
  }

  if (!fight) throw new Error("no fight in progress; send {tag:'New'} first");

  // THE COMONAD, ENFORCED BY THE WIRE.
  // history is the past. There is no field here that contains tape[hist.length].
  if (tag === "State") {
    return {
      tag: "State",
      history: fight.hist.slice(),
      yourHP: fight.php, monsterHP: fight.mhp,
      beatsLeft: fight.left, over: fight.over,
    };
  }

  if (tag === "Move") {
    if (fight.over) return { tag: "Over", result: fight.over };
    const attack = msg.attack === true;
    const blow = fight.tape[fight.hist.length];
    let outcome: string;
    if (attack && blow === 0) { fight.mhp -= HIT;   outcome = "CONNECT"; }
    else if (attack)          { fight.php -= WHIFF; outcome = "CLASH"; }
    else if (blow === 1)      {                     outcome = "BLOCKED"; }
    else                      {                     outcome = "MISSED"; }
    fight.hist.push(blow);
    fight.left -= 1;
    if (fight.mhp <= 0)      fight.over = "player wins";
    else if (fight.php <= 0) fight.over = "monster wins";
    else if (fight.left <= 0) fight.over = "out of time";
    return {
      tag: "Beat", blow, outcome,
      yourHP: fight.php, monsterHP: fight.mhp,
      beatsLeft: fight.left, over: fight.over,
    };
  }

  // Only after it is settled. Before that, this is the future.
  if (tag === "Reveal") {
    if (!fight.over) throw new Error("the fight is not over");
    return { tag: "Reveal", alpha: fight.alpha, cf: cf(fight.alpha), tape: fight.tape.slice(0, BEATS) };
  }

  throw new Error(`unknown tag ${JSON.stringify(tag)}`);
});
