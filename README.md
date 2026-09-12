# Sturmian monsters

A monster attacks on a schedule that is a **Sturmian sequence**. You block by
default, which is free and achieves nothing, or you attack — worth 2 against an
open beat, and costing you 5 against a strike. You never see the current beat
before committing, only the ones already past.

The claim is that Sturmian schedules are the *interesting* ones. A periodic
monster is solved at a finite depth and then permanently over; a random one
cannot be solved at any depth. A Sturmian one splits exactly once at every
window length, so there is always precisely one situation your memory cannot
resolve — and no amount of memory removes it, it only makes it rarer.

Measured over 300 fights per strategy, all at the same strike density:

| monster | m=2 | m=4 | m=6 | m=8 | m=12 |
|---|---|---|---|---|---|
| periodic, period 7 | 0% | 14% | 100% | 100% | 100% |
| Sturmian | 9% | 47% | 58% | 72% | 86% |
| random | 0% | 0% | 0% | 0% | 0% |

A step, a climb, a flat line.

## Play it

```sh
ghc -O2 -o game Game.hs
./game              # 20 HP vs 60, 120 beats. space to attack, anything else blocks
./game --bench      # re-check the balance if you change the constants
```

## The notes

- **[splitting.pdf](splitting.pdf)** — what a *split* is, what a Sturmian
  sequence is, and why they make interesting monsters. Three worked examples
  (Fibonacci, Pell, and the rotation of slope 1/√2), each with the 3→4 split
  computed by hand.
- **[what-claude-did.pdf](what-claude-did.pdf)** — a language model was given a
  shell and the rules and told to play ten fights. It lost the first, noticed
  that its own failed periods were the continued-fraction convergents of an
  irrational number, identified the schedules as Sturmian, wrote a program, and
  won the other nine while computing each monster's slope.

## The arena

The monster runs in one process and the player in another, so *"you may only
see the past"* is a property of the protocol rather than of anyone's restraint.
`state` has no field that could carry the next beat, and `reveal` refuses until
the fight is over.

```sh
cd arena
./up.sh --bot afterone            # or ./up.sh for a model-driven player
deno run --allow-net referee.ts 3 # three fights
```

`arena/engine.ts`, `live.ts`, `verify.ts` and `sweep.ts` were written by the
model during the ten-fight run, not by hand.

## The working-out

The Haskell files are the derivation, roughly in order: `Monsters.hs`,
`Zipper.hs`, `Explain.hs` and `StreamComonad.hs` on the comonad;
`Collapse.hs`, `PastOnly.hs`, `Same.hs`, `Nothing.hs` and `Reading.hs` on what
a fixed-memory player can and cannot know; `Player.hs`, `Study.hs`,
`Confident.hs`, `Duel.hs` and `Game.hs` on the game itself; `Alphas.hs` and
`Whole.hs` on the sequences.
