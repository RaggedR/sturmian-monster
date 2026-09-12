# Sturmian monsters

A monster attacks on a schedule that is a **Sturmian sequence**. You block by
default, or you attack. You never see the current beat before committing, only
the ones already past.

|            | monster **open** | monster **striking** |
|------------|------------------|----------------------|
| **block**  | nothing          | you take 1 *(chip)*  |
| **attack** | it takes 2       | you take 5           |

There is **no clock**. Blocking a strike costs you, so waiting is a choice you
pay for in the same currency as everything else — which is what makes the fight
end.

The claim is that Sturmian schedules are the *interesting* ones. A periodic
monster is solved at a finite depth and then permanently over; a random one
cannot be solved at any depth. A Sturmian one splits exactly once at every
window length, so there is always precisely one situation your memory cannot
resolve — and no amount of memory removes it, it only makes it rarer.

Measured over 300 fights per cell, all three monsters at the same strike
density, against a player that memorises the last *m* beats
(`bench/ThreeMonsters.hs`):

| monster | m=2 | m=4 | m=6 | m=8 | m=12 |
|---|---:|---:|---:|---:|---:|
| periodic, period 7 | 0% | 0% | 100% | 100% | 100% |
| Sturmian | 21% | 56% | 68% | 82% | 83% |
| random | 0% | 0% | 0% | 0% | 0% |

A step, a climb, a flat line.

## The split — and what is on the other side of it

For ½ < α < ⅔ the word contains no `..` and no `HHH`, so of the three things
the last two beats can be, **exactly one is undetermined**:

```
   H.  ->  H    certain strike     never swing   (-5 if you do)
   HH  ->  .    certain open       always swing  (+2, free)
   .H  ->  ?    THE SPLIT          the whole game is here
```

Notice the Sturmian row above stops climbing: 82% at m=8, 83% at m=12. A table
memorises one level and then has nothing left to learn, because its blind spot
is the same split at every length.

But the outcomes at the `.H` sites form a Sturmian word **of their own** —
complexity k+1, checked. So the game *renormalises*, and a player that recurses
into the derived word learns α's next partial quotient instead of memorising
more of the word. 300 fights, α ∈ [0.52, 0.62], 60 HP each (`./game --bench`):

| strategy | wins |
|---|---:|
| attack every beat | 0% |
| swing only after `HH` — never wrong, too slow to outrun the chip | 0% |
| swing after any strike — takes the split blind | 23% |
| memory-8 table over the raw beats (256 contexts) | 82% |
| **renormalising, depth 4** (16 contexts on the derived word) | **92%** |
| perfect prediction | 100% |

A 16-context recursive player beats a 4096-context memoriser. Renormalising is
worth about eight levels of raw memory, at 1/256 of the storage.

Every number here is measured, not tuned — see
**[difficulty-level.md](difficulty-level.md)** for the full tables and
`bench/` for the programs that produce them.

## Play it

```sh
ghc -O2 -o game Game.hs
./game              # 60 HP each. space to attack, anything else blocks
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
