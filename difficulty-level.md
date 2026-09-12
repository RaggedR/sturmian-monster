# Difficulty

Measured, not tuned. Every table here comes from a program in `bench/`;
rebuild any of them with `ghc -O2 -o /tmp/x bench/<file>.hs && /tmp/x`.

Unless stated: you 20 HP, monster 60 HP, attack deals 2, a mistaken swing
costs 5. So **30 hits to win, 3 mistakes to lose**.

---

## The law

Blocking is free and achieves nothing except that you see the beat. So
blocking is an information purchase costing only tempo, and **time converts
into certainty at a fixed exchange rate**:

```
(density of certain situations) × (beats available)  ≥  hits needed
```

Everything below is that inequality wearing different clothes. The slope
matters only through the density term; the clock matters directly.

For ½ < α < ⅔ the certain situation is "the beat after `HH`", whose density is
**2α − 1**.

---

## Fight length is the dominant lever

Slope fixed at α ∈ [0.52, 0.62], density ≈ 0.14, 30 hits needed.
200 fights per row. `bench/Slack.hs`

| beats | certain hits available | after-`HH` only | memory-8 table |
|---:|---:|---:|---:|
| 60 | 8 | 0% | 0% |
| 90 | 13 | 0% | 58% |
| 120 | 17 | 0% | 71% |
| 180 | 25 | 38% | 71% |
| 240 | 34 | 57% | 71% |
| 320 | 45 | 74% | 71% |
| 420 | 59 | 84% | 71% |

The pure-certainty player goes 0% → 84% on the clock alone. Predicted
crossover 30/0.14 ≈ 214 beats; measured between 180 and 240.

**The memory-8 column flatlines at 71% from 120 beats on.** Extra time is
worth nothing to a fixed-memory table, because its blind spot is the same one
split at every length and does not shrink with observation. Time only helps a
player whose ignorance is reducible.

---

## Tiers by slope

200 fights per tier, 120 beats. `bench/Tiers.hs`

| tier | α | mash | after-`H` | after-`HH` | memory-8 | longest strike run | certainty density |
|---|---|---:|---:|---:|---:|---:|---:|
| trash | .35–.45 | 0% | **100%** | 0% | 90% | 1 | 40% |
| regular | .52–.62 | 0% | 8% | 0% | 71% | 2 | 14% |
| elite | .63–.66 | 0% | 0% | **100%** | 82% | 2 | 29% |
| boss | .70–.78 | 0% | 0% | 0% | **10%** | 3 | 13% |

Note the non-monotonicity: `.63–.66` is **easier** than `.52–.62`, because
certainty density rises to 0.30 and 0.30 × 120 = 36 ≥ 30 satisfies the law.
Difficulty is a sawtooth in α, not a slope — it spikes on crossing a boundary
and eases until the next one. **Put monsters at the bottom of a band.**

---

## Where the boundaries are

Longest run of strikes is ⌊α/(1−α)⌋ + 1, so it changes at α = k/(k+1):

| α | longest strike run | the free rule you get |
|---|---:|---|
| < ½ | 1 | after **any** strike, an open — guaranteed and frequent |
| ½ – ⅔ | 2 | after `HH`, an open — guaranteed but rationed |
| ⅔ – ¾ | 3 | after `HHH` — rarer still |
| → 1 | grows | certainty vanishes |

Each boundary kills the rule learned on the tier below it. A memory-8 reader
wins essentially everything below ⅔ and almost nothing above.

---

## The three kinds of monster

Matched strike density so only the splitting differs. 300 fights.
`bench/ThreeMonsters.hs`

| monster | m=2 | m=4 | m=6 | m=8 | m=12 |
|---|---:|---:|---:|---:|---:|
| periodic, period 7 | 0% | 14% | 100% | 100% | 100% |
| Sturmian | 9% | 47% | 58% | 72% | 86% |
| random | 0% | 0% | 0% | 0% | 0% |

A step, a climb, a flat line. Periodic is solved at finite depth and then
permanently over; random cannot be solved at any depth; Sturmian can be
understood further at every depth and completely at none.

---

## HP against beats

Choosing the size. `bench/HPandBeats.hs`

| you | monster | beats | mash | after-`H` | after-`HH` | mem4 | mem8 |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 10 | 10 | 40 | 0% | 49% | 60% | 91% | 100% |
| 20 | 60 | 120 | 0% | 8% | 0% | 47% | 72% |
| 20 | 80 | 120 | 0% | 2% | 0% | 26% | 49% |
| 15 | 100 | 150 | 0% | 0% | 0% | 20% | 41% |

The monster's HP sets the clock pressure; yours sets the mistake budget. They
want different values, which is why one shared number could not tune this.
20/60/120 is the setting with a usable ladder: memory 2 → 9%, 4 → 47%,
6 → 58%, 8 → 72%, 12 → 86%, perfect → 100%.

---

## Not measured

- **The cost of blocking.** It is currently free, which is the whole reason
  time converts into certainty. Charging for it — chip damage, stamina — is a
  one-cell change to `payoff` and should be the sharpest untested lever: it
  would make a long fight hard again without touching the slope.
- **Continued-fraction structure.** A slope with a large partial quotient is
  near-periodic for ~aₖ·qₖ₋₁ beats, so it plays as *solved* for a computable
  stretch and then betrays once. Demonstrated in `Alphas.hs`, never benched
  as a difficulty setting.
- A sweep at **fixed α per row** was attempted and discarded: with only the
  phase varying, each row is one monster played many times, so results are 0%
  or 100% and the fine structure is noise. Sample α *within* a band.
