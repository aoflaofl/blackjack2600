# Disassembly of Atari 2600 Blackjack

## 1) Project overview

This project investigates a long-running claim that Atari 2600 **Blackjack** cheats by dealing itself stronger hands when it is behind.

The central question is: **does the game logic intentionally bias outcomes, or do players perceive bias from limited pseudo-randomness and shuffle behavior in a 2 KB ROM?**

Because the game is implemented in a very small 6502 codebase, this repository focuses on annotated reverse engineering so the claim can be tested against the actual instructions rather than anecdotal reports.

## 2) Reproducible workflow

### Tools

- DiStella (disassembly)
- DASM (assembly)
- `sha256sum` (artifact verification)

### Commands

Run from repository root.

1. Disassembly reference used in this repo:

```bash
DiStella -pabf -cblackjack.cfg Blackjack.bin
```

2. Rebuild ROM from annotated source:

```bash
dasm blackjack.s -f3 -oBlackjack.bin
```

3. Verify output artifact hash:

```bash
sha256sum Blackjack.bin
```

### Expected output artifacts

- `blackjack.s`: annotated disassembly source tracked in this repo.
- `Blackjack.bin`: rebuilt ROM image.
- `sha256sum` output line for `Blackjack.bin` used as a reproducibility check.

## 3) Analysis status tracker

### Confirmed

- The canonical DiStella invocation for this project is `-pabf -cblackjack.cfg`.
- The canonical rebuild command is `dasm blackjack.s -f3 -oBlackjack.bin`.
- The investigation scope is the gameplay logic, especially RNG/shuffle behavior, within a 2 KB ROM budget.

### Open questions

- Does the dealer logic contain any explicit branch path that conditionally improves dealer outcomes when trailing?
- How uniform is the RNG output sequence over practical play windows?
- Is the shuffle/deal routine unbiased with respect to player-vs-dealer hand quality?
- Which currently uncertain RAM variable labels (for example, `$AF`) can be elevated to confirmed meanings with instruction-level evidence?

## Open documentation tasks

Ongoing review notes and follow-up improvements are tracked in:

- [`docs/analysis-roadmap.md`](docs/analysis-roadmap.md)
