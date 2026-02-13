# RAM Ledger

This ledger tracks current understanding of RIOT RAM variables used by `blackjack.s`.
It is seeded from the top-of-file RAM map comments and from unresolved behavior near `LF07C`, `LF214`, and `LF215`.

| Address | Current name/alias | Observed role | Evidence (routine labels + brief behavioral note) | Confidence |
|---|---|---|---|---|
| `$86` | P1 chip count (high BCD pair) | Per-player chip total digits (upper pair) | `LF2FA`: compared against `#$0A`, copied into `$D6,X`, reset during setup cycle. | probable |
| `$89` | P1 chip count (low BCD pair) | Per-player chip total digits (lower pair) | `LF2FA`: copied into `$E1,X` before `$86/$89` get sentinel setup values. | probable |
| `$8F` | player bet amount (BCD) | Bet value storage | Top-of-file RAM map notes bet in BCD; no contradicting usage observed in sampled routines. | unknown |
| `$AF` | active player index | Player selector for paddle/card kernel | `LF07C`: masked to `0..3`; `LF215`: loaded into `X` then used as `INPT0,X` paddle source. | probable |
| `$B0/$B1` | right-card pointer | Indirect pointer to right card glyph bytes | `LF214` comment block + `LF215`: `LDA ($B0),Y` feeds displayed right card. | confirmed |
| `$B2/$B3` | center-card/display pointer | Indirect pointer for center card/digit graphics | Top RAM map and `LF215`: `LDA ($B2),Y` rendered every kernel pass. | confirmed |
| `$B4/$B5` | left-card pointer | Indirect pointer to left card glyph bytes | `LF214` block + `LF215`: `LDA ($B4),Y` feeds left card rendering. | confirmed |
| `$BA/$BB` | tens/left color pointer | Pointer used for center/left overlay digits and/or color source | Top RAM map says tens pointer; `LF19E` ORs `($BA),Y` into digit stream; `LF215` uses `$BA` as left card color. | probable |
| `$BC/$BD` | ones-digit pointer | Pointer used for center ones digit | Top RAM map says ones-digit pointer; `LF19E` ORs `($BC),Y` into center digit composition. | probable |
| `$BE/$BF` | left-player chip display pointer | Pointer used in chip/message composition | Top RAM map + `LF19E`: ORs `($BE),Y` with right-side message/digit data. | probable |
| `$C2` | paddle accumulator / next-bet scratch | BCD accumulator updated during paddle scan kernel | Top RAM map says "next bet"; `LF215` runs `LDA $C2 / ADC $C9 / STA $C2` while paddle carry state is active. | probable |
| `$C4/$C5` | center-left display pointer | Pointer for left part of center column digit composition | Top RAM map + `LF19E`: `LDA ($C4),Y` staged into `X` and emitted to `GRP1`. | probable |
| `$C6/$C7` | left message pointer / kernel line counter alias | Pointer for left-side message data; low byte later reused as decrementing loop state | Top RAM map + `LF19E`: `LDA ($C6),Y` for message draw; post-kernel at `LF265`: `DEC $C6` controls return to `LF1E6`. | probable |
| `$C8` | table/descriptor cursor | Cursor through descriptor bytes for display setup | `LF145`/`LF1F6`: loaded to `X`, incremented as bytes are decoded into `$B0..$B8`, compared against `#$88`. | probable |
| `$C9` | paddle delta term | BCD term added to `$C2` per scanline pair | `LF215`: added to `$C2` immediately after paddle carry-derived arithmetic context. | unknown |
| `$D0..$D?` | RAM init block | Initialized state copied from ROM table `LF7F3` | `RAMINIT`: `LDA LF7F3,X` then `STA $D0,X` across descending `X`. | confirmed |
| `$F2/$F3` | button/reset gating bytes | Control bytes ANDed into `$CA..$CD` display control values | `LF07C` path sets/clears on button edges; `LF122` uses `$F2 AND $F3 AND $B0` before lookup/xor into `$CA,X`. | probable |

## Notes

- When comments in `blackjack.s` need tentative wording for these addresses, prefer referring back to this ledger instead of repeating guesses inline.
