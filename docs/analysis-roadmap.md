# Analysis roadmap

This file tracks documentation cleanup tasks and follow-up reverse-engineering work so the top-level README can stay focused.

## Review notes and suggested improvements

1. Keep command lines consistent across files.
   - The old README used `-pafs`, but the disassembly header in `blackjack.s` records `-pabf -cblackjack.cfg`.
   - The old README build command also had a filename typo (`blacjack.s`).

2. Tighten technical wording in comments.
   - Prefer "Television Interface Adapter" over misspellings.
   - In the playfield bit layout sketch, the register label should be `PF0 PF1 PF2`, not `PF0 PF1 PF1`.

3. Improve reproducibility and confidence in analysis.
   - Add a short verification section showing the expected ROM hash after rebuilding.
   - Add a running table of resolved RAM variables (address, purpose, evidence), so uncertain notes such as "$AF = Number of players?" can be systematically confirmed.
   - Keep high-level narrative separate from technical workflow (tool versions, commands, outputs).
