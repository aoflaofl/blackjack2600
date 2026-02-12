### Disassembly of Atari 2600 Blackjack

I read on Reddit that someone thought the Blackjack game on the Atari 2600 cheated by dealing itself winning hands when it was losing.

I've heard rumors like this before.  For instance my brother swore the Backgammon game on the Atari 2600 rolled itself double sixes more frequently near the end of the game.

Blackjack is only 2k of 6502 machine code and a chunk of that is for graphics, so I'm wondering if there is any room left over to add a cheating feature.

My guess is the code that makes up the random number generator and the shuffling algorithm (both difficult things to get right) might not be very good in such a small space.

Hopefully annotating this code will find out.  

Some technical details:

The code was generated using DiStella with the arguments `-pabf -cblackjack.cfg`

To rebuild it, use DASM with the `-f3` argument: `dasm blackjack.s -f3 -oBlackjack.bin`

## Review notes and suggested improvements

While reading through the current annotations, I found a few documentation mistakes and opportunities to improve the reverse-engineering notes:

1. Keep command lines consistent across files.
   - The old README used `-pafs`, but the disassembly header in `blackjack.s` records `-pabf -cblackjack.cfg`.
   - The old README build command also had a filename typo (`blacjack.s`).

2. Tighten technical wording in comments.
   - Prefer "Television Interface Adapter" over misspellings.
   - In the playfield bit layout sketch, the register label should be `PF0 PF1 PF2`, not `PF0 PF1 PF1`.

3. Improve reproducibility and confidence in analysis.
   - Add a short "verification" section showing the expected ROM hash after rebuilding.
   - Add a running table of resolved RAM variables (address, purpose, evidence), so uncertain notes such as "$AF = Number of players?" can be systematically confirmed.
   - Split high-level narrative (rumor/background) from technical workflow (tool versions, commands, and outputs) to make the project easier to follow for future contributors.
