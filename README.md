### Disassembly of Atari 2600 Blackjack

I read on Reddit that someone thought the Blackjack game on the Atari 2600 cheated by dealing itself winning hands when it was losing.

I've heard rumors like this before.  For instance my brother swore the Backgammon game on the Atari 2600 rolled itself double sixes more frequently near the end of the game.

Blackjack is only 2k of 6502 machine code and a chunk of that is for graphics, so I'm wondering if there is any room left over to add a cheating feature.

My guess is the code that makes up the random number generator and the shuffling algorithm (both difficult things to get right) might not be very good in such a small space.

Hopefully annotating this code will find out.  

Some technical details:

The code was generated using distella with the arguments -pafs

To rebuild it, use dasm with the -f3 argument: `dasm blacjack.s -f3 -oblackjack.bin`
