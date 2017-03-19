I read on a Reddit thread that someone thought the blackjack game on the Atari 2600 cheated by dealing itself winning hands when it was on a losing streak.

I've heard rumors like this before.  For instance my brother swore the Backgammon game on the Atari 2600 rolled itself double sixes more frequently near the end of the game.

Blackjack is only 2k of 6502 machine language, and a chunk of that is for graphics, so I'm wondering if there is any room left over to add some cheating.

But my guess is the code that makes up the random number generator and the shuffling algorithm (both difficult things to get right) might not be very good in such a small space.

Hopefully annotating this code will find out.  

Some technical details:

The code was generated using distella with the arguments -pafs

To rebuild it, use dasm with the -f3 argument: `dasm blacjack.s -f3 -oblackjack.bin`
