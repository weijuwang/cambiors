# cambio

This project contains an API for the card game Cambio, a Monte Carlo search to determine the best move from any position even when most cards are unknown, and a REPL to use the Monte Carlo search as a Cambio bot.

I previously coded part of this in C (see my repo [cambio](https://github.com/weijuwang/cambio)) and Kotlin (see [sparklingWater](https://github.com/weijuwang/sparklingWater)). C is unfortunately too unsafe, hard to write in, and probably not much faster than Rust if at all, so I abandoned it. I pretty much finished the Kotlin version, but it's slow, not completely safe, and janky because it's Kotlin. Rust seemed like the best of all worlds: Speed, safety, and easy to code in.

## Rules

There are many different variants of this game. Below are the rules I implemented in this project. It is worth reading them because there are many different variants with small differences that can affect strategy significantly.

### Goal

Each player is dealt 4 cards. With a few exceptions, players may not look at or rearrange their cards. At the end of the game, you win if the sum of the values of your cards is lower than everyone else's. Playing certain cards throughout the game allow you to peek at and trade cards so you can try to get lower cards.

### Card values
The point values of cards 2-10 are the same as their rank. Other cards' point values are specified below.

| Card                  | Value |
|-----------------------|-------|
| Ace                   | 1     |
| Jack/queen/black king | 10    |
| Red king              | -1    |
| Joker                 | 0     |

### Gameplay

#### Beginning
When you are dealt your four cards, you arrange them in a 2x2 square and are allowed to see the bottom two. This is your only opportunity to do so.

#### On your turn
Draw a card to start your turn. You have two options:
1) Discard the card you just drew.
2) Replace one of the cards you already had with the card you just drew, and then discard the replaced card.

If you chose the first option and the card you discarded was one of the ones below, you can choose to take the associated action. Taking the action is not required.

| Card                     | Action                                                                                                       |
|--------------------------|--------------------------------------------------------------------------------------------------------------|
| 7 or 8                   | Look at one of your own cards.                                                                               |
| 9 or 10                  | Look at one card of another player.                                                                          |
| Jack, queen, or red king | Swap one of any player's card with one of any other player's card.                                           |
| Black king               | Look at one card of another player. If you wish, you can then swap that card with any one of your own cards. |

If you look at a card after discarding a black king, and then that card is stuck successfully, the card is now face-up anyways, so you are allowed to choose another card and then optionally swap it as described above.

If you look at a card after discarding a black king, and then that card is given away to someone else, you do not get to choose another card; if you were to use your swap, it would still be on the card you looked at even though it now belongs to a different player.

If the draw deck is empty after drawing, shuffle the discard pile and add the cards to the draw deck.

#### Ending the game
At the beginning of your turn, before you draw a card, you may say "Cambio" when you believe you will win the game. From this point on, none of your cards can be traded or rearranged -- whatever you have is set until the end of the game. After you have called Cambio, your turn is skipped and everyone else gets one more turn. Everyone then turns over their cards, and adds up the value of their cards. Whoever has the lowest total wins.

If there is a tie, the player who did not call Cambio wins; if there is still a tie among players who did not call Cambio, the game is tied between them, and they are considered to have shared the win.

If the draw pile is empty and there are no cards in the discard pile to replace the draw pile with, the game is over immediately and scored as described above.

#### Sticking
If someone places a card in the discard pile, you may "stick" that card by taking one of your or anyone's cards that is the same rank and discarding it on top of the discarded card. It does not have to be your turn, but only one person can stick each discarded card. You must stick a discarded card before the next player draws their card. You are allowed to stick your own card.

If you stick by taking someone else's card, you can optionally give them any one of your cards (*give-away*).

If you try to stick with a card and it turns out it's not the same rank (*false-sticking*), you have to return that card to where it was originally, and you also have to draw another card as punishment. If this happens, anyone else -- including yourself -- can try to stick again with the same rules until a correct stick attempt is made.

After any stick or stick attempt is finished, the flow of play resumes as if the stick didn't happen.

In the unlikely event that a player tries so many false-sticks that they draw the entire draw deck to their pile (and eventually the discard pile when it becomes the draw deck), the game ends immediately because there are no more cards to draw from and thus no meaningful actions.

## REPL
This program will soon have a REPL to interact with it. I have not implemented this yet.
