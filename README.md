# ai-checkers
AI Project, Elise Elliott and Mackenzie Little: Implementation of 
Checkers with MCTS and alpha-beta minimax.

TO COMPILE AND LOAD:

1.  Open an interactions window in the checkers directory.
2.  Run ":cl maker" to compile and load the maker file.
3.  Call the maker function: (maker)
4.  All the appropriate files should now be loaded.

TO CREATE AND PLAY A GAME:

1.  (setf g (init-game)) creates a new game of checkers.
2.  To see all legal moves, type (legal-moves g), or
    (legal-moves-with-chains g) to see the legal moves that include
    chained jumps.
3.  To do a move, type (do-move! g PATH)
    where PATH is a vector of the form #((r0 c0) (r1 c1) ... (rk ck))
    where each (rj cj) is a row-column pair. (r0 c0) should be the
    coordinates of the piece you wish to move. Each subsequent (rj cj)
    is where you would place the token after a jump.
4.  To see if the game is over, type (game-over? g).
5.  To undo your last move, type (undo-move! g).

TO CALCULATE YOUR NEXT MOVE WITH MCTS:
Run (uct-search g NUM-SIMS C) with your chosen values.

TO CALCULATE YOUR NEXT MOVE WITH A/B MINIMAX:
Run (compute-move g CUTOFF-DEPTH). We recommend a cutoff-depth
of no more than 10 for time reasons.

TO RUN A GAME WITH MCTS:
Run (compete NUM-BLACK-SIMS BLACK-C NUM-RED-SIMS RED-C).

TO RUN A GAME WITH A/B MINIMAX:
Run (compute-do-and-show-n-moves g NUM-TURNS CUTOFF-DEPTH).

TO RUN BLACK WITH MCTS AND RED WITH A/B MINIMAX:
Run (compete-methods NUM-BLACK-SIMS BLACK-C CUTOFF-DEPTH).

NOTE: (legal-moves-with-chains g) is not the default legal
      moves function because when used with MCTS and A/B minimax
      it leads to stack overflow. However, it can be used by
      the player to view legal moves with chained jumps.
