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
2.  To see all legal moves, type (legal-moves g).
3.  To do a move, type (do-move! g CHECK-LEGAL? PATH)
    where CHECK-LEGAL? is T if you want to see if your move is legal,
    and PATH is a vector of the form #((r0 c0) (r1 c1) ... (rk ck))
    where each (rj cj) is a row-column pair. (r0 c0) should be the
    coordinates of the piece you wish to move. Each subsequent (rj cj)
    is where you would place the token after a jump.
4.  To see if the game is over, type (game-over? g).
5.  To undo your last move, type (setf g (undo-move g)).

TO CALCULATE YOUR NEXT MOVE:


