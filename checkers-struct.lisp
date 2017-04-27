;; Elise Elliott and Mackenzie Little
;; Checkers Implementation


;;  GLOBAL CONSTANTS

;;  The players

(defconstant *red* 0)
(defconstant *black* 1)

;;  WIN-LOSS VALUES

(defconstant *win-value* 400000)
(defconstant *loss-value* -400000)

;;  NEGATIVE and POSITIVE INFINITY
;; ----------------------------------------

(defconstant *neg-inf* -10000000)
(defconstant *pos-inf*  10000000)


;;  CHECKERS struct
;; -----------------------------------------------------------------------
;;  Fields:
;;   BOARD   --  An 8-by-8 array containing PIECE structs or NIL
;;   PIECES  --  A 2-by-12 array for accessing pieces (even if not on board)
;;   WHOSE-TURN?  --  Either *red* or *black*
;;   EVAL-SUBTOTALS -- Vector containing two values:  the sum of the
;;      values of *red*'s pieces, and the sum of the values of *black*'s
;;      pieces
;;   MOVE-HISTORY  -- A list of the moves that got us from initial 
;;      state to the current state
;; -----------------------------------------------------------------------
;;  NOTE:  Red's home rows are 0 and 1.
;;         Black's home rows are 6 and 7.

(defstruct (checkers (:print-function print-checkers))
  (board (make-array '(8 8) :initial-element nil))
  (pieces (make-array '(2 12) :initial-element nil))
  (whose-turn? *red*)
  (eval-subtotals (vector 0 0))
  (move-history nil)
  )

game struct
    board (R,B,r,b) - 8x8 2d array
    (red-pieces, black-pieces?)
    pieces (each slot represents a dark board spot?)
    king-pieces
    whose-turn
    move-history
    

;;  INIT-GAME
;; ----------------------------------------------------------------
;;  INPUTS:  None
;;  OUTPUT:  A CHECKERS struct corresponding to a new game of checkers

    
(defun init-game (&optional (red-info *red-info*)
			    (red-info *black-info*))
  (let* ((game (make-checkers))
	 (ctr 0))
    ;; Create all of the pieces
    (dolist (triple *red-info*)
      (let ((p-type (first triple)))
	(create-but-dont-set-piece game *red* p-type ctr)
	(create-but-dont-set-piece game *black* p-type ctr)
	(incf ctr)))

    (dolist (triple red-info)
      (let ((p-type (first triple))
	    (row (second triple))
	    (col (third triple)))
	;; Find first piece of this type 
	(let ((pc (find-piece game *red* p-type)))
	  (setf (piece-row pc) row)
	  (setf (piece-col pc) col)
	  (put-piece! game pc))))
    (dolist (triple black-info)
      (let ((p-type (first triple))
	    (row (second triple))
	    (col (third triple)))
	(let ((pc (find-piece game *black* p-type)))
	  (setf (piece-row pc) row)
	  (setf (piece-col pc) col)
	  (put-piece! game pc))))
	;; RETURN THE GAME!
    game))



do-move! / undo-move!


;;  DO-MOVE!
;; -------------------------------------
;;  INPUTS:  GAME, a CHECKERS struct
;;           CHECK-LEGAL?, T or NIL
;;           ROW, COL, two integers (between 0 and 7)
;;  OUTPUT:  The modified GAME
;;  SIDE EFFECT:  Destructively modifies GAME by doing the specified move.
;;    Note:  If CHECK-LEGAL? is T, then it only does the move if it
;;           passes the IS-LEGAL? check.

(defmethod do-move! ((game checkers) check-legal? row col)
  (cond 
   ; if we're checking that the move is legal and it is not, quit!
   ((and check-legal? (not (is-legal? game row col)))
    (format t "(~A, ~A) is not a legal move!~%" row col)
    (return-from do-move! game))
   ; otherwise...
   (t
    
    ; place the current player's token at the given posn, if not *pass*
    
    (when (not (is-pass? (list row col)))
      
      (place-token game (othello-board game) (whose-turn game) row col)
      (decf (othello-num-open game))
      
      ; flip all tokens in any legal direction to the current player's token
      (dotimes (i (length coords))
	
	(let* ((dir (svref coords i))
	       (r-mod (first dir))
	       (c-mod (second dir))
	       (new-r (+ row r-mod))
	       (new-c (+ col c-mod))
	       (bored (othello-board game))
	       (plr (whose-turn game)))
	  
	  ; if it's a legal direction, we need to flip the other player's
	  ; tokens which are on that path
	  
	  (when (is-legal-direction? game row col dir)
	    
	    (while (eq (other-player plr)
		       (aref bored new-r new-c))
	      	      
	      (flip-token game bored plr new-r new-c)
	      
	      (setf new-r (+ new-r r-mod))
	      (setf new-c (+ new-c c-mod)))))))
      
      ; toggle current player and return the game struct
      
      (toggle-player! game)
      game)))

toggle-turn!

game-over?

legal-moves (is-legal?)

eval-func (count pieces, kings count as more, normalize?)

display/print

MCTS: random-move, do-random-move!, default-policy,
    make-hash-key-from-game

a/b: just eval func?