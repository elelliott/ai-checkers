;; =====================================
;;  CS 365 Spring 2017
;;  Checkers Implementation
;;  Elise Elliott and Mackenzie Little
;;  checkers-defns.lisp
;; =====================================

;;  GLOBAL CONSTANTS
;; ----------------------------------------

;;  The players / pieces

(defconstant *black* 0)
(defconstant *red* 1)

(defconstant *bking* 2)
(defconstant *rking* 3)

; vector used by print-checkers to display pieces:
;      indexed by the pieces defined above
(defconstant pieces #(B R K L))

;;  WIN-LOSS VALUES
;; ----------------------------------------

(defconstant *win-value* 400000)
(defconstant *loss-value* -400000)

;;  NEGATIVE and POSITIVE INFINITY
;; ----------------------------------------

(defconstant *neg-inf* -10000000)
(defconstant *pos-inf*  10000000)


;;  CHECKERS struct
;; -----------------------------------------------------------------------
;;  Fields:
;;   BOARD  --  An 8-by-8 array containing 
;;              *red*, *black*, *rking*, *bking*, or nil
;;   RED-ALIVE, BLACK-ALIVE  --  Number of red/black tokens on the board
;;   RED-KINGS, BLACK-KINGS  --  Number of red/black kings on the board
;;   WHOSE-TURN?  --  Either *red* or *black*
;;   MOVE-HISTORY  --  A list of the moves that got us from initial 
;;      state to the current state
;; -----------------------------------------------------------------------
;;  NOTE:  Red's home rows are 0, 1, 2.
;;         Black's home rows are 5, 6, 7.

(defstruct (checkers (:print-function print-checkers))
  (board (make-array '(8 8) :initial-element nil))
  (red-alive 12)
  (red-kings 0)
  (black-alive 12)
  (black-kings 0)
  (whose-turn? *black*) ; black moves first
  (move-history nil) ; for alpha-beta minimax
  )
    
;;  INIT-GAME
;;  INPUTS:  None
;;  OUTPUT:  A CHECKERS struct corresponding to a new game of checkers

    
(defun init-game ()
  (let ((game (make-checkers)))
    (dotimes (r 8)
      (dotimes (c 8)
	
	(cond
	 ;; red pieces go on first 3 rows of dark squares
	 ((or (and (evenp r) (oddp c) (< r 3))
	      (and (oddp r) (evenp c) (< r 3)))
	  
	  (move-token! game r c nil *red* nil))
	 
	 ;; black pieces go on last 3 rows of dark squares
	 ((or (and (evenp r) (oddp c) (> r 4))
	      (and (oddp r) (evenp c) (> r 4)))
	  
	  (move-token! game r c nil *black* nil)))))
		   
    ;; RETURN THE GAME!
    game))


;;  UTILITY FUNCTIONS
;; ------------------------------------------------------------------------

;; WHOSE-TURN
;; INPUT: GAME, a checkers struct
;; OUTPUT: *red* or *black*

(defmethod whose-turn ((game checkers))
  (checkers-whose-turn? game))

;; CHOOSE-PIECE
;; INPUT: PLR, *red* or *black*
;;        KING?, T if desired piece is a king
;; OUTPUT: *red*, *black*, *bking*, or *rking* depending on PLR and KING?

(defun choose-piece (plr king?)
  (cond
   ((and king? (= plr *black*))
    *bking*)
   ((and king? (= plr *red*))
    *rking*)
   ((= plr *black*)
    *black*)
   (t
    *red*)))

;; MOVE-TOKEN!
;; INPUT: GAME, a checkers struct
;;        R, C, ints representing the row and col where token will be moved
;;        PLOC, a list of the form (row col) representing the token's 
;;              previous location
;;        PLR, *red* or *black*
;;        KING?, T if token to be moved is a king
;; OUTPUT: a modified version of GAME where the appropriate piece has been
;;         placed at (r c) and ploc has been set to nil.

(defmethod move-token! ((game checkers) r c ploc plr king?)
  (let* ((bored (checkers-board game))
	 (piece (choose-piece plr king?)))
    
    ; set (r c) to plr's piece, king if appropriate
    (setf (aref bored r c) piece)
    
    ; set board at ploc to nil if ploc is non-nil
    (when ploc
      (setf (aref bored (first ploc) (second ploc)) nil))
    
    ; return the game
    game))

;; IS-RED-PIECE?
;; INPUT: PIECE, *red*, *black*, *bking*, *rking*, or nil

(defun is-red-piece? (piece)
  (if piece
      (oddp piece)
    nil))

;; IS-BLACK-PIECE?
;; INPUT: PIECE, *red*, *black*, *bking*, *rking*, or nil

(defun is-black-piece? (piece)
  (if piece
      (evenp piece)
    nil))
    
;; IS-KING?
;; INPUT: PIECE, *red*, *black*, *bking*, *rking*, or nil
;; OUTPUT: T if PIECE is *bking* or *rking*

(defun is-king? (piece)
  (if piece
      (> piece 1)
      nil))

;; MAKE-KING
;; INPUT: GAME, a checkers struct
;;        PLR, *red* or *black*
;;        R, C, ints representing the row and col of piece to be kinged
;; OUTPUT: a modified version of GAME where the piece at (r c) has been kinged

(defmethod make-king ((game checkers) plr r c)
  (let ((bored (checkers-board game))
	(piece (choose-piece plr t)))
    
    ; set (r c) to kinged piece
    (setf (aref bored r c) piece)
    
    ; increase number of kings for correct player
    (if (= plr *black*)
	(incf (checkers-black-kings game))
        (incf (checkers-red-kings game)))
    
    game))

;; FIND-KINGS
;; INPUT: GAME, a checkers struct
;;        PLR, the player whose kings you want to find
;; OUTPUT: a vector of pairs (r c) representing the locations of the kings
;;         for PLR. 

(defmethod find-kings ((game checkers) plr)
  (let ((bored (checkers-board game))
	(acc nil))
    
    (dotimes (r 8)
      (dotimes (c 8)
	
	(let ((piece (aref bored r c)))
	  
	  (when (and (is-king? piece)
		     (or (and (= plr *black*) (is-black-piece? piece))
			 (and (= plr *red*) (is-red-piece? piece))))
	    
	    (setf acc (cons (list r c) acc))))))
    
    (make-array (length acc) :initial-contents acc)))
	    
;; TOGGLE-TURN
;; INPUT: GAME, a checkers struct
;; OUTPUT: none
;; SIDE EFFECT: sets the whose-turn? field of the checkers struct
;;              to *red* if currently *black* and vice versa

(defmethod toggle-turn! ((game checkers))
  (if (= *black* (whose-turn game))
      (setf (checkers-whose-turn? game) *red*)
    (setf (checkers-whose-turn? game) *black*)))

;; MUST-PASS? - naive implementation
;; INPUT: GAME, a checkers struct
;;        PLR, *red* or *black*
;; OUTPUT: T if PLR has no legal moves

(defmethod must-pass? ((game checkers))
  (let ((leg-moves (legal-moves game)))
    (= 1 (length leg-moves)))) ; only move is pass


;;  GAMEPLAY FUNCTIONS
;; ------------------------------------------------------------------------

;;  DO-MOVE!
;;  INPUTS:  GAME, a checkers struct
;;           CHECK-LEGAL?, T or NIL
;;           PATH, a vector with elements (r c) representing a token's path
;;           of movement to its new location
;;  OUTPUT:  a modified version of GAME where the the piece at the first
;;           posn on the path has been moved to the last posn in path,
;;           and any pieces which were jumped have been removed
;;  SIDE EFFECT:  Destructively modifies GAME by doing the specified move.
;;    Note:  If CHECK-LEGAL? is T, then it only does the move if IS-LEGAL?
;;           succeeds.

(defmethod do-move! ((game checkers) check-legal? path)
  (let* ((bored (checkers-board game))
	 (plr (whose-turn game))
	 (ploc (svref path 0)) ; previous/starting location
	 (piece (aref bored (first ploc) (second ploc))))
    
    (cond
     ((and check-legal? (not (is-legal? game path)))
      (format t "Not a legal move!~%")
      (return-from do-move! game))
     
     (t	; if we arrive here, we can assume the path is legal
      
      
      ;; have to remove any pieces between spaces on PATH
      ;; have to update counts in game struct accordingly
      ;;       ---> helper: (remove-token! game r c) TODO
      ;; make king if necessary
      ;;       ---> if red, token should be kinged if in row 7
      ;;            if black, in row 0
      ;;            helper: (make-king ...) already done
      ;; have to MOVE-TOKEN from first spot in path to last
      ;;       ---> (move-token! ...) implemented
      ;; ADD PATH TO MOVE HISTORY (how should move history be stored?)
      ))))
      
;; UNDO-MOVE!
;; INPUT: GAME, a checkers struct
;; OUTPUT: a modified version of the game in which the last move has
;;         been undone.
;; SIDE EFFECT: destructively modifies GAME

(defmethod undo-move! ((game checkers))
    nil)
   
;; GAME-OVER?
;; INPUT: GAME, a checkers struct
;; OUTPUT: T if the game is over (i.e. at least one player must pass)

(defmethod game-over? ((game checkers))
  (let ((pass? (must-pass? game)))
    
    (when (not pass?) ; if current player doesn't have to pass,
      (toggle-turn! game) ; check other player.
      (setf pass? (must-pass? game))
      (toggle-turn! game)) ; return game state to correct player
    
    ; pass? if T if at least one player must pass their turn.
    pass?))

;; LEGAL-MOVES
;; INPUT: GAME, a checkers struct
;; OUTPUT: a vector of the legal moves (in the form (r c)) for current player

(defmethod legal-moves ((game checkers))
  (let ((plr (whose-turn game)))
    nil))


;;  DISPLAY FUNCTION
;; ------------------------------------------------------------------------

;; PRINT-CHECKERS
;; used by the checkers struct to nicely display a game of checkers.

(defun print-checkers (game str depth)
  (declare (ignore depth))
  
  (let ((bored (checkers-board game))
	(red-live (checkers-red-alive game))
	(black-live (checkers-black-alive game))
	(plr (whose-turn game)))
    
    (format str "    0 1 2 3 4 5 6 7~%")
    (format str "  -------------------~%")
    
    (dotimes (r 8)
      (format str "~A:  " r)
      (dotimes (c 8)
	(let ((elt (aref bored r c)))
	  (if elt
	      (format str "~A " (svref pieces elt))
	    (format str "_ "))))
      (format str "~%"))
    
    (format str "  -------------------~%")
    
    (format str "Red Alive: ~A, Black Alive: ~A~%" red-live black-live)
    (format str "It's ~A's turn!~%"
	    (if (= *black* plr) "black" "red"))))
	 
;;  FUNCTIONS FOR MCTS
;; ------------------------------------------------------------------------

;MCTS: random-move, do-random-move!, default-policy,
;    make-hash-key-from-game


<<<<<<< HEAD
;;  RANDOM-MOVE
;; ------------------------------------------
;;  INPUT:  GAME, a CHECKERS struct
;;  OUTPUT:  One of the legal moves available to the current
;;   player, chosen randomly.

(defmethod random-move ((game checkers))
  (let* ((leg-moves (legal-moves game))
	 (rand-int (random (length leg-moves))))

    ; return the legal move at a random index
    (svref leg-moves rand-int)))


;;  DO-RANDOM-MOVE!
;; ------------------------------------------------
;;  INPUT:   GAME, a CHECKERS struct
;;  OUTPUT:  The modified game
;;  SIDE EFFECT:  Destructively modifies GAME by doing one of the 
;;   legal moves available to the current player, chosen randomly.

(defmethod do-random-move! ((game checkers))
  (let* ((rand-mv (random-move game)))
        
    (do-move! game nil (first rand-mv) (second rand-mv))))


;;  DEFAULT-POLICY
;; ---------------------------------------------------------------------------
;;  INPUT:  GAME, a CHECKERS struct
;;  OUTPUT: The result (from black's perspective) of playing out the
;;    current game using randomly selected moves.  The value has been
;;    normalized between -1 and 1, where negative values mean red won
;;    and positive values mean black won

(defmethod default-policy ((game checkers))
  (let ((g (copy-game game)))
    
    ; perform random moves on the copy of the game until game is over
        
    (while (not (game-over? g))
      (do-random-move! g))
    
    ; game is over
    
    (let ((num-red (checkers-red-alive game))
		(num-black (checkers-black-alive game))
		(red-kings (checkers-red-kings game))
		(black-kings (checkers-black-kings game))
		(red-value 0)
		(black-value 0))

    	; kings are worth 5 additional points each
    	(incf red-value (+ num-red (* 5 red-kings)))
    	(incf black-value (+ num-black (* 5 black-kings)))

    	;; 60 means a victory with all kings and no pieces lost
    	(let* ((diff (- black-value red-value))
    		   (score (/ diff 60)))
      
      (if (< diff 0) ; if black lost,
	  (* score -1) ; return negative score
	  score))))) ; otherwise positive score


;;  MAKE-HASH-KEY-FROM-GAME
;; --------------------------------------------
;;  INPUT:  GAME, a CHECKERS struct
;;  OUTPUT:  A list of the form (WHITE-PIECES BLACK-PIECES WHOSE-TURN)
;;    where the contents are as described in the STATE struct


;; ********* NOT DONE ****************
(defmethod make-hash-key-from-game
    ((game checkers))
    ;; How are we representing piece locations? othello uses 64-bit ints, but we just
    ;; have counts of the pieces? should we implement that?
  (list (checkers-red-pieces game)
	(checkers-black-pieces game)
	(checkers-whose-turn game)))



=======
>>>>>>> origin/master
;;  EVAL-FUNC FOR A/B MINIMAX
;; ------------------------------------------------------------------------

;; EVAL-FUNC 
;; INPUT: GAME, a checkers struct 
;; OUTPUT: a number representing how favorable GAME is from 
;;         black's perspective

(defmethod eval-func ((game checkers))
  (let ((num-red (checkers-red-alive game))
	(num-black (checkers-black-alive game))
	(red-kings (checkers-red-kings game))
	(black-kings (checkers-black-kings game))
	(red-value 0)
	(black-value 0))
    
    ; kings are worth 5 additional points each
    (incf red-value (+ num-red (* 5 red-kings)))
    (incf black-value (+ num-black (* 5 black-kings)))
    
<<<<<<< HEAD
    ;; To normalize between 0 and 1:
    ;; If black has all 12 kings and red lost(best case), score is 60.
	;; If red has all 12 kings and black lost(worst case), score is -60
	;; Divide by 60 to normalize between -1 and 1
    (/ (- black-value red-value) 60)
    ))



=======
    (- black-value red-value))) ;; NORMALIZE??
>>>>>>> origin/master

