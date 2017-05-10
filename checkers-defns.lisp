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
;;   MOVE-HISTORY  --  A list CHECKERS STRUCTS containing the state of the game
;;         before the last move was made.
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

;; REMOVE-TOKEN!
;; INPUT: GAME, a checkers struct
;;        R, C, ints representing the row and col of the piece to remove
;; OUTPUT: the modified game with the piece removed and totals adjusted

(defmethod remove-token! ((game checkers) r c)
  (let* ((bored (checkers-board game))
	 (piece (aref bored r c)))
    
    ; adjust totals, including king counts if appropriate
    (cond
     ((is-red-piece? piece) ; if red, decrement red totals
      
      (decf (checkers-red-alive game))
      
      (when (is-king? piece)
	(decf (checkers-red-kings game))))
     
     ((is-black-piece? piece) ; if black, decrement black totals
      
      (decf (checkers-black-alive game))
      
      (when (is-king? piece)
	(decf (checkers-black-kings game)))))
    
    (setf (aref bored r c) nil) ; set the piece to nothing!
    
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

;; MAKE-KING?
;; INPUT: PLR, *red* or *black*
;;        R, the row of the piece in question
;; OUTPUT: T if a piece belonging to PLR which is in row R should be kinged

(defun make-king? (plr r)
  (or (and (= plr *red*) (= r 7)) ; red is kinged in row 7
      (and (= plr *black*) (= r 0)))) ; black is kinged in row 0

;; KING-ME
;; INPUT: GAME, a checkers struct
;;        PLR, *red* or *black*
;;        R, C, ints representing the row and col of piece to be kinged
;; OUTPUT: a modified version of GAME where the piece at (r c) has been kinged

(defmethod king-me ((game checkers) plr r c)
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

;; COPY-GAME
;; INPUT: GAME, a checkers struct
;; OUTPUT: a copy of GAME

(defmethod copy-game ((game checkers))
  (labels 
      ((copy-board (bored)
	 (let* ((copy (make-array '(8 8))))
	   (dotimes (r 8)
	     (dotimes (c 8)
	       (setf (aref copy r c) (aref bored r c))))
	   
	   copy)))
    
    (make-checkers :board (copy-board (checkers-board game))
		   :red-alive (checkers-red-alive game)
		   :red-kings (checkers-red-kings game)
		   :black-alive (checkers-black-alive game)
		   :black-kings (checkers-black-kings game)
		   :whose-turn? (whose-turn game)
		   :move-history (checkers-move-history game))))
  

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
  (labels 
      ;; CALC-JUMPED-LOC helper
      ;; INPUT: POSN1, POSN2, lists of form (r c)
      ;; OUTPUT: a list of the same form, representing the space diagonally
      ;;         between POSN1 and POSN2, or NIL
      ((calc-jumped-loc (posn1 posn2) 
	 (let* ((r1 (first posn1))
		(r2 (first posn2))
		(c1 (second posn1))
		(c2 (second posn2))
		(diff-r (- r2 r1)) ; (r2-r1) will always be +/- 2
		(diff-c (- c2 c1))) ; and same for (c2-c1)
	   
	   ; location of jumped piece is one slot in the (diff-r diff-c)
	   ; direction from the starting posn of the moving piece.
	   ; if no piece is jumped, return NIL.
	   
	   (if (= (abs diff-r) 2)
	       (list (+ r1 (/ diff-r 2)) (+ c1 (/ diff-c 2)))
	     nil))))
    
    (let* ((bored (checkers-board game))
	   (plr (whose-turn game))
	   (ploc (svref path 0)) ; previous/starting location
	   (endloc (svref path (- (length path) 1))) ; ending location
	   (r (first endloc))
	   (c (second endloc))
	   (piece (aref bored (first ploc) (second ploc)))
	   (move-history (checkers-move-history game)))
      
      (cond
       ((and check-legal? (not (is-legal? game path)))
	(format t "Not a legal move!~%")
	(return-from do-move! game))
       
       (t ; if we arrive here, we can assume the path is legal
	
	; add current game state to move history before we modify it
	
	(setf (checkers-move-history game) 
	  (cons (copy-game game) move-history))
	
	; have to remove pieces between spaces listed on PATH
	
	(dotimes (i (- (length path) 1))
	  
	  (let* ((posn1 (svref path i))
		 (posn2 (svref path (+ i 1)))
		 (jumped (calc-jumped-loc posn1 posn2)))
	    
	    (when jumped ; only remove token if something was jumped.
	      (remove-token! game (first jumped) (second jumped)))))
	
	; move token from starting position to final spot in path
	; leave its king status temporarily unchanged
	
	(move-token! game r c ploc plr (is-king? piece))
	
	; check if token should be a king; king it if so!
	
	(when (make-king? plr r)
	  (king-me game plr r c))
	
	; toggle the turn
	(toggle-turn! game)
	
	; return the modified game
	game)))))
      
;; UNDO-MOVE
;; INPUT: GAME, a checkers struct
;; OUTPUT: a version of the game in which the last move has
;;         been undone.

(defmethod undo-move ((game checkers))
  (first (checkers-move-history game)))
    
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
  (let ((plr (whose-turn game))
  		(bored (checkers-board game)))
  	(cond
  		;; do we need to have a condition for if player is black or red?

  		;; for every piece of the player's color:
  			;; if piece is king:
  				;; (can-jump? bored piece true ())
  				;; if ^ is nil, see if 1 forward/back/left/right is open
  					;; if so, cons move onto list of leg moves
  				;; otherwise, cons result of that function onto list of leg moves

  			;; if piece is not king:
  				;; (can-jump? bored piece false ())
				;; if ^ is nil, see if 1 forward/left/right is open
  					;; if so, cons move onto list of leg moves
  				;; otherwise, cons result of that function onto list of leg moves

  		)


    nil))


;; CAN-JUMP?
;; INPUT: GAME, a checkers struct, PIECE, a piece position, and KING?, a boolean
;; OUTPUT: a list of coords the piece can jump to, or nil

(defun can-jump? (game piece king? coords)

	;; Not sure if this will go through all possible combinations or if it
	;; will just stop at the forward left thing every time

	;; If space 1 forward and 1 left has a token of the other player's color:
		;; Look 1 forward and 1 left of that: if open and not out of bounds, cons coord to coords
		;; If new position is at edge of board and king? is false: king? becomes true
		;; (can-jump? updated-game new-position king? coords)
	;; If space 1 forward and 1 right has a token of the other player's color:
		;; Look 1 forward and 1 right of that: if open and not out of bounds, cons coord to coords
		;; If new position is at edge of board and king? is false: king? becomes true
		;; (can-jump? updated-game new-position king? coords)

	;; if king? is true:
		;; If space 1 back and 1 right has a token of the other player's color:
			;; Look 1 back and 1 right of that: if open and not out of bounds, cons coord to coords
			;; (can-jump? updated-game new-position true coords)
		;; If space 1 back and 1 left has a token of the other player's color:
			;; Look 1 back and 1 left of that: if open and not out of bounds, cons coord to coords
			;; (can-jump? updated-game new-position true coords)

	;; when can't jump, return nil

	nil)


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
  (let* ((rand-path (random-move game)))
        
    (do-move! game nil rand-path)))


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
    
    (eval-func g)))

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
  (list (checkers-red-alive game)
	(checkers-black-alive game)
	(whose-turn game)))



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
    
    ;; To normalize between -1 and 1:
    ;; If black has all 12 kings and red lost (best case), score is 60.
    ;; If red has all 12 kings and black lost (worst case), score is -60
    ;; Divide by 60 to normalize between -1 and 1
    (/ (- black-value red-value) 60)))