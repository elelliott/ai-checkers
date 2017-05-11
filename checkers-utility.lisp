;; =====================================
;;  CS 365 Spring 2017
;;  Checkers Utility Functions
;;  Elise Elliott and Mackenzie Little
;;  checkers-utility.lisp
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

; the PASS move
(defconstant pass #(nil nil))

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

;; CHECK-PIECE-PLR
;; INPUT: PIECE, *red*, *black*, *bking*, *rking*, or nil
;;        PLR, *red* or *black*
;; OUTPUT: T if PIECE belongs to PLR

(defun check-piece-plr (piece plr)
  (or (and (= plr *red*) 
	   (is-red-piece? piece))
      (and (= plr *black*) 
	   (is-black-piece? piece))))
  
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

;; ON-BOARD?
;; INPUT: R, C, ints representing a (potential) spot on a checkers board
;; OUTPUT: T if (r c) is a valid spot on the board

(defun on-board? (r c)
  (and (<= 0 r 7)
       (<= 0 c 7)))

;; GET-FORWARD-DIR
;; INPUT: PLR, *red* or *black*
;; OUTPUT: -1 if *black*, 1 if *red*

(defun get-forward-dir (plr)
  (if (= plr *black*)
      -1
    1))

;; GET-DIAGS
;; INPUT: R, C, ints representing a spot on a checkers board
;;        DIR, 1 or -1
;; OUTPUT: a vector containing the coords of the 2 diagonals in the DIR
;;         direction relative to R

(defun get-diags (r c dir)
  (let ((new-r (+ r dir))
	(diags (make-array 2 :initial-element nil)))
    
    (when (on-board? new-r (- c 1)) ; make sure left diagonal is on the board
      (setf (svref diags 0) (list new-r (- c 1)))) ; save it if so
    
    (when (on-board? new-r (+ c 1)) ; same for right diagonal!
      (setf (svref diags 1) (list new-r (+ c 1))))
    
    ; left diag is always in index 0; right diag in index 1
    diags))

;; FIND-JUMPS
;; INPUT: GAME, a checkers struct
;;        R, C, ints representing a slot on the game board
;; OUTPUT: a list of possible jumps for the token at (r c), or nil

(defmethod find-jumps ((game checkers) r c)
  (let* ((bored (checkers-board game))
	 (plr (whose-turn game))
	 ; the "forward" row modifier for plr (-1 or 1)
	 (forward-dir (get-forward-dir plr))
	 ; backward row modifier (for kings)
	 (backward-dir (* -1 forward-dir))
	 ; the directions of the possible jump paths relative to C
	 (dir-vec #(-1 1))
	 ; the forward diagonals, if they exist
	 (f-diags (get-diags r c forward-dir))
	 ; the backward diagonals, if they exist (for kings)
	 (b-diags (get-diags r c backward-dir))
	 (jumps nil))
    
    (labels 
	((check-diags (diags dir)
	   (dotimes (i (length diags)) ; for each potential diagonal...
	     (let* ((d (svref diags i))
		    (d-r (first d))
		    (d-c (second d)))
	       
	       (when d ; if d is non-nil...
		 (let ((diag-piece (aref bored (first d) (second d))))
		   
		   ; check if diag-piece belongs to the other player
		   (when (or (and (= plr *red*) 
				  (is-red-piece? diag-piece))
			     (and (= plr *black*) 
				  (is-black-piece? diag-piece)))
		     
		     ; if so, calculate the new slot the token 
		     ; at (r c) would occupy
		     (let ((new-slot (list (+ d-r dir) 
					   (+ d-c (svref dir-vec i)))))
		       
		       ; then, if the new slot is on the board and empty,
		       (when (and (on-board? (first new-slot)
					     (second new-slot))
				  (null (aref bored 
					      (first new-slot) 
					      (second new-slot))))
			 
			 ; add it to the jumps list.
			 (setf jumps (cons new-slot jumps)))))))))))
      
      ; always check forward diagonals
      (check-diags f-diags forward-dir)
      
      ; if king piece, check backward diagonals
      (when (is-king? (aref bored r c))
	(check-diags b-diags backward-dir))
      
      ; return all possible jumps
      jumps)))

;; LEGAL-MOVES
;; INPUT: GAME, a checkers struct
;; OUTPUT: a vector of the legal moves (in the form (r c)) for current player

(defmethod legal-moves ((game checkers))
  (let ((plr (whose-turn game))
	(bored (checkers-board game))
	(total-moves nil)
	(curr-move nil))

    (dotimes (r 8) ; for every slot on the board...
      (dotimes (c 8)
	
	  (when (check-piece-plr (aref bored r c) plr)	    
	    (let ((jumps (find-jumps game r c)))

	      (cond
	       (jumps ; jumps is non-nil
		
		(dolist (j jumps)
		 nil))
	       
	       (t ; jumps is nil: have to accumulate all one space moves
		
		(let* ((forward-dir (get-forward-dir plr))
		       (f-diags (get-diags r c forward-dir))
		       (b-diags nil)
		       (all-diags f-diags))
		  
		  ; if king, ALL-DIAGS must include backward diagonals
		  (when (is-king? (aref bored r c))
		    (setf b-diags (get-diags r c (* -1 forward-dir)))
		  
		    (setf all-diags 
		      (make-array 4 :initial-contents (list 
						       (svref f-diags 0)
						       (svref f-diags 1)
						       (svref b-diags 0)
						       (svref b-diags 1)))))
		  
		  ; for every valid diagonal, add the path to it from (r c)
		  ; to the total moves
		  (dotimes (i (length all-diags))
		    (let ((d (svref all-diags i)))
		      
		      (when (and d (null (aref bored (first d) (second d))))
		
			(setf total-moves 
			  (cons 
			   (make-array 2 :initial-contents (list (list r c) d))
			   total-moves))))))))))))
	      
    (make-array ; include PASS in output vector.
     (+ 1 (length total-moves)) :initial-contents (cons pass total-moves))))
