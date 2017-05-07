;; Elise Elliott and Mackenzie Little
;; Checkers Implementation


;;  GLOBAL CONSTANTS

;;  The players / pieces

(defconstant *black* 0)
(defconstant *red* 1)

(defconstant *bking* 2)
(defconstant *rking* 3)

(defconstant pieces #(B R K L))

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
;;   BOARD   --  An 8-by-8 array containing 
;;                      *red*, *black*, *rking*, *bking*, or nil
;;   WHOSE-TURN?  --  Either *red* or *black*
;;   MOVE-HISTORY  -- A list of the moves that got us from initial 
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
;; ----------------------------------------------------------------
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

;; PLACE-TOKEN
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
    
    (setf (aref bored r c) piece)
    
    (if (= plr *black*)
	(incf (checkers-black-kings game))
        (incf (checkers-red-kings game)))
    
    game))

;do-move! / undo-move!


;;  DO-MOVE!
;; -------------------------------------
;;  INPUTS:  GAME, a CHECKERS struct
;;           CHECK-LEGAL?, T or NIL
;;           ROW, COL, two integers (between 0 and 7)
;;  OUTPUT:  The modified GAME
;;  SIDE EFFECT:  Destructively modifies GAME by doing the specified move.
;;    Note:  If CHECK-LEGAL? is T, then it only does the move if it
;;           passes the IS-LEGAL? check.

(defmethod do-move! ((game checkers) check-legal? r c)
  nil)

;; TOGGLE-TURN
;; INPUT: GAME, a checkers struct
;; OUTPUT: none
;; SIDE EFFECT: sets the whose-turn? field of the checkers struct
;;              to *red* if currently *black* and vice versa

(defmethod toggle-turn! ((game checkers))
  (if (= *black* (checkers-whose-turn? game))
      (setf (checkers-whose-turn? game) *red*)
    (setf (checkers-whose-turn? game) *black*)))

;game-over?

;legal-moves (is-legal?)

;; EVAL-FUNC
;; INPUT: GAME, a checkers struct
;; OUTPUT: a number representing how favorable GAME is from black's perspective

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
    
    (- black-value red-value))) ;; NORMALIZE??

;; PRINT-CHECKERS
;; used by the checkers struct to nicely display a game of checkers.

(defun print-checkers (game str depth)
  (declare (ignore depth))
  
  (let ((bored (checkers-board game))
	(red-live (checkers-red-alive game))
	(black-live (checkers-black-alive game))
	(plr (checkers-whose-turn? game)))
    
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
	 

;MCTS: random-move, do-random-move!, default-policy,
;    make-hash-key-from-game

;a/b: just eval func?
