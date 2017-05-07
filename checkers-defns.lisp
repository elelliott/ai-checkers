;; Elise Elliott and Mackenzie Little
;; Checkers Implementation


;;  GLOBAL CONSTANTS

;;  The players

(defconstant *black* 0)
(defconstant *red* 1)

(defconstant *bking* 2)
(defconstant *rking* 3)

(defconstant pieces #(b r K L))

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
;;                      *red*, *black*, *rking*, *bking*, or *blank*
;;   KING-POSNS -- necessary?
;;   WHOSE-TURN?  --  Either *red* or *black*
;;   MOVE-HISTORY  -- A list of the moves that got us from initial 
;;      state to the current state
;; -----------------------------------------------------------------------
;;  NOTE:  Red's home rows are 0, 1, 2.
;;         Black's home rows are 5, 6, 7.

(defstruct (checkers (:print-function print-checkers))
  (board (make-array '(8 8) :initial-element nil))
  ;(king-posns nil) ; initially no kings -- vector of two lists:
                   ; first list black kings, second red, each of form (r c)
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
	 ((or (and (evenp r) (oddp c) (< r 3))
	      (and (oddp r) (evenp c) (< r 3)))
	  
	  (move-token game r c nil *red* nil))
	 
	 ((or (and (evenp r) (oddp c) (> r 4))
	      (and (oddp r) (evenp c) (> r 4)))
	  
	  (move-token game r c nil *black* nil)))))
		   
    ;; RETURN THE GAME!
    game))

;; CHOOSE-PIECE

(defmethod choose-piece ((game checkers) plr king?)
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

(defmethod move-token ((game checkers) r c ploc plr king?)
  (let* ((bored (checkers-board game))
	 ;(kings (svref (checkers-kings-posns game) plr))
	 (piece (choose-piece game plr king?)))
    
    ; set (r c) to plr's piece, king if appropriate
    (setf (aref bored r c) piece)
    
    ; set ploc = (pr pc) to nil if ploc non-nil
    (when ploc
      (setf (aref bored (first ploc) (second ploc)) nil))
    
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

(defmethod toggle-turn! ((game checkers))
  (if (= *black* (checkers-whose-turn? game))
      (setf (checkers-whose-turn? game) *red*)
    (setf (checkers-whose-turn? game) *black*)))

;game-over?

;legal-moves (is-legal?)

;eval-func (count pieces, kings count as more, normalize?)

;; DISPLAY

(defun print-checkers (game str depth)
  (declare (ignore depth))
  
  (let* ((bored (checkers-board game))
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
    
    (format str "  -------------------~%")))
	 

;MCTS: random-move, do-random-move!, default-policy,
;    make-hash-key-from-game

;a/b: just eval func?
