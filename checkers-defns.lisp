;; =====================================
;;  CS 365 Spring 2017
;;  Checkers Implementation
;;  Elise Elliott and Mackenzie Little
;;  checkers-defns.lisp
;; =====================================

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

;; MUST-PASS? - naive implementation
;; INPUT: GAME, a checkers struct
;;        PLR, *red* or *black*
;; OUTPUT: T if PLR has no legal moves

(defmethod must-pass? ((game checkers))
  (let ((leg-moves (legal-moves game)))
    (= 1 (length leg-moves)))) ; only move is pass

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
	 
