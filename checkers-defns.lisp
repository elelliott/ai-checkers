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
;;           PATH, a vector with elements (r c) representing a token's path
;;           of movement to its new location
;;  OUTPUT:  a modified version of GAME where the the piece at the first
;;           posn on the path has been moved to the last posn in path,
;;           and any pieces which were jumped have been removed
;;  SIDE EFFECT:  Destructively modifies GAME by doing the specified move.
;;    Note:  If CHECK-LEGAL? is T, then it only does the move if IS-LEGAL?
;;           succeeds.

(defmethod do-move! ((game checkers) path)
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
    
    ; RETURN GAME IF MOVE IS PASS
    (when (null (svref path 0))
      (toggle-turn! game)
      (return-from do-move! game))
    
    (let* ((bored (checkers-board game))
	   (plr (whose-turn game))
	   (ploc (svref path 0)) ; previous/starting location
	   (endloc (svref path (- (length path) 1))) ; ending location
	   (r (first endloc))
	   (c (second endloc))
	   (piece (aref bored (first ploc) (second ploc)))
	   (move-history (checkers-move-history game)))
      	
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
	
      (when (and (make-king? plr r) (not (is-king? piece)))
	(king-me game plr r c))
	
      ; toggle the turn
      (toggle-turn! game)
	
      ; return the modified game
      game)))
      
;; UNDO-MOVE!
;; INPUT: GAME, a checkers struct
;; OUTPUT: a version of the game in which the last move has
;;         been undone.

(defmethod undo-move! ((game checkers))
  (let* ((new-g (first (checkers-move-history game)))
	 (bored (checkers-board new-g))
	 (red-alive (checkers-red-alive new-g))
	 (black-alive (checkers-black-alive new-g))
	 (red-kings (checkers-red-kings new-g))
	 (black-kings (checkers-black-kings new-g))
	 (turn (whose-turn new-g))
	 (move-history (checkers-move-history new-g)))
    
    (setf (checkers-board game) bored)
    (setf (checkers-red-alive game) red-alive)
    (setf (checkers-black-alive game) black-alive)
    (setf (checkers-red-kings game) red-kings)
    (setf (checkers-black-kings game) black-kings)
    (setf (checkers-whose-turn? game) turn)
    (setf (checkers-move-history game) move-history)))
    

;; MUST-PASS? - naive implementation
;; INPUT: GAME, a checkers struct
;;        PLR, *red* or *black*
;; OUTPUT: T if PLR has no legal moves

(defmethod must-pass? ((game checkers))
  (let ((leg-moves (legal-moves game)))
    (null (svref (svref leg-moves 0) 0)))) ; only move is pass

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
;; OUTPUT: a vector of the vectors containing legal paths,
;;         with moves containing no jump or one jump

(defmethod legal-moves ((game checkers))
  (let ((plr (whose-turn game))
	(bored (checkers-board game))
	(total-moves nil))

    (dotimes (r 8) ; for every black slot on the board...
      (dotimes (c 8)
	
	(when (or (and (evenp r) (oddp c))
		  (and (evenp c) (oddp r)))
	  
	  (let ((piece (aref bored r c)))
	    
	    (when (check-piece-plr piece plr)	    
	      (let ((jumps (find-jumps game r c (is-king? piece))))
		
		(cond
		 (jumps
		  ; jumps is non-nil -- at least one jump MUST be taken
		  
		  (dolist (j jumps)
		      
		    ; for every valid jump, add the path to it from (r c)
		    ; to the total moves
		    (setf total-moves
		      (cons (make-array 2 :initial-contents (list (list r c) j))
			    total-moves))))
		 
		 (t ; jumps is nil: have to accumulate all one space moves
		  
		  (let ((all-diags (get-all-diags r c plr bored)))
		    
		    ; for every valid diagonal, add the path to it from (r c)
		    ; to the total moves
		    (dotimes (i (length all-diags))
		      (let ((d (svref all-diags i)))
			
			(when (and d (null (aref bored (first d) (second d))))
			  
			  (setf total-moves 
			    (cons 
			     (make-array 2 :initial-contents (list (list r c) d))
			     total-moves))))))))))))))
      
    (if (null total-moves)
	(make-array 1 :initial-element pass)
      (make-array (length total-moves) :initial-contents total-moves))))

;; LEGAL-MOVES-WITH-CHAINS
;; INPUT: GAME, a checkers struct
;; OUTPUT: a vector of the vectors containing legal paths,
;;         including chained jumps

(defmethod legal-moves-with-chains ((game checkers))
  (let ((plr (whose-turn game))
	(bored (checkers-board game))
	(total-moves nil))

    (dotimes (r 8) ; for every black slot on the board...
      (dotimes (c 8)
	
	(when (or (and (evenp r) (oddp c))
		  (and (evenp c) (oddp r)))
	  
	  (let ((piece (aref bored r c)))
	    
	    (when (check-piece-plr piece plr)	    
	      (let ((jumps (find-jump-chains game r c (is-king? piece))))
		
		(cond
		 ((> (length jumps) 0)
		  ; jumps is non-nil -- at least one jump MUST be taken
		  
		  (dotimes (i (length jumps))
		    (let ((j (svref jumps i)))
		      
		      ; for every valid jump, add the path to it from (r c)
		      ; to the total moves
		      (setf total-moves
			(cons (make-array (length j) :initial-contents j)
			      total-moves)))))
		 
		 (t ; jumps is nil: have to accumulate all one space moves
		  
		  (let ((all-diags (get-all-diags r c plr bored)))
		    
		    ; for every valid diagonal, add the path to it from (r c)
		    ; to the total moves
		    (dotimes (i (length all-diags))
		      (let ((d (svref all-diags i)))
			
			(when (and d (null (aref bored (first d) (second d))))
			  
			  (setf total-moves 
			    (cons 
			     (make-array 2 :initial-contents (list (list r c) d))
			     total-moves))))))))))))))
      
    (if (null total-moves)
	(make-array 1 :initial-element pass)
        (make-array (length total-moves) :initial-contents total-moves))))