;; ====================================
;;  CMPU-365, Spring 2017
;;  Checkers Project
;;  Elise Elliott and Mackenzie Little
;;  ab-minimax.lisp
;; ====================================

;;  STATS struct
;; ---------------------------
;;  Stats compiled during minimax search

(defstruct stats
  (num-moves-done 0)
  (num-potential-moves 0))

;;  COMPUTE-MOVE
;; -------------------------------------------------------------
;;  INPUTS:  G, a CHECKERS struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  The best move according to MINIMAX with ALPHA-BETA
;;   pruning, using the static eval func, EVAL-FUNC.  Searches to
;;   a depth of CUTOFF-DEPTH.

(defun compute-move (g cutoff-depth)
  (format t "~%COMPUTE-MOVE (cutoff=~A)~%" cutoff-depth)
  (let* ((statty (make-stats))
	 (comp-max (compute-max g 0 *neg-inf* *pos-inf* statty cutoff-depth))
	 (my-move (first comp-max))
	 (alpha-val (second comp-max)))

    (format t "   Root node alpha: ~A~%" alpha-val)
    (format t "   Num Moves Done: ~A, " (stats-num-moves-done statty))
    (format t "Num Moves Pruned: ~A~%" (- (stats-num-potential-moves statty)
					   (stats-num-moves-done statty)))
    (format t "   Best Move: ~A~%" my-move)

    ;; return my-move
    my-move))
   

;;  COMPUTE-MAX / COMPUTE-MIN
;; ---------------------------------------------------------------
;;  INPUTS:  G, a CHECKERS struct
;;           CURR-DEPTH, the current depth in the search
;;           ALPHA, BETA, alpha/beta values for this node in search
;;           STATTY, stats struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  If CURR-DEPTH is zero, returns best move
;;           Otherwise returns value of this node according
;;           to MINIMAX with ALPHA-BETA pruning.

(defun compute-max (g curr-depth alpha beta statty cutoff-depth)
  (let* ((moves (legal-moves g))
	 (best-move nil)
	 (new-alpha alpha)) ; new-alpha is initially the old alpha
    
    (cond
     ((game-over? g) ; if game is over, opponent moved and ended game.
      ; you lose!
      (+ curr-depth *loss-value*))
     
     ((= curr-depth cutoff-depth) ; if at cutoff-depth, return static eval.
      (eval-func g))
     
     (t ; otherwise...
      (incf (stats-num-potential-moves statty) (length moves))
      
      (dolist (mv moves)
	(apply #'do-move! g nil mv) ; DO the move!
	
	(let ((old-alpha new-alpha) ; save unaltered alpha val
	      (comp-min ; save result of compute-min
	       (compute-min g (1+ curr-depth) 
			    new-alpha beta statty cutoff-depth)))
	  
	  (setf new-alpha ; new alpha val is max of old alpha & val of comp-min
	    (max old-alpha comp-min))
	  
	  (setf g (undo-move g)) ; UNDO the move!
	  
	  (when (< old-alpha new-alpha)	; found a better move
	    (setf best-move mv))
	  
	  (incf (stats-num-moves-done statty) 1) ; increase moves done by 1
	  
	  (when (<= beta new-alpha) ; when parent beta <= new alpha
	    ; can prune! don't do any more moves from here.
	    (if (= curr-depth 0) ; return list of best move and alpha at root
		(return-from compute-max (list best-move new-alpha))
	        (return-from compute-max new-alpha))))) ; otherwise alpha val
      
      (if (= curr-depth 0) ; return list of best move and alpha if at root
	  (list best-move new-alpha)
	  new-alpha))))) ; otherwise simply return alpha val

;;  COMPUTE-MIN
;; -------------------------------------------------------
;;  INPUTS:  G, a CHECKERS struct
;;           CURR-DEPTH, the depth of this MIN node
;;           ALPHA, BETA, values received from parent MAX node
;;           STATTY, a stats struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  The value of this MIN node according to rules
;;           of MINIMAX with ALPHA-BETA pruning

(defun compute-min (g curr-depth alpha beta statty cutoff-depth)
  (let ((moves (legal-moves g))
	(new-beta beta)) ; new-beta is initially the old beta
    
    (cond
     ((game-over? g) ; you just moved and ended the game.
      ; you win!
      (- *win-value* curr-depth))
     
     ((= curr-depth cutoff-depth) ; if at cutoff-depth, return static eval.
      (eval-func g))
     
     (t ; otherwise... 
      (incf (stats-num-potential-moves statty) (length moves))
      
      (dolist (mv moves)
	(apply #'do-move! g nil mv) ; DO the move!
	
	(let ((old-beta new-beta) ; save unaltered beta
	      (comp-max
	       (compute-max g (1+ curr-depth) 
			    alpha new-beta statty cutoff-depth)))

	  (setf new-beta ; new-beta is min of old beta and val of comp-max
	    (min old-beta comp-max))
	
	  (setf g (undo-move g)) ; UNDO the move!
	  
	  (incf (stats-num-moves-done statty) 1) ; increase moves done by 1
	  
	  (when (<= new-beta alpha) ; can prune! return new beta val.
	    (return-from compute-min new-beta))))
      
      ; return beta value
      new-beta))))


