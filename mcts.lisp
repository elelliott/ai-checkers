;; ========================================
;;  CMPU-365, Spring 2017
;;  Checkers project
;;  Elise Elliott and Mackenzie Little
;;  mcts.lisp
;; ========================================

;;  Calls the following domain dependent methods
;; ----------------------------------------------------
;;     COPY-GAME
;;     LEGAL-MOVES  --  returns VECTOR of legal moves
;;     MAKE-HASH-KEY-FROM-GAME
;;     WHOSE-TURN -- returns *BLACK* or *RED*
;;     GAME-OVER? -- 
;;     DEFAULT-POLICY  --  returns random legal move
;;     DO-MOVE!

;;  Defines the following functions:
;; ----------------------------------------------------------
;;     GET-ROOT-NODE
;;     NEW-MC-TREE
;;     INSERT-NEW-NODE
;;     SIM-TREE
;;     SIM-DEFAULT
;;     BACKUP
;;     UCT-SEARCH
;;     COMPETE
;;     SELECT-MOVE


;;  MC-NODE struct -- a node in MCTS tree

(defstruct mc-node
  key             ;; hash-table key:  compact rep'n of current state of game
  whose-turn      ;;  *BLACK* or *RED*
  (num-visits 0)  ;; number of times this state has been visited
  veck-moves      ;; a VECTOR of moves
  veck-visits     ;; a VECTOR recording number of times each move visited
  veck-scores     ;; a VECTOR recording average score for each move
  )


(defstruct mc-tree
  ;; hash-table:  key = compact repn of state, value = mc-node
  (hashy (make-hash-table :test #'equal))      
  root-key)

(defun get-root-node
    (tree)
  (gethash (mc-tree-root-key tree) (mc-tree-hashy tree)))


;;  NEW-MC-TREE
;; ------------------------------------------------------------
;;  INPUT:   GAME, a game struct
;;  OUTPUT:  A new MC tree whose root state is derived from GAME.

(defun new-mc-tree (game)
  (let* ((root-hash-key (make-hash-key-from-game game)))
  
    ; return the tree
    (make-mc-tree 
     ; hash table has default value
     :root-key root-hash-key)))

;;  INSERT-NEW-NODE
;; -----------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           KEY, a hash-key representing the state of the game
;;  OUTPUT:  The newly created and inserted node
;;  SIDE EFFECT:  Inserts a new node into TREE using KEY.

(defun insert-new-node (game tree key)
  (let* ((leg-moves (legal-moves game))
	 (num-leg-moves (length leg-moves))
	 (nodey (make-mc-node :key key
			      :whose-turn (whose-turn game)
			      ; initialize moves array to legal moves
			      :veck-moves leg-moves
			      ; initialize visits and scores arrays to 0s
			      :veck-visits (make-array num-leg-moves
						       :initial-element 0)
			      :veck-scores (make-array num-leg-moves
						       :initial-element 0))))
    
    ; insert the node into the given tree
    
    (setf (gethash key (mc-tree-hashy tree)) nodey)))

;;  SELECT-MOVE
;; ------------------------------------------
;;  INPUTS:  NODEY, an MC-NODE struct
;;           C, exploitation-exploration constant
;;  OUTPUT:  The INDEX of the selected move into the moves vector

(defun select-move (nodey c)
  (let ((best-mv-ind nil) ; index of best move
	(best-val nil) ; value associated with best move
	(val 0)
	(moves (mc-node-veck-moves nodey))
	(visits (mc-node-veck-visits nodey))
	(scores (mc-node-veck-scores nodey))
	(num-visits (mc-node-num-visits nodey)))
    
    (dotimes (i (length moves))
       
      (cond
       
       ; if it's black's turn...
       ((eq *black* (mc-node-whose-turn nodey))
	
	; set the value to pos-inf if the move hasn't been visited yet.
	; otherwise, do the complicated equation.
	(if (zerop (svref visits i))
	    (setf val *pos-inf*)
	  (setf val (+ (svref scores i) (* c (sqrt (/ (log num-visits)
						      (svref visits i)))))))
	
	; update best-val and best-mv-ind if they have not been updated yet,
	; or if you have found a better move (greater than old best).
	(when (or (null best-val) (> val best-val))
	  (setf best-val val)
	  (setf best-mv-ind i)))
       
       (t ; if it's red's turn...
	
	; set val to neg-inf if the move hasn't been visited yet. otherwise,
	; do the complicated equation.
	(if (zerop (svref visits i))
	    (setf val *neg-inf*)
	  (setf val (- (svref scores i) (* c (sqrt (/ (log num-visits)
						      (svref visits i)))))))
	
	; update best-val and best-mov-ind if they have not been updated yet,
	; or if you have found a better move (less than old best).
	(when (or (null best-val) (< val best-val)) 
	  (setf best-val val)
	  (setf best-mv-ind i)))))
    
    ; return the index of the best move
    
    best-mv-ind))
   
;;  SIM-TREE
;; --------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           C, the exploration/exploitation constant
;;  OUTPUT:  A list of the form (state0 move0 state1 move1 ... statek movek)
;;    where each state_i is a key into the hashtable, and each move_i
;;    is an index into the MOVES vector of the node assoc with state_i.

(defun sim-tree (game tree c)
  (let* ((current-node (get-root-node tree))
	 (current-key (mc-tree-root-key tree))
	 (acc ()))
   
    (while (not (game-over? game))
      
      ; when the node is not in the tree...
      (when (null (gethash current-key (mc-tree-hashy tree)))
	; insert it and append the key to the accumulator. then return.
	(insert-new-node game tree current-key)
	(setf acc (append acc (list current-key)))
	(return-from sim-tree acc))

      ; if the node is in the tree...
      
      (let ((moves (mc-node-veck-moves current-node))
	    (chosen-mv (select-move current-node c)))
	
	(setf acc (append acc (list current-key chosen-mv)))
	
	; destructively do the chosen move
	(do-move! game (svref moves chosen-mv))
	
	; move to next key/node
	
	(setf current-key (make-hash-key-from-game game))
	(setf current-node (gethash current-key (mc-tree-hashy tree)))))

    ; return the accumulator
    acc))

;;  SIM-DEFAULT
;; ----------------------------------------------
;;  INPUT:  GAME, a game struct
;;  OUTPUT:  The result of following the game's default policy
;;             (domain-dependent method)

(defun sim-default
    (game)
  (default-policy game))


;;  BACKUP
;; ---------------------------------------------------
;;  INPUTS:  HASHY, the hash-table for the MCTS
;;           KEY-MOVE-ACC, the accumulated list of KEYs and MOVEs
;;              from a simulation run
;;           RESULT, the result (from black's perspective) of the 
;;              recently played out simulation
;;  OUTPUT:  doesn't matter
;;  SIDE EFFECT:  Updates the relevant nodes in the MC-TREE/HASHY

(defun backup (hashy key-move-acc result)
  
  (cond
   ; empty list? done -- do nothing
   ((null key-move-acc)
    ; doesn't matter what it returns
    t)
   
   ; list contains one key, and nothing else
   ((null (rest key-move-acc))
    ; increase the num-visits at the node associated with the key
    (incf (mc-node-num-visits (gethash (first key-move-acc) hashy))))
   
   ; otherwise...
   (t 
    
    (let* ((curr-key (first key-move-acc))
	   (curr-move (second key-move-acc))
	   (curr-node (gethash curr-key hashy))
	   (visits (mc-node-veck-visits curr-node))
	   (scores (mc-node-veck-scores curr-node)))

      ; increase number of visits to this node
      (incf (mc-node-num-visits curr-node))

      ; increase number of visits to the move
      (incf (svref visits curr-move))
      
      ; increase the score for that move
      (incf (svref scores curr-move)
	    (/ (- result (svref scores curr-move))
	       (svref visits curr-move)))
      
      ; continue backing up
      (backup hashy (rest (rest key-move-acc)) result)))))
   
;;  UCT-SEARCH
;; -------------------------------------------------------
;;  INPUTS:  ORIG-GAME, a game struct
;;           NUM-SIMS, a positive integer
;;           C, the exploration/exploitation parameter
;;  OUTPUT:  The best move according to monte-carlo tree search.

(defun uct-search (orig-game num-sims c)
  (let ((tree (new-mc-tree orig-game)))
    
    (dotimes (i num-sims) ; run num-sims simulations
      
      (let* (; copy the game so it can be destructively modified
	     (g (copy-game orig-game))
	     (key-move-acc (sim-tree g tree c))
	     (result (sim-default g)))
	
	; update values in the visits/scores vectors of the nodes visited
	(backup (mc-tree-hashy tree) key-move-acc result)))
    
    ; return the move at the index returned by select move on the root node
    (svref (mc-node-veck-moves (get-root-node tree))
	   (select-move (get-root-node tree) c))))
  
;;  COMPETE
;; --------------------------------------------------
;;  INPUTS:  BLACK-NUM-SIMS, the number of simulations for each of 
;;           black's moves
;;           BLACK-C, the exploration/exploitation constant used by black
;;           RED-NUM-SIMS, the number of simulations for each of 
;;           red's moves
;;           RED-C, the exploration/exploitation constant used by red
;;  OUTPUT:  Don't care
;;  SIDE EFFECT:  Displays the entire game using UCT-SEARCH to compute 
;;    best moves for both players according to the specified parameters.

(defun compete
    (black-num-sims black-c red-num-sims red-c)
  (let ((g (init-game))
	(turns-left 125))
    (while (and (not (zerop turns-left))
		(not (game-over? g)))
      (cond
       ((eq (whose-turn g) *black*)
	(format t "BLACK'S TURN!~%")
	(format t "~A~%" 
		(do-move! g  
			  (uct-search g black-num-sims black-c))))
       (t
	(format t "RED'S TURN!~%")
	(format t "~A~%"
		(do-move! g  
			  (uct-search g red-num-sims red-c)))))
      
      (decf turns-left))
    
    (cond
     ((zerop turns-left)
      (format t "125 turns have passed!~%"))
     ((> (eval-func g) 0)
      (format t "Black won!~%"))
     (t
      (format t "Red won!~%")))))



