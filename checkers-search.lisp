;; =====================================
;;  CS 365 Spring 2017
;;  Checkers Functions for MCTS/AB
;;  Elise Elliott and Mackenzie Little
;;  checkers-search.lisp
;; =====================================


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
        
    (do-move! game rand-path)))


;;  DEFAULT-POLICY
;; ---------------------------------------------------------------------------
;;  INPUT:  GAME, a CHECKERS struct
;;  OUTPUT: The result (from black's perspective) of playing out the
;;    current game using randomly selected moves.  The value has been
;;    normalized between -1 and 1, where negative values mean red won
;;    and positive values mean black won

(defmethod default-policy ((game checkers))
  (let ((g (copy-game game))
	(turns-left 50))
    
    ; perform random moves on the copy of the game until game is over
        
    (while (and (not (zerop turns-left))
		(not (game-over? g)))
      (do-random-move! g)
      (decf turns-left))
    
    ; game is over
    
    (eval-func g)))

;;  MAKE-HASH-KEY-FROM-GAME
;; --------------------------------------------
;;  INPUT:  GAME, a CHECKERS struct
;;  OUTPUT:  A list containing the board and whose turn it is

(defmethod make-hash-key-from-game ((game checkers))
  (list (checkers-board game)
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
	(black-value 0)
	(plr (whose-turn game)))
    
    ; kings are worth 5 additional points each
    (incf red-value (+ num-red (* 5 red-kings)))
    (incf black-value (+ num-black (* 5 black-kings)))
    
    (cond
     ((not (zerop (- black-value red-value)))
      (/ (- black-value red-value) 12))
     
     ; below here, red-value = black-value
     ((= plr *black*) ; player is black; favor red
      (/ -5 12))
     
     (t ; player is red; favor black
      (/ 5 12)))))