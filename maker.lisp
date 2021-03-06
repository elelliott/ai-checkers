;; =====================================
;;  CS365 Spring 2017
;;  Maker file
;;  Elise Elliott and Mackenzie Little
;;  maker.lisp
;; =====================================

(defparameter *file-list*
    (list "maker" 
	  "checkers-utility"
	  "checkers-defns"
	  "checkers-search"
	  "mcts"
	  "ab-minimax"))

;; USE THIS FUNCTION TO COMPILE AND LOAD ALL FILES

(defun maker ()
  ;; COMPILER FLAGS
  (setq compiler:tail-call-self-merge-switch t)
  (setq compiler:tail-call-non-self-merge-switch t) 
  (dolist (file *file-list*)
    (compile-file file)
    (load file)))