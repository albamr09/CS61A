(load "test-utils.scm")
(print ">>> Running tests for Question B4")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Person B Part 1
;;; Question 4b: negate-bool
;;; 
;;; Negates boolean
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-test "negate-bool-true"
	  (ask (negate-bool *PY-TRUE*) 'true?)
	   #f)

(run-test "negate-bool-false"
	  (ask (negate-bool *PY-FALSE*) 'true?)
	  #t)
