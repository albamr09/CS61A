(load "test-utils.scm")
(print ">>> Running tests for A5")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Person A Part 1
;;; Question 5a: and/or
;;; 
;;; Special form "and' & "or" that short-circuits
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Checks for AND
(define and-true-simple
"True and True")
(run-test "and-true-simple"
	  (run-python-string and-true-simple)
	  "True")

(define and-false-simple
"True and False")
(run-test "and-false-simple"
	  (run-python-string and-false-simple)
	  "False")

(define and-true-int
"True and 3 and 5") ; Returns the last "true" thing
(run-test "and-true-int"
	  (run-python-string and-true-int)
	  "5")

(define and-false-int
"True and 3 and False")
(run-test "and-false-int"
	  (run-python-string and-false-int)
	  "False")

(define and-short-circuit
"False and (1 / 0)") ; Should short circuit and not error
(run-test "and-short-circuit"
	  (run-python-string and-short-circuit)
	  "False")

;;; Checks for OR
(define or-true-simple
"True or False")
(run-test "or-true-simple"
	  (run-python-string or-true-simple)
	  "True")

(define or-true-both
"True or True")
(run-test "or-true-both"
	  (run-python-string or-true-both)
	  "True")

(define or-false-simple
"False or False")
(run-test "or-false-simple"
	  (run-python-string or-false-simple)
	  "False")

(define or-true-int
"2 or 3 or 5")
(run-test "or-true-int"
	  (run-python-string or-true-int)
	  "2")

(define or-short-circuit
"2 or (1 / 0)")
(run-test "or-short-circuit"
	  (run-python-string or-short-circuit)
	  "2")
