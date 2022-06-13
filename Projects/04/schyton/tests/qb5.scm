(load "test-utils.scm")
(print ">>> Running tests for Exercise B5")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Person B Part 1
;;; Question 5b: in/not in
;;; 
;;; Checks for membership in a list
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Checks for "in" infix-operator
(define in-int-true 
"1 in [1, 2, 3]")
(run-test "in-int-true"
	  (run-python-string in-int-true)
	  "True")

(define in-int-false
"4 in [1,2,3]")
(run-test "in-int-false"
	  (run-python-string in-int-false)
	  "False")

(define in-string-true
"'hello' in ['hello','world']")
(run-test "in-string-true"
	  (run-python-string in-string-true)
	  "True")

(define in-string-false
"'hella' in ['hello','world']")
(run-test "in-string-false"
	  (run-python-string in-string-false)
	  "False")

;;; Checks for "not in" infix-operator

(define not-in-int-true
"4 not in [1,2,3]")
(run-test "not-in-int-true"
	  (run-python-string not-in-int-true)
	  "True")

(define not-in-int-false
"1 not in [1, 2, 3]")
(run-test "not-in-int-false"
	  (run-python-string not-in-int-false)
	  "False")

(define not-in-string-true
"'hella' not in ['hello','world']")
(run-test "not-in-string-true"
	  (run-python-string not-in-string-true)
	  "True")

(define not-in-string-false
"'hello' not in ['hello','world']")
(run-test "not-in-string-false"
	  (run-python-string not-in-string-false)
	  "False")
