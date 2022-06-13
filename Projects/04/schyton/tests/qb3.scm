(load "test-utils.scm")
(print ">>> Running tests for Exercise B3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Person B Part 1
;;; Question 3b: get-num
;;; 
;;; Collects numbers, including decimals
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-num-int
"314")
(run-test "get-num-int"
	  (run-python-string get-num-int)
	  "314")

(define get-num-float
"3.14")
(run-test "get-num-float"
	  (run-python-string get-num-float)
	  "3.14")

;;; 3.foo should treat 3 as an int with a method call foo
;;; Not possible using the current construct
#|
(define get-num-int-str
"3.foo")
(run-test "get-num-int-str"
	  (tokens (run-python-read get-num-int-str))
	  '(3 .foo))
|#

(define get-num-float-str
"3.14.foo")
(run-test "get-num-float-str"
	  (tokens (run-python-read get-num-float-str))
	  '(3.14 .foo))
