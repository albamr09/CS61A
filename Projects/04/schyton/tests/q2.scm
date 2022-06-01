(load "test-utils.scm")
(print ">>> Running tests Question 2")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Exercise Part 2
;;; Question 2: get-indent
;;; 
;;; Checks if indents are counted correctly
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-indent-no-indent
"x = 3")
(run-test "get-indent-no-indent"
	  (indentation (run-python-read get-indent-no-indent))
	  0)

(define get-indent-one-indent
" x = 3")
(run-test "get-indent-one-indent"
	  (indentation (run-python-read get-indent-one-indent))
	  1)

(define get-indent-three-indent
"   x = 3")
(run-test "get-indent-three-indent"
	  (indentation (run-python-read get-indent-three-indent))
	  3)
