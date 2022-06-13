(load "test-utils.scm")
(print ">>> Running tests Question 1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Exercise Part 1
;;; Question 1: Ignore-Comment
;;; 
;;; Checks if comments are ignored for newline, block
;;; nested and eof
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ignore-comment-newline 
"x = 5 # I am a comment IGNORE ME
x")
(run-test "ignore-comment-newline" 
	  (run-python-string ignore-comment-newline)
	  "5")

(define ignore-comment-block
"def square(x): # This should be ignored
   return x*x

square(5)")

(run-test "ignore-comment-block"
	  (run-python-string ignore-comment-block)
	  "25")

(define ignore-comment-nest
"x = 3 # Om # nom nom
x")
(run-test "ignore-comment-nest"
	  (run-python-string ignore-comment-nest)
	  "3")

(define ignore-comment-eof
"5 # This line does not end with a newline")
(run-test "ignore-comment-eof"
	  (run-python-string ignore-comment-eof)
	  "5")
