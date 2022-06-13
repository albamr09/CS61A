(load "test-utils.scm")
(print ">>> Running tests for Exercise A4")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Person A Part 1
;;; Question 4a: contains
;;; 
;;; Checks if a python list can check for "contains"
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Checks if contains work for ints
(define test-num-1 (make-py-num 3))
(define test-num-2 (make-py-num 2))
(define contains-int-list (make-py-list (list (make-py-num 3) (make-py-num 4)
					      (make-py-num 5) (make-py-num 6))))

(run-test "contains-int-true"
	  (ask (ask contains-int-list '__contains__ test-num-1) 'true?)
	  #t)

(run-test "contains-int-false"
	  (ask (ask contains-int-list '__contains__ test-num-2) 'true?)
	  #f)

;;; Checks if contains work for strings
(define test-string-1 (make-py-string "hello"))
(define test-string-2 (make-py-string "world"))
(define contains-string-list (make-py-list (list (make-py-string "hella") 
						 (make-py-string "world"))))

(run-test "contains-string-false"
	  (ask (ask contains-string-list '__contains__ test-string-1) 'true?)
	  #f)
(run-test "contains-string-true"
	  (ask (ask contains-string-list '__contains__ test-string-2) 'true?)
	  #t)

