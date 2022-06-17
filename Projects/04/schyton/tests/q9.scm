(load "test-utils.scm")
(print ">>> Running tests Question 6")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Exercise Part 3
;;; Question 9: memoize
;;; 
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fib_memo (string-append 
		  (file->string "../memoize.py")
		  "
def __fib(x, memo):
    if x <= 1:
        return x
    else:
        return memo(x-1) + memo(x-2)
" 
"
fib_memo=memoize(__fib)
"
))

(run-test "fib-memo-base"
	  (run-python-string (string-append fib_memo
					    "fib_memo(1)"))
	  "1")

(run-test "fib-memo-small"
	  (run-python-string (string-append fib_memo
					    "fib_memo(10)"))
	  "55")

(newline)
(print ">>> WARNING: Note that for large values, the result of calling fib might be off because of overflow/rounding error")
(run-test "fib-memo-medium"
	  (run-python-string (string-append fib_memo
					    "fib_memo(50)"))
	  "12586269025")

(run-test "fib-memo-large"
	  (> (string->number (run-python-string (string-append fib_memo
							       "fib_memo(100)")))
	     300000000000000000000)
	  #t)
