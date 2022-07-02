; Write a memoized accumulated factorial procedure in a similar fashion to memo_fib. You MUST use recursion.
; Accumulated Factorial of 5 = 5! * 4! * 3! * 2! * 1!

(load "test-utils.scm")
(print ">>> Running E09")

(define memoized_factorial
	(string-append
		(file->string "E09.py")
"
factorial = memo(factorial_memo)
"
	)
)

(define test_five_memoized_factorial
	(string-append
		memoized_factorial
"
factorial(5)
"
	)
)

(define test_zero_memoized_factorial
	(string-append
		memoized_factorial
"
factorial(0)
"
	)
)

(define test_ten_memoized_factorial
	(string-append
		memoized_factorial
"
factorial(10)
"
	)
)

(run-test "Memoized and recursive factorial for n = 5"
	  (run-python-string test_five_memoized_factorial)
		"120"
)

(run-test "Memoized and recursive factorial for n = 0"
	  (run-python-string test_zero_memoized_factorial)
		"1"
)

(run-test "Memoized and recursive factorial for n = 10"
	  (run-python-string test_ten_memoized_factorial)
		"3628800"
)
