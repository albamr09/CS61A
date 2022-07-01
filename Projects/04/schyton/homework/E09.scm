; Write a memoized accumulated factorial procedure in a similar fashion to memo_fib. You MUST use recursion.
; Accumulated Factorial of 5 = 5! * 4! * 3! * 2! * 1!

(load "test-utils.scm")
(print ">>> Running E09")

(define test_decode_cesar
	(string-append
		(file->string "E09.py")
"
decode_cipher(rotate_letters(13), \"jbj fpurzr vf terng\")
"
	)
)

(run-test "Decode string with Cesar Cipher"
	  (run-python-string test_decode_cesar)
		"wow scheme is great"
)
