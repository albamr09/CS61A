; Write a program that prints the integers from 1 to n (n is an argument to the procedure). 
; But for multiples of three print "Fizz" instead of the number, and for the multiples of 
; five print "Buzz". For numbers which are multiples of both three and five print "FizzBuzz".

(load "test-utils.scm")
(print ">>> Running E03")

(define fizz_five
	(string-append
		(file->string "E03.py")
"
fizzbuzz(5)
"
	)
)

(define fizz_longer
	(string-append
		(file->string "E03.py")
"
fizzbuzz(27)
"
	)
)

(run-test "FizzBuzz for n=5"
	  (run-python-string fizz_five)
	  " Fizz Buzz"
)

(run-test "FizzBuzz for n=27"
	  (run-python-string fizz_longer)
	  " Fizz Buzz Fizz Fizz Buzz Fizz FizzBuzz Fizz Buzz Fizz Fizz Buzz Fizz"
)
