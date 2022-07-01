; Write a function char_freq() that takes a string and builds a frequency listing of the characters contained 
; in it. Represent the frequency listing as a Python dictionary with each letter as a key that stores the number 
; of times that letter appears.

; Try it with something like char_freq("abbabcbdbabdbdbabababcbcbab").

(load "test-utils.scm")
(print ">>> Running E07")

(define test_char_freq
	(string-append
		(file->string "E07.py")
"
char_freq(\"abbabcbdbabdbdbabababcbcbab\")
"
	)
)

(run-test "Character frequency on 'abbabcbdbabdbdbabababcbcbab'"
	  (run-python-string test_char_freq)
		"{
  \"a\" : 7,
  \"b\" : 14,
  \"c\" : 3,
  \"d\" : 3
}"
)
