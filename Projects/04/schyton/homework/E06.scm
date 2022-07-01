; Write a program that return True if the string "cat" and "dog" appear the same number of times 
; in the given string.

; cat_dog('catdog') → True
; cat_dog('catcat') → False
; cat_dog('1cat1cadodog') → True

(load "test-utils.scm")
(print ">>> Running E06")

(define true_catdog
	(string-append
		(file->string "E06.py")
"
cat_dog('catdog')
"
	)
)

(run-test "Same number of cat and dog"
	  (run-python-string true_catdog)
		"True"
)

