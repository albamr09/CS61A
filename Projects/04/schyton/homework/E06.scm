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

(define false_catdog
	(string-append
		(file->string "E06.py")
"
cat_dog('catcat')
"
	)
)

(define complex_true_catdog
	(string-append
		(file->string "E06.py")
"
cat_dog('1cat1cadodog')
"
	)
)

(run-test "Same number of cat and dog"
	  (run-python-string true_catdog)
		"True"
)

(run-test "Different number of cat and dog"
	  (run-python-string false_catdog)
		"False"
)

(run-test "Same number of cat and dog on string with gibberish"
	  (run-python-string complex_true_catdog)
		"True"
)

