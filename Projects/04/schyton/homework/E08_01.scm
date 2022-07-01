; Write a function rotate_letters() that takes in a number and creates a new mapping of lower case letters offset by that number.
; Return the new mapping as a dictionary such that the original letter is mapped to the shifted letter.
; For example, rotate_letters(2) would map 'a'->'c', 'b'->'d', 'c'->'e' and so on.

(load "test-utils.scm")
(print ">>> Running E08_01")

(define test_rotate_letters
	(string-append
		(file->string "E08_01.py")
"
rotate_letters(2)
"
	)
)

(run-test "Offset alphabet letters by 2"
	  (run-python-string test_rotate_letters)
		"{
  \"a\" : \"c\",
  \"b\" : \"d\",
  \"c\" : \"e\",
  \"d\" : \"f\",
  \"e\" : \"g\",
  \"f\" : \"h\",
  \"g\" : \"i\",
  \"h\" : \"j\",
  \"i\" : \"k\",
  \"j\" : \"l\",
  \"k\" : \"m\",
  \"l\" : \"n\",
  \"m\" : \"o\",
  \"n\" : \"p\",
  \"o\" : \"q\",
  \"p\" : \"r\",
  \"q\" : \"s\",
  \"r\" : \"t\",
  \"s\" : \"u\",
  \"t\" : \"v\",
  \"u\" : \"w\",
  \"v\" : \"x\",
  \"w\" : \"y\",
  \"x\" : \"z\",
  \"y\" : \"a\",
  \"z\" : \"b\"
}"
)
