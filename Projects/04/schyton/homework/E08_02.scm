; Write a function decode_cipher() that takes in a dictionary of letter mappings and a cipher string (of only lower case letters).
; Return the decoded string that is created when every character is replaced by its mapping from the dictionary
; For example, decode_cipher(rotate_letters(2), "abc") should return "cde".
; Use this function to decode "jbj fpurzr vf terng" given that the letters had been shifted by 13.

(load "test-utils.scm")
(print ">>> Running E08_02")

(define test_decode_cesar
	(string-append
		(file->string "E08_01.py")
		(file->string "E08_02.py")
"
decode_cipher(rotate_letters(13), \"jbj fpurzr vf terng\")
"
	)
)

(run-test "Decode string with Cesar Cipher"
	  (run-python-string test_decode_cesar)
		"wow scheme is great"
)
