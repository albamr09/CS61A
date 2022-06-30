; Write a program called snow_white that takes in two numbers as arguments, the first is the 
; num_chants, the second is the max_sing.

; The program: 
; 1. prints "heigh" "ho" alternatingly 
; 2. prints "its off to work we go" after num_chants of "heigh" or "ho" 
; 3. stops printing after having "it's off to work we go" max_sing times

; EXAMPLE: should print it's off to work we go between every 5 alternating his and hos, for a maximum of 2 times

(load "test-utils.scm")
(print ">>> Running E04")

(define simple_snow_white
	(string-append
		(file->string "E04.py")
"
snow_white(5,10)
"
	)
)

(define bigger_snow_white
	(string-append
		(file->string "E04.py")
"
snow_white(13,35)
"
	)
)

(run-test "Snow White for num_chants=5 max_sing=10"
	  (run-python-string simple_snow_white)
		"ho heigh ho heigh ho its off to work we go heigh ho heigh ho heigh its off to work we go "
)

(run-test "Snow White for num_chants=13 max_sing=35"
	  (run-python-string bigger_snow_white)
		"ho heigh ho heigh ho heigh ho heigh ho heigh ho heigh ho its off to work we go heigh ho heigh ho heigh ho heigh ho heigh ho heigh ho heigh ho heigh ho heigh ho heigh ho heigh ho its off to work we go "
)
