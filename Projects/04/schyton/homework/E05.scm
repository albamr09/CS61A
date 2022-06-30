; Write a function called push_first_odd_back that takes in a list as an argument 
; This function should place the first odd number at the back of the input list. 
; Do not return a new list - in fact this function shouldn't return anything, 
; it should only modify the input list. (Hint: use the while loop)

(load "test-utils.scm")
(print ">>> Running E05")

(define test_push_first_odd_back
	(string-append
		(file->string "E05.py")
"
lst = [1, 2, 3, 4, 5, 6]
push_first_odd_back(lst)
lst
"
	)
)

(define second_test_push_first_odd_back
	(string-append
		(file->string "E05.py")
"
lst = [8, 2, 4, 9, 1, 2, 5, 9]
push_first_odd_back(lst)
lst
"
	)
)

(run-test "Push first odd back for [1, 2, 3, 4, 5, 6]"
	  (run-python-string test_push_first_odd_back)
		"[2, 3, 4, 5, 6, 1]"
)

(run-test "Push first odd back for [8, 2, 4, 9, 1, 2, 5, 9]"
	  (run-python-string second_test_push_first_odd_back)
		"[8, 2, 4, 1, 2, 5, 9, 9]"
)
