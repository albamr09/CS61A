(load "test-utils.scm")
(print ">>> Running E02")

(run-test "E02"
	  (run-python-string (file->string "E02.py"))
	  "apples bananas and carrots"
)
