(load "test-utils.scm")
(print ">>> Running tests for Exercise A3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Person A Part 1
;;; Question 3a: Get-String
;;; 
;;; Checks if double/single quotes are used correctly
;;; to collect strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-string-single
"'This string uses single quote'")
(run-test "get-string-single"
	  (run-python-string get-string-single)
	  "This string uses single quote")

(define get-string-double 
"\"This string uses double quote\"")
(run-test "get-string-double"
	  (run-python-string get-string-double)
	  "This string uses double quote")

(define get-string-nest-single
"\"This string 'has nested single quote'\"")
(run-test "get-string-nest-single"
	  (run-python-string get-string-nest-single)
	  "This string 'has nested single quote'")

(define get-string-nest-double
"'This string \"has nested double quote\"'")
(run-test "get-string-nest-double"
	  (run-python-string get-string-nest-double)
	  "This string \"has nested double quote\"")

(define get-string-error-single
"'this is a string 'this is not' this is a string'")
(run-test "get-string-error-single"
	  (catch (run-python-string get-string-error-single))
	  #t)

(define get-string-error-double
"\"this is a string \"this is not\" this is a string\"")
(run-test "get-string-error-double"
	  (catch (run-python-string get-string-error-double))
	  #t)
