(load "test-utils.scm")
(print ">>> Running tests Question 8")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Exercise Part 3
;;; Question 8: dictionary
;;; 
;;; collects key-value
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dict-simple
  "{1 : 2}")

(run-test "dict-simple"
	  (run-python-string dict-simple)
	  "{
  1 : 2
}")

(define dict-two-kv
  "{1 : 2, 2 : 3}")
(run-test "dict-two-kv"
	  (run-python-string dict-two-kv)
	  "{
  1 : 2,
  2 : 3
}")

(define dict-empty
  "{}")
(run-test "dict-empty"
	  (run-python-string dict-empty)
	  "{
}")

(define dict-string
  "{'hello':'world'}")
(run-test "dict-string"
	  (run-python-string dict-string)
	  "{
  \"hello\" : \"world\"
}")

(define dict-error-colon
  "{1 : 2 : 3}")
(run-test "dict-error-colon"
	  (catch (run-python-string dict-error-colon))
	  #t)

(define dict-error-comma
  "{1 : 2 , 3 , 4}")
(run-test "dict-error-comma"
	  (catch (run-python-string dict-error-comma))
	  #t)

(define dict-in
  "1 in {1 : 2}")
(run-test "dict-in"
	  (run-python-string dict-in)
	  "True")

(define dict-not-in
  "0 not in {1 : 2}")
(run-test "dict-not-in"
	  (run-python-string dict-not-in)
	  "True")
