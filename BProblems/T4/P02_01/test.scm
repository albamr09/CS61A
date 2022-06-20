
; Expressions
(load "P01_02.scm")
; Environment
(load "P01_03.scm")
; Load eval
(load "P01_01.scm")
; Load interpreter
(load "P01_04.scm")

(driver-loop)

;;; TEST IN INTERACTIVE MODE
;; $ stk -l ../../lib/simply.scm 

;; STk> (load "P01_04.scm")
;;; L-Eval input:
;;; (define (try a b) (if (= a 0) 1 b))
;;; Because of lazy loading this does not print an error, if you try in
;;; any of the other interpreters this will make the interpreter crash
;;; (try 0 (/ 1 0))
