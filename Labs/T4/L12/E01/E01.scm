;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; E01
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Extend the evaluator in this section to support the special form let

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

;; STk> (load "E01.scm")

; (let ((a (list 1 2 3))
;       (b (list 4 5 6)))
;      (cons a b))


