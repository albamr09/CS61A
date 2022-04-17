
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
;;; M-Eval input:
; (define (append x y)
; 
; (if (null? x)
; 
; y (
; 
; cons (car x) (append (cdr x) y))))
; 
; ;;; M-Eval value:
; ok
; 
; ;;; M-Eval input:
; (append '(a b c) '(d e f))
; 
; ;;; M-Eval value:
; (a b c d e f)
