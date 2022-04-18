;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E01_01
; SICP 4.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Rewrite eval in data-directed style.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
; Changes:

; 1: We create the eval package that contains all of the procedures for every type of expression
; 2: We create a dispatch-eval procedure that look up the type of expression in the table and returns the corresponding procedure
; 3: We override the evaluator definition (P01_01), so the methods use dispatch-eval instead of eval. Methods changed
;   - list-of-values
;   - eval-if
;   - eval-sequence
;   - eval-assignment
;   - eval-definition
; 4: We override the interpreter execution definition (P01_04), so the method driver-loop uses dispatch-eval instead of eval

(load "eval-pkg")

; Start evaluating
(driver-loop)

;;;; TEST
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
; 
; ok
; 
; ;;; M-Eval input:
; 
; (append '(a b c) '(d e f))
; 
; ;;; M-Eval value:
; 
; (a b c d e f)
