;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E04_01
; SICP 4.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Notice that we cannot tell whether the metacircular evaluator evaluates operands from le to right or from
; right to le. Its evaluation order is inherited from the underlying Lisp: If the arguments to cons in list-of-values
; are evaluated from le to right, then list-of-values will evaluate operands from le to right; and if the arguments to
; cons are evaluated from right to le, then list-of-values will evaluate operands from right to le.

; Write a version of list-of-values that evaluates operands from left to right regardless of the order of evaluation in the
; underlying Lisp. Also write a version of list-of-values that evaluates operands from right to left

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Changes:
; 1. Create two implementations for first-operand and rest-operands, so list-of-values forcefuly evaluates 
;			to right or vice versa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Expression representation 
(load "./interpreter/P01_02.scm")
; Environment
(load "./interpreter/P01_03.scm")
; Load changed eval
(load "./interpreter/P01_01.scm")
; Load interpreter
(load "./interpreter/P01_04.scm")

; Run interpreter
(driver-loop)

;;;;;;;;;;;;;;
; TEST
;;;;;;;;;;;;;;

; To test make sure the first-operand and rest-operands methods are uncommented properly

; Left to right
(- 1 2)
; -1
; Right to left
(- 1 2)
; 1
; Left to right
(- 2 3 4 5 4)
; -14
; Right to left
(- 2 3 4 5 4)
; -10
