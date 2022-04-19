;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E01_03
; SICP 4.7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Implement the same as ../E01_03.scm but in data directed programming

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Changes:

; 1. We added the expression type for let* and its corresponding evaluation
;     to the table of expression types-evaluation rule in eval-pkg.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Table of expression types-evaluation rules 
(load "./interpreter/eval-pkg.scm")

; Check dispatch-eval is being called
; (trace dispatch-eval)

; Run interpreter
(driver-loop)

;;;;;;;;;;;;;;
; TEST
;;;;;;;;;;;;;;

; Run interactively:
; (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) x)
; ; 3
; 
; (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
; ; 39
; 
; (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (begin (+ x 1) (* z z)))
; ; 169
