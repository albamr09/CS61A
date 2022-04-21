;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E04_05
; SICP 4.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Scheme allows an additional syntax for cond
; clauses, (⟨test⟩ => ⟨recipient⟩). If ⟨test⟩ evaluates to a
; true value, then ⟨recipient⟩ is evaluated. Its value must be a
; procedure of one argument; this procedure is then invoked
; on the value of the ⟨test⟩, and the result is returned as the
; value of the cond expression.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Changes:
; 1. Created representation for extended-cond in interpreter/P01_02.scm
; 2. Created special evaluation rule for extended-cond in interpreter/P01_01.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(cond ((assoc 'b '((a 1) (b 2))) => cadr) (else false))
; 2
(cond ((assoc 'b '((a 1) (c 2))) => cadr) (else false))
; #f
(cond ((assoc 'b '((a 1) (c 2))) => cadr) ((assoc 'c '((a 2) (c 43))) => car) (else false))
; c
(cond ((assoc 'b '((a 1) (c 2))) => cadr) ((assoc 'c '((a 2) (c 43))) => cdr) (else false))
; (43)
