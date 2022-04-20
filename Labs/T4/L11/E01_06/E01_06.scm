;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E01_05
; SICP 4.11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Scheme allows us to create new bindings for variables by means of define, but provides no way to get
; rid of bindings. Implement for the evaluator a special form make-unbound! that removes the binding of a given symbol
; from the environment in which the make-unbound! expression is evaluated. is problem is not completely specified.
; For example, should we remove only the binding in the first frame of the environment?

; We only remove the binding in the current environment, because each binding represents a different object 
; that could be completely unrelated with the symbol we are trying to remove.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Changes:
; 1. We created a make-unbound! procedure that removes the binding in the current frame
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
; (driver-loop)

;;;;;;;;;;;;;;
; TEST
;;;;;;;;;;;;;;

; Create variable
(define-variable! 'a 1 the-global-environment)
(print the-global-environment)
(make-unbound! 'a the-global-environment)
(print the-global-environment)
; Remove the first element in the frame
(make-unbound! 'car the-global-environment)
(print the-global-environment)
; Remove the last element in the frame
(make-unbound! '/ the-global-environment)
(print the-global-environment)
; Remove the last element before the last in the frame
(make-unbound! '* the-global-environment)
(print the-global-environment)

; Create another environment
(define extended-env
	(extend-environment 
		; Variables
		'(e f)
		; Values
		'(1 2)
		; Enclosing environment
		the-global-environment
	)
)

; Should only have e and f (also shows enclosing env)
(print extended-env)
; Create variables in the enclosing env too
(define-variable! 'e 13 the-global-environment)
; Should have e
(print the-global-environment)
; Should only remove in the first frame
(make-unbound! 'e extended-env)
; Should still have e
(print the-global-environment)
; Should only f (also shows enclosing env)
(print extended-env)
