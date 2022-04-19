;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E01_04
; SICP 4.10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; By using data abstraction, we were able to write an eval procedure that is 
; independent of the particular syntax of the language to be evaluated. To illustrate this, 
; design and implement a new syntax for Scheme by modifying the procedures in this section, 
; without changing eval or apply.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NEW SYNTAX

; Self-evaluating:			number, string -> number, string
; Variable:							symbol -> symbol
; Quote:								' -> '
; Assignment:						set! -> :=
; Definition:						define -> var
; Lambda:								lambda -> "->"
; if:										if -> check 
; begin:								begin -> start
; cond:									cond -> switch
; Let:									let -> fn
; Let*:									let* -> *fn
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Changes:
; 1. Modified the functions that check for each type of expression (i.e. lambda?) 
; in P01_02.scm, so now they check for the keywords specified above
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

; Self-evaluating
1
; Define
(var a 2)
; Variable
a
; 2
(var b 'test)
; Quote
b
; test
; Assignment
(:= a 5)
a
; 5
; Lambda
(
	(->
		(x)
		(+ x 1)
	)
	a
)
; 6
; If
(check (null? '())
	a
	b
)
; 5
; Begin
(start
	(+ 1 a)
	(+ 2 a)
)
; 4
; Cond
(switch
	((null? '()) 'hi)
	(else
		'ooooh
	)
)
; hi
; Let
(fn
	(
		(x (+ 1 1))
	)
	(* x 4)
)
; 8
; Let*
(*fn ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
; 39
