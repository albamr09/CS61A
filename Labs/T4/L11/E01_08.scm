;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E01_08
; SICP 4.15
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given a one-argument procedure p and an object a, p is said to “halt” on 
; a if evaluating the expression (p a) returns a value (as opposed to 
; terminating with an error message or running forever). Show that it is 
; impossible to write a procedure halts? that correctly determines whether 
; p halts on a for any procedure p and object a. Use the following reasoning: If you had such a procedure halts?, you could implement the following program:

(define (run-forever) (run-forever))

(define (try p)
	(if (halts? p p) 
		(run-forever) 
		'halted
	)
)

; (1) Suppose you run (try try) and (halts? try try) is true then (try try) 
;			runs forever by the definition of the procedure, which is a contradiction
; (2) Suppose you run (try try) and (halts? try try) is false then (try try) 
;			halts by the definition of the procedure, which is a contradiction

; Therefore it cannot exist a halt? procedure that returns whether a any arbitrary procedure halts or 
; not given any arbitrary input
