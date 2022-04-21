;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E02
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Modify the metacircular evaluator to allow type-checking of arguments to procedures.

; 1. When a new procedure is defined, a formal parameter can be either 
;				- a symbol as usual or else 
;				- a list of two elements.
; 2. If it is a list, 
;				- the second element is a symbol, the name of the formal parameter. 
;				- the first element is an expression whose value is a predicate 
;					function that the argument must satisfy. That function should return 
;					#t if the argument is valid.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Changes:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; > (define (foo (integer? num) ((lambda (x) (not (null? x))) lst))
;     (list-ref lst num))
; > (foo 3 '(a b c d e))
; d
; > (foo 3.5 `(a b c d e))
; Error: wrong argument type -- 3.5
; > (foo 2 '())
; Error: wrong argument type -- ()
