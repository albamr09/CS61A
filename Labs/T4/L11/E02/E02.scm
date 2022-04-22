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
; 1. Changed procedure-parameters (interpreter/P01_02.scm) 
;			to return only the parameters not each pair (predicate, parameter)
; 2. Created procedure-predicates (interpreter/P01_02.scm) to 
;			return the predicates for each parameter, if no parameter 
;			is defined the predicate is null
; 3. Created type-check procedure (interpreter/P01_01.scm) to iterate over
;			the parameters to check if they satisfy their predicate
; 4. Modifi my-apply (interpreter/P01_01.scm) to call type-check before
;			applying procedure 
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

;;;;;;;;;;;;;;;;;;
;;; TEST
;;;;;;;;;;;;;;;;;;
; > (define (foo (integer? num) ((lambda (x) (not (null? x))) lst)) (list-ref lst num))
; > (foo 3 '(a b c d e))
; d
; > (foo 3.5 '(a b c d e))
; Error: wrong argument type -- 3.5
; > (foo 2 '())
; Error: wrong argument type -- ()
