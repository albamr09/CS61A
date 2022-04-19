;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E01_03
; SICP 4.7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; let* is similar to let, except that the bindings of the let* variables are performed sequentially from
; left to right, and each binding is made in an environment in which all of the preceding bindings are 
; visible. For example
; 
; >>> (let* 
; >>>   (
; >>>     (x 3) 
; >>>     (y (+ x 2)) 
; >>>     (z (+ x y 5))
; >>>   )
; >>>   (* x z)
; >>> )
; 
; returns 39. 
; 
; Explain how a let* expression can be rewritten as a set of nested let expressions,
; and write a procedure let*->nested-lets that performs this transformation. If
; we have already implemented let (Exercise 4.6) and we want to extend the evaluator 
; to handle let*, is it sufficient to add a clause to eval whose action is
; 
; (eval (let*->nested-lets exp) env)
; 
; or must we explicitly expand let* in terms of non-derived expressions?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We can rewrite the above let* expression as follows:

; >>> (let 
; >>> 	(
; >>>     (x 3)
; >>> 	)
; >>>   (let
; >>>     (
; >>>       (y (+ x 2))
; >>>     )
; >>>     (let
; >>>       (
; >>>         (z (+ x y 5))
; >>>       )
; >>>       (* x z)
; >>>     )
; >>>   )
; >>> )

; Which means that given a let* expression:
; >>  (let*
; >>    (
; >>      (<var1> <exp1>)
; >>      (<var2> <exp2>)
; >>      ...
; >>      (<varn> <expn>)
; >>    )
; >>    <body>
; >>  )

; This can be reformulated like so:

; >>  (let
; >>    ((<var1> <exp1>))
; >>    (let
; >>      ((<var2> <exp2>))
; >>      ...
; >>      (let
; >>        ((<varn> <expn>))
; >>        <body>
; >>      )
; >>    )
; >>  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Changes:
; 1. Added representation for the let* expression in P01_02.scm
; 2. Added evaluation rule for a let* expression in eval in P01_01.scm
; 3. Added arithmetic operations as primitive procedures in P01_04.scm 
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

; Run interactively:
; (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) x)
; ; 3
; 
; (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
; ; 39
; 
; (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (begin (+ x 1) (* z z)))
; ; 169
