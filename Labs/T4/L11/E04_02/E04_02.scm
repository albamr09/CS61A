;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E04_02
; SICP 4.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Louis Reasoner plans to reorder the cond clauses in eval so that the clause for procedure 
; applications appears before the clause for assignments. He argues that this will make the 
; interpreter more efficient: Since programs usually contain more applications than assignments,
; definitions, and so on, his modified eval will usually check fewer clauses than the original 
; eval before identifying the type of an expression.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a. What is wrong with Louis’s plan? 

; The check for application? is (pair? exp), which applies for almost every expression. 

; For example the expression (define a 1) satisfies the predicate pair?, so this would be evaluated as an
; application, and therefore throw an error, because there is not procedure, primitive or otherwise, 
; called define.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; b. Louis is upset that his plan didn’t work. He is willing to go to any lengths to make his evaluator 
; recognize procedure applications before it checks for most other kinds of expressions. Help him by 
; changing the syntax of the evaluated language so that procedure applications start with call. For 
; example, instead of (factorial 3) we will now have to write (call factorial 3) and instead of (+ 1 2) 
; we will have to write (call + 1 2).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Changes:
; 1. Modified the application? so it checks if the first element of the expression is call
; 2. Modified operator so it retrieves the second element of the expression
; 3. Modified operand so it retrieves the elements after the second element of the expression
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

; Run this first
(define (factorial x)
  (if (call = x 0)
	  1
	  (call * x (call factorial (call - x 1)))))

; Test
(call factorial 3)
; 6
