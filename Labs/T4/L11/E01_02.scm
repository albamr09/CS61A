;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E01_02
; SICP 4.6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; let expressions are derived expressions, because

; >>	(let 
; >>		(
; >>			(⟨var1⟩ ⟨exp1⟩) 
; >>			... 
; >>			(⟨varn⟩ ⟨expn⟩)
; >>		)
; >>		⟨body⟩
; >>	)

; is equivalent to

; >>	(
; >>		(lambda 
; >>			(⟨var1⟩ ... ⟨varn⟩)
; >>			⟨body⟩
; >>		)
; >>		; Call lambda function with expn as arguments
; >>		⟨exp1⟩
; >>		...
; >>		⟨expn⟩
; >>	)

; Implement a syntactic transformation let->combination that reduces 
; evaluating let expressions to evaluating combinations of the type 
; shown above, and add the appropriate clause to eval to handle let expressions

(load "../../../BProblems/T4/P01_02.scm")
; Environment
(load "../../../BProblems/T4/P01_03.scm")
; Load changed eval
(load "../../../BProblems/T4/P01_01.scm")
; Load interpreter
(load "../../../BProblems/T4/P01_04.scm")

; Check if the first element is "cond"
(define (cond? exp) (tagged-list? exp 'cond))

; Override interpreter

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVAL DEFINITION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (eval exp env)
  (cond 
    ; If it is a primitive self-evaluating expression (number), return itself
    ((self-evaluating? exp) exp)
    ; If it is a variable, search its value
    ((variable? exp) (lookup-variable-value exp env))
    ; If it is a quoted expression, return the expression that was quoted
    ((quoted? exp) (text-of-quotation exp))
    ; Call eval recursively to compute the value to be associated with the variable
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-definition exp env))
    ; Check if the predicate is true to decice which argument to evaluate (first or second)
    ((if? exp) (eval-if exp env))
    ; If it is a lambda expression pack the parameters and the body along with the environment 
    ((lambda? exp) 
      (make-procedure 
        (lambda-parameters exp)
        (lambda-body exp)
        env
      )
    )
    ; If it is a begin expression, evaluate the sequence of expressions in order
    ((begin? exp)
      (eval-sequence (begin-actions exp) env)
    )
    ; If it is a cond expression convert it to a set of if expressions
    ((cond? exp) (eval (cond->if exp) env))
    ; If it is the application of a procedure
    ((application? exp)
      ; Apply the operator obtained to the list of operands
      (my-apply 
        ; Evaluate recursively the operator
        (eval (operator exp) env)
        ; Evaluate recursively the operands
        (list-of-values (operands exp) env)
      )
    )
    (else
    (error "Unknown expression type: EVAL" exp))
  )
)
