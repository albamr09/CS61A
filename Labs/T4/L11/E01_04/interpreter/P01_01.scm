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
    ; If it is a let expression, transform it to a lambda expression and evaluate
    ((let? exp) 
      (eval
        (let->combination exp)
        env
      )
    )
    ; If it is a let* expression, transform it to nested let expressions
    ((let*? exp) 
      (eval
        (let*->nested-lets exp)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; APPLY DEFINITION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (my-apply procedure arguments)
  ; There are two types of procedures
  (cond 
    ; Primitive procedures
    ((primitive-procedure? procedure)
      (apply-primitive-procedure procedure arguments)
    )
    ; Compount procedures
    ((compound-procedure? procedure)
      ; Evaluate sequentially the expressions that make up the body
      (eval-sequence
        ; Body of procedure
        (procedure-body procedure)
        ; Environment for the evaluation is the extension of the base environment
        ; where each formal parameter is binded to the corresponding argument
        (extend-environment
          (procedure-parameters procedure)
          arguments
          (procedure-environment procedure)
        )
      )
    )
    (else
      (error "Unknown procedure type: APPLY" procedure)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROCEDURE ARGUMENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Produce the list of arguments to which the procedure is to be applied
; Used when eval processes a procedure application (application? rule is satisfied)

(define (list-of-values exps env)
  ; When the list is empty (no more arguments)
  (if (no-operands? exps)
    '()
    (cons 
      ; Evaluate first argument on the list
      (eval (first-operand exps) env)
      ; Keep extracting values from the list
      (list-of-values (rest-operands exps) env)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONDITIONALS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eval-if exp env)
  ; Is the predicate true? in the given environment
  (if (true? (eval (if-predicate exp) env))
    ; If so evaluate the first argument
    (eval (if-consequent exp) env)
    ; Else evaluate the second argument
    (eval (if-alternative exp) env)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SEQUENCES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Eval-sequence is used by 
; (1) apply to evaluate the sequence of expressions in a procedure body and 
; (2) by eval to evaluate the sequence of expressions in a begin expression. 

; It takes as arguments a sequence of expressions and an environment, and evaluates 
; the expressions in the order in which they occur.

(define (eval-sequence exps env)
  (cond 
    ; If it is the last expression, do not call recursively to evaluate
    ; the next expression
    ((last-exp? exps)
      (eval (first-exp exps) env)
    )
    (else
      ; Evaluate the first expression
      (eval (first-exp exps) env)
      ; Keep evaluating expressions
      (eval-sequence (rest-exps exps) env)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ASSIGNMENTS AND DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Handles assignments to variables

(define (eval-assignment exp env)
  ; Update the value of the variable
  (set-variable-value! 
    ; Obtain variable
    (assignment-variable exp)
    ; Call eval to obtain the value
    (eval (assignment-value exp) env)
    ; In given environment
    env
  )
  'ok
)

; Handles definitions

(define (eval-definition exp env)
  ; Create definition variable
  (define-variable! 
    ; Obtain the variable
    (definition-variable exp)
    ; Call eval to obtain the value
    (eval (definition-value exp) env)
    ; In given environment
    env
  )
  'ok
)
