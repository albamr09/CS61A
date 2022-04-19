; Load type expression function (the load is with respect to "E01_03")
(load "./interpreter/type-exp.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DISPATCH EVAL DEFINITION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dispatch-eval exp env)
  (let
    ; Obtain the type of the expression
    ((type (type-exp exp)))
    (let
      ; Look up the procedure to apply by the type of the expression
      ((proc (lookup type eval-table)))
      ; If it exists
      (if proc
        ;; With apply
        ; ; Call it
        ; (apply proc (list exp env))
        ; ; Else treat it as an application procedure
        ; (apply (lookup 'application eval-table) (list exp env))
        ;;;;;;
        ; Also without apply
        ; Call it
        (proc exp env)
        ; Else treat it as an application procedure
        ((lookup 'application eval-table) exp env)
      )
    )
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
      (dispatch-eval (first-operand exps) env)
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
  (if (true? (dispatch-eval (if-predicate exp) env))
    ; If so evaluate the first argument
    (dispatch-eval (if-consequent exp) env)
    ; Else evaluate the second argument
    (dispatch-eval (if-alternative exp) env)
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
      (dispatch-eval (first-exp exps) env)
    )
    (else
      ; Evaluate the first expression
      (dispatch-eval (first-exp exps) env)
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
    (dispatch-eval (assignment-value exp) env)
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
    (dispatch-eval (definition-value exp) env)
    ; In given environment
    env
  )
  'ok
)
