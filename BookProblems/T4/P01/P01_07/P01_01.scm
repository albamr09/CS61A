;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVAL DEFINITION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (eval exp env) ((analyze exp) env))

(define (analyze exp)
  (cond 
    ; If it is a primitive
    ((self-evaluating? exp) (analyze-self-evaluating exp))
    ; If it is a quoted expression
    ((quoted? exp) (analyze-quoted exp))
    ; If it is a variable
    ((variable? exp) (analyze-variable exp))
    ; Expression to set a variable equal to value
    ((assignment? exp) (analyze-assignment exp))
    ; Create variable/procedure
    ((definition? exp) (analyze-definition exp))
    ; If expression
    ((if? exp) (analyze-if exp))
    ; Lambda function
    ((lambda? exp) (analyze-lambda exp))
    ; Begin expression
    ((begin? exp) (analyze-sequence (begin-actions exp)))
    ; Conditional expression
    ((cond? exp) (analyze (cond->if exp)))
    ; Procedure call
    ((application? exp) (analyze-application exp))
    ; Else throw error
    (else 
      (error "Unknown expression type: ANALYZE" exp)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIMITIVES ANALYSIS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-self-evaluating exp)
  ; Just return expression
  (lambda (env) exp)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUOTES ANALYSIS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-quoted exp)
  ; Obtain text inside quote
  (let ((qval (text-of-quotation exp)))
    ; Return text
    (lambda (env) qval)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLE HANDLER ANALYSIS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-variable exp)
  (lambda 
    (env) 
    ; Search for variable in env
    (lookup-variable-value exp env)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ASSIGNMENT ANALYSIS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-assignment exp)
  (let 
    (
      ; Obtain variable name
      (var (assignment-variable exp))
      ; Variable value
      (vproc (analyze (assignment-value exp)))
    )
    (lambda 
      (env)
      ; Save variable in environment
      (set-variable-value! var (vproc env) env)
      'ok
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE ANALYSIS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-definition exp)
  (let 
    (
      ; Variable name
      (var (definition-variable exp))
      ; Variable value
      (vproc (analyze (definition-value exp)))
    )
    (lambda (env)
      ; Save in environment
      (define-variable! var (vproc env) env)
      'ok
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IF ANALYSIS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-if exp)
  (let 
    (
      ; Predicate
      (pproc (analyze (if-predicate exp)))
      ; First argument
      (cproc (analyze (if-consequent exp)))
      ; Second argument
      (aproc (analyze (if-alternative exp)))
    )
    (lambda 
      (env) 
      ; Evaluate predicate
      (if (true? (pproc env))
        ; If true, evaluate first argument
        (cproc env)
        ; Else, evaluate second argument
        (aproc env)
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LAMBDA ANALYSIS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-lambda exp)
  (let 
    (
      ; Obtain parameters
      (vars (lambda-parameters exp))
      ; Obtain body
      (bproc (analyze-sequence (lambda-body exp)))
    )
    (lambda 
      (env) 
      (make-procedure vars bproc env)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SEQUENCE (BEGIN) ANALYSIS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-sequence exps)

  (define (sequentially proc1 proc2)
    (lambda 
      (env) 
      ; Evaluate first line
      (proc1 env) 
      ; Evaluate rest of lines
      (proc2 env)
    )
  )

  (define (loop first-proc rest-procs)
    ; If there is only this line
    (if (null? rest-procs)
      ; Evaluate this line
      first-proc
      (loop 
        ; Evaluate declarations
        (sequentially first-proc (car rest-procs))
        (cdr rest-procs)
      )
    )
  )

  (let ((procs (map analyze exps)))
    (if (null? procs) 
      (error "Empty sequence: ANALYZE")
    )
    ; Start analyzing
    (loop (car procs) (cdr procs))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRODECURES ANALYSIS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-application exp)
  (let 
    (
      ; Obtain procedure/operator
      (fproc (analyze (operator exp)))
      ; Obtain arguments
      (aprocs (map analyze (operands exp)))
    )
    (lambda 
      (env)
      ; Apply procedure
      (execute-application
        ; Procedure
        (fproc env)
        ; Arguments
        (map 
          (lambda (aproc) (aproc env))
          aprocs
        )
      )
    )
  )
)

(define (execute-application proc args)
  (cond 
    ; Is it primitive?
    ((primitive-procedure? proc)
      ; Simply apply
      (apply-primitive-procedure proc args)
    )
    ; Else
    ((compound-procedure? proc)
      ; Create procedure
      ((procedure-body proc)
        ; Extend environment with arguments
        (extend-environment
          ; Formal parameters
          (procedure-parameters proc)
          ; Arguments
          args
          ; Environment
          (procedure-environment proc)
        )
      )
    )
    (else
      (error "Unknown procedure type: EXECUTE-APPLICATION"
      proc)
    )
  )
)
