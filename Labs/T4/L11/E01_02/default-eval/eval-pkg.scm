; Load table
(load "../../../../BProblems/T3/P03_03/table_1D.scm")

; Load interpreter
; Expressions
(load "../../../../BProblems/T4/P01_02.scm")
; Environment
(load "../../../../BProblems/T4/P01_03.scm")
; Load changed eval
(load "P01_01.scm")
; Load interpreter
(load "P01_04.scm")

; Table that holds type expression - procedure object
(define eval-table (make-table))

(define (install-eval-pkg)
  ;;;;;;;;;;;;;;;;;;;;;
  ; Self-evaluating: number or string
  (insert!
    'self-evaluating
    (lambda
      (exp env)
      ; Return the number or string
      exp
    )
    eval-table
  )
  
  ;;;;;;;;;;;;;;;;;;;;;
  ; variable?
  (insert!
    'variable
    ; Search the symbol
    lookup-variable-value
    eval-table
  )
  
  ;;;;;;;;;;;;;;;;;;;;;
  ; quoted?
  (insert!
    'quote
    ; Lambda because we cannot pass the 
    ; environment and the expression to text-of-quotation
    (lambda
      (exp env)
      ; Get the text
      (text-of-quotation exp)
    ) 
    eval-table
  )
  
  ;;;;;;;;;;;;;;;;;;;;;
  ; assignment?
  (insert!
    'set!
    ; Evaluate and set the value
    eval-assignment
    eval-table
  )
  
  ;;;;;;;;;;;;;;;;;;;;;
  ; definition?
  (insert!
    'define
    ; Create definition
    eval-definition
    eval-table
  )
  
  ;;;;;;;;;;;;;;;;;;;;;
  ; if?
  (insert!
    'if
    ; Evaluate if expression
    eval-if
    eval-table
  )
  
  ;;;;;;;;;;;;;;;;;;;;;
  ; lambda?
  (insert!
    'lambda
    ; Anonymous function because we have to decouple the expression
    ; and create the lambda procedure
    (lambda
      (exp env)
      (make-procedure
        (lambda-parameters exp)
        (lambda-body exp)
        env
      )
    ) 
    eval-table
  )
  
  ;;;;;;;;;;;;;;;;;;;;;
  ; begin?
  (insert!
    'begin
    ; Anonymous function because we have introduce logic before 
    ; calling the function that evaluates the sequence inside begin
    (lambda
      (exp env)
      (eval-sequence
        (begin-actions exp)
        env
      )
    ) 
    eval-table
  )
  
  ;;;;;;;;;;;;;;;;;;;;;
  ; cond?
  (insert!
    'cond
    ; Anonymous function because we have introduce logic before 
    ; calling the function that evaluates the cond
    (lambda
      (exp env)
      (dispatch-eval
        (cond->if exp) env
      )
    ) 
    eval-table
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; E01_02: add expression type and its corresponding handler
  ; for the let expresion
  ;;;;;;;;;;;;;;;;;;;;;
  ; let?
  (insert!
    'let
    ; Anonymous function because we have introduce logic before 
    ; calling the function that evaluates the let transformed
    (lambda
      (exp env)
      ; Evaluate the lambda expression
      (dispatch-eval
        ; Transform let intro a evaluable lambda expression
        (let->combination)
        ; In the given environment
        env
      )
    ) 
    eval-table
  )
  
  ;;;;;;;;;;;;;;;;;;;;;
  ; application?
  (insert!
    'application
    ; Anonymous function because we have to intrduce logic
    ; before calling our apply function
    (lambda
      (exp env)
      (my-apply 
        (dispatch-eval (operator exp) env)
        (list-of-values (operands exp) env)
      )
    ) 
    eval-table
  )
)

; Call package installing
(install-eval-pkg)
