;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING EXPRESSIONS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Abstraction of how expressions are represented 
; in our language

(define (self-evaluating? exp)
  (cond 
    ; Only numbers and strings are self-evaluating primitives
    ((number? exp) true)
    ((string? exp) true)
    (else false)
  )
)

;;;;;;;;;;;;;;;;;
; Variables are represented by symbols
(define (variable? exp) (symbol? exp))

;;;;;;;;;;;;;;;;;
; Quotations have the form (quote <text-of-quotation>)

; Use tagged-list? to check if the expression is quoted
; comparing the first element to the tag='quote
(define (quoted? exp) (tagged-list? exp 'quote))

(define (tagged-list? exp tag)
  ; First check if is a pair
  (if (pair? exp)
    ; If so, check if the first element equals the tag
    (eq? (car exp) tag)
    ; If it is not a pair, it is not a quoted expression
    false
  )
)

; Extract the text of a quoted expression
(define (text-of-quotation exp) (cadr exp))

;;;;;;;;;;;;;;;;;
; Assignments have the form (set! ⟨var⟩ ⟨value⟩):

; Check if the first element of the list equals 'set!
(define (assignment? exp) (tagged-list? exp 'set!))
; Get the second element of the expression above
(define (assignment-variable exp) (cadr exp))
; Get the third element of the expression above
(define (assignment-value exp) (caddr exp))


;;;;;;;;;;;;;;;;;;
; Definitions have the form

; (1) (define ⟨var⟩ ⟨value⟩)
; or the form
; (2) (define (⟨var⟩ ⟨parameter_1⟩ ... ⟨parameter_n⟩)
;     ⟨body⟩
; )

; The latter form (standard procedure definition) is syntactic sugar for

; (define ⟨var⟩
;   (lambda 
;     (⟨parameter_1⟩ ... ⟨parameter_n⟩)
;     ⟨body⟩
;   )
; )

; Check if the first element of the expression equals "define"
(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  ; If the second element of the expression is a variable
  ; then it is a definition of the form (1)
  (if (symbol? (cadr exp))
    ; Get the variable by extracting the second element
    (cadr exp)
    ; Get the variable by extracting the first element of the second element
    ; of the expression (2)
    (caadr exp)
  )
)

(define (definition-value exp)
  ; If the second element of the expression is a variable
  ; then it is a definition of the form (1)
  (if (symbol? (cadr exp))
    ; The variable is stored in the third element of the expression
    (caddr exp)
    ; Else create a lambda expression with the formal parameters
    ; and the body
    (make-lambda 
      (cdadr exp) ; formal parameters: second element of second element of the expression
      (cddr exp)  ; body: third element of the expression
    )
  ) 
)

;;;;;;;;;;;;;;;;;;;;;
; Lambda expressions are lists that begin with the symbol lambda:

; Check if the first symbol is "lambda"
(define (lambda? exp) (tagged-list? exp 'lambda))
; The parameters are the second element of the expression
(define (lambda-parameters exp) (cadr exp))
; The body is the third element of the expression
(define (lambda-body exp) (cddr exp))

;;;;;;;;;;;;;;;;;
; Constructor for lambda expressions, which is
; used by definition-value, above:

(define (make-lambda parameters body)
  (cons 
    ; tag
    'lambda 
    ; List that encapsulates the parameters and the body
    (cons parameters body)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;
; Conditionals begin with if and have a predicate, a consequent,
; and an (optional) alternative. If the expression has no alternative
; part, we provide false as the alternative.10

; Check if the first element equals "if"
(define (if? exp) (tagged-list? exp 'if))

; The predicate is the second element of the expression
(define (if-predicate exp) (cadr exp))
; The consequent is the third element of the expression
(define (if-consequent exp) (caddr exp))
; The alternative is the fourth element
(define (if-alternative exp)
  ; Check if there is a fourth element
  (if (not (null? (cdddr exp)))
    ; If so return it
    (cadddr exp)
    ; Otherwise return false
    'false
  )
)

; Constructor for if expressions, to be used by
; cond->if to transform cond expressions into if expressions:

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative)
)

;;;;;;;;;;;;;;;;;;;;;
; begin packages a sequence of expressions into a single expression. 

; Is the first element of the expression equal to "begin"
(define (begin? exp) (tagged-list? exp 'begin))

; Get the sequence of expressions inside the begin expression
(define (begin-actions exp) (cdr exp))
; If there are no more elements after the current one in the 
; list, then the current is the last expression
(define (last-exp? seq) (null? (cdr seq)))
; Obtain the first expression in the sequence of expressions
(define (first-exp seq) (car seq))
; Obtain the rest of expressions in the sequence of expressions
(define (rest-exps seq) (cdr seq))

; constructor sequence->exp (for use by cond- >if) 
; transforms a sequence into a single expression, using
; begin if necessary:

(define (sequence->exp seq)
  (cond 
    ((null? seq) seq)
    ; If the current element is the last expression
    ; return the expression
    ((last-exp? seq) (first-exp seq))
    ; Else create begin expression that encapsulates
    ; the sequence
    (else 
      (make-begin seq)
    )
  )
)

; Create begin expression by appending the tag begin
; to the sequence of elements
(define (make-begin seq) (cons 'begin seq))

;;;;;;;
; A procedure application is any compound expression that is not
; one of the above expression types. 

(define (application? exp) (pair? exp))
; The car of the expression is the operator 
(define (operator exp) (car exp))
; The cdr is the list of operands:
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COND

; Check if the first element is "cond"
(define (cond? exp) (tagged-list? exp 'cond))

; The cond clauses are the second element of the expression
(define (cond-clauses exp) (cdr exp))

; A clause is an else if the predicate equals "else"
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else)
)

; The predicate is the first element of the clause
(define (cond-predicate clause) (car clause))
; The actions are the second element of the clause
(define (cond-actions clause) (cdr clause))
; Transform a cond expression into nested if expressions
; made up of the clauses of the conditional
(define (cond->if exp) (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  ; If there is no clause return false
  (if (null? clauses)
    'false ; no else clause
    ; Else
    (let 
      (
        (first (car clauses))
        (rest (cdr clauses))
      )
      ; If it is an else
      (if (cond-else-clause? first)
        ; If there are no more clauses after the current
        (if (null? rest)
          ; Create a sequence of expressions from the actions of 
          ; the current clause
          (sequence->exp (cond-actions first))
          ; Else error
          (error "ELSE clause isn't last: COND->IF" clauses)
        )
        ; Else: create nested ifs
        (make-if 
          ; Predicate of the current clause
          (cond-predicate first)
          ; Action of the current clause
          (sequence->exp (cond-actions first))
          ; If the predicate of the current clause is false
          ; evaluate (expand) next clauses
          (expand-clauses rest)
        )
      )
    )
  )
)
