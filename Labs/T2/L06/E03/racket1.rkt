#lang racket 

(require (planet dyoo/simply-scheme))
; Exports
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (make-base-namespace) is needed to initialize the current namespace
; else if this is not called while in interactive mode it will fail

(define ns (make-base-namespace))

(parameterize ([current-namespace ns])
  (namespace-require 'berkeley) ; loads berkeley
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Simple evaluator for Racket without DEFINE, using substitution model.
;; Version 1: No DEFINE, only primitive names are global.

;; The "read-eval-print loop" (REPL):

(define (racket-1)
  (newline)
  (display "Racket-1: ")
  (flush-output)
  (print (eval-1 (read)))
  (newline)
  (racket-1)
)

;; Two important procedures:
;; EVAL-1 takes an expression and returns its value.
;; APPLY-1 takes a procedure and a list of actual argument values, and
;;  calls the procedure.
;; They have these names to avoid conflict with Racket's EVAL and APPLY,
;;  which have similar meanings.

;; Comments on EVAL-1:

;; There are four basic expression types in Racket:
;;    1. self-evaluating (a/k/a constant) expressions: numbers, #t, etc.
;;    2. symbols (variables)
;;    3. special forms (in this evaluator, just QUOTE, IF, and LAMBDA)
;;    4. procedure calls (can call a primitive or a LAMBDA-generated procedure)

;; 1.  The value of a constant is itself.  Unlike real Racket, an Racket
;; procedure is here considered a constant expression.  You can't type in
;; procedure values, but the value of a global variable can be a procedure,
;; and that value might get substituted for a parameter in the body of a
;; higher-order function such as MAP, so the evaluator has to be ready to
;; see a built-in procedure as an "expression."  Therefore, the procedure
;; CONSTANT? includes a check for (PROCEDURE? EXP).

;; 2.  In the substitution model, we should never actually evaluate a *local*
;; variable name, because we should have substituted the actual value for
;; the parameter name before evaluating the procedure body.

;; In this simple evaluator, there is no DEFINE, and so the only *global*
;; symbols are the ones representing primitive procedures.  We cheat a little
;; by using Racket's EVAL to get the values of these variables.

;; 3.  The value of the expression (QUOTE FOO) is FOO -- the second element of
;; the expression.

;; To evaluate the expression (IF A B C) we first evaluate A; then, if A is
;; true, we evaluate B; if A is false, we evaluate C.

;; The value of a LAMBDA expression is the expression itself.  There is no
;; work to do until we actually call the procedure.  (This won't be true
;; when we write a more realistic interpreter that handles more Racket
;; features, but it works in the substitution model.)

;; 4.  To evaluate a procedure call, we recursively evaluate all the
;; subexpressions.  We call APPLY-1 to handle the actual procedure invocation.

(define (eval-1 exp)
  (cond 
    ((constant? exp) exp)
    ; If it is an and expression
    ((and-exp? exp) 
      (eval-and exp)
    )
    ; If it is an and expression
    ((map-exp? exp) 
      (eval-map exp)
    )
    ; ns, namespace is needed to initialize the current namespace
    ; else if this is not called while in interactive mode it will fail
    ; symbol: keywords of scheme
    ((symbol? exp) (eval exp ns)) ; use underlying Racket's EVAL
    ; In interactive mode this is sufficient
    ; ((symbol? exp) (eval exp))
    ((quote-exp? exp) (cadr exp))
    ((if-exp? exp)
     ; Evaluate the first argument
     (if (eval-1 (cadr exp))
        ; If true evaluate the second argument
        (eval-1 (caddr exp))
        ; If false evaluate the third argument
        (eval-1 (cadddr exp))
     )
    )
    ; If it is a lambda expression, return the expression
    ((lambda-exp? exp) exp)
    ; If it is a let expression, apply special evaluation
    ((let-exp? exp)
      (eval-let exp)
    )
    ; If the expression is a tuple where the 
    ; operator is the left-most element, i.e:
    ; (+ 1 2 3)
    ((pair? exp) 
      ; Apply the procedure given by the operator
      ; on the arguments
      (apply-1 
        ; Extract the operator
        (eval-1 (car exp))      ; eval the operator
        ; Evaluate all of the arguments
        (map eval-1 (cdr exp))
      )
    )
    (else (error "bad expr: " exp))
  )
)

(trace eval-1)

;; Comments on APPLY-1:

;; There are two kinds of procedures: primitive and LAMBDA-created.

;; We recognize a primitive procedure using the PROCEDURE? predicate in
;; the underlying Racket interpreter.

;; If the procedure isn't primitive, then it must be LAMBDA-created.
;; In this interpreter (but not in later, more realistic ones), the value
;; of a LAMBDA expression is the expression itself.  So (CADR PROC) is
;; the formal parameter list, and (CADDR PROC) is the expression in the
;; procedure body.

;; To call the procedure, we must substitute the actual arguments for
;; the formal parameters in the body; the result of this substitution is
;; an expression which we can then evaluate with EVAL-1.

(define (apply-1 proc args)
  (cond 
    ; use underlying Racket's APPLY
    ((procedure? proc) 
     ; if the first element of the arguments is a lambda procedure
     ; (i.e. 
     ;  (map ; proc
     ;    ; arguments
     ;    (lambda (x) (first x))  ; first argument is a lambda expression
     ;    '(the rain in spain)    ; rest of arguments
     ;  )
     ; )
     ; pair? prevents error when the args are empty
     (if (and (pair? args) (lambda-exp? (car args)))
      (apply
        ; regular function
        proc
        ; lambda function definition
        (lambda
          ; arguments passed to this lambda 
          ; function from applying proc
          params
          ; use apply-1 to evaluate 
          ; the lambda function for the parameters
          ; passed the lambda function with "apply"
          (apply-1 (car args) params)
        )
        ; rest of arguments
        (cdr args)
      )
      (apply proc args)
     )
    )
    ((lambda-exp? proc)
      (eval-1 
        (substitute 
          (caddr proc)   ; the body of the lambda function
          (cadr proc)    ; the formal parameters of the lambda function
          args           ; the actual arguments for the lambda function
          '()            ; bound-vars, see below
        )
      )
    )
    (else (error "bad proc: " proc))
  )
)


;; Some trivial helper procedures:

(define (constant? exp)
  (or 
    (number? exp) 
    (boolean? exp) 
    (string? exp) 
    (procedure? exp)
  )
)

(define (exp-checker type)
  (lambda 
    (exp) 
    (and 
      (pair? exp) 
      (eq? (car exp) type)
    )
  )
)

(define quote-exp? (exp-checker 'quote))
(define if-exp? (exp-checker 'if))
(define and-exp? (exp-checker 'and))
(define lambda-exp? (exp-checker 'lambda))
(define map-exp? (exp-checker 'map-1))
(define let-exp? (exp-checker 'let-1))


;;;;;;;;;;;;;;;;;;;;;;
;; Special evaluation rules:
;;;;;;;;;;;;;;;;;;;;;;

; Special evaluation for the and special form

(define (eval-and exp)
  (cond
    ; If the expression has one element '(and)
    ; return true, all the evaluations returned true
    ((= (count exp) 1) #t)
    ; Else evaluate the first argument of the and expression
    ((eval-1 (cadr exp))
      ; If it is true continue evaluating the rest
      ; of the arguments in the and
      (eval-1 
        ; Append operation name to rest of arguments
        (cons 
          ; operation name: and
          (car exp)
          ; all but first argument, already evaluated
          (cddr exp)
        )
      )
    )
    ; If the evaluation if false, then the and is false
    (else #f)
  )
)

; -------------------

(define (eval-map exp)
  ; Explanation for (cdr (caddr exp))
  ; Given exp = (map-1 first '(1 2 3))
  ; (cdr exp) = '(first '(1 2 3))
  ; (cddr exp) = '('(1 2 3))
  ; (caddr exp) = ''(1 2 3)
  ; (cdr (caddr exp)) = '((1 2 3))
  ; args = (cadr (caddr exp)) = '(1 2 3)
  (let
    ((args
      (cadr (caddr exp))
    ))
    (if (null? args)
      ; Stop when there are no more 
      ; elements in the list
      '()
      (cons
        ; Evaluate the application of the 
        ; procedure over the fist element of 
        ; the list
        (eval-1
          (list
            ; Prcedure
            (cadr exp)
            ; First element of the list, we 
            ; use maybe-quote, because if we are working
            ; with constants we need them double quoted to
            ; work in racket-1
            (maybe-quote (car args))
          )
        )
        (eval-map
          (list
            ; map-1
            (car exp)
            ; Procedure
            (cadr exp)
            ; Rest of the elements (note the double quoting)
            (maybe-quote (cdr args))
          )
        )
      )
    )
  )
)

; -------------------

(define (eval-let-helper exp parameters args)
  ; -----
  ; Given
  ; exp = (
  ;   (a (+ 1 1))
  ;   (b (+ 1 1))
  ; )
  ; We have:
  ; (caar exp) = a
  ; (cadr exp) = (+ 1 1) -> we evaluate this with eval-1
  ; (cdr exp) = ((b (+ 1 1)))
  ; ----
  (cond
    ; If there are no more arguments, return a list with two sublists
    ; - formal parameters (i.e. a, b)
    ; - corresponding real arguments (i.e. 2)
    ((null? exp) (list parameters args))
    (else
      (eval-let-helper
        ; Update expression, remove the first argument 
        ; to evaluate the next
        (cdr exp)
        ; Update the list of formal parameters
        (cons (caar exp) parameters)
        ; Update the list of arguments
        (cons 
          ; Evaluate the expression
          (eval-1 (cadr (car exp))) 
          ; Arguments evaluated before
          args
        )
      )
    )
  )
)

(define (eval-let exp)
  (let
    ; Obtain the pairs of formal parameter - real argument
    ((par-arg-pairs
      (eval-let-helper (cadr exp) '() '())
    ))
    ; Evaluate the resulting expression
    (eval-1
      ; Use subtitute to replace the formal parameters with the real arguments
      (substitute 
        (caddr exp)             ; the body of the let
        (car par-arg-pairs)     ; the formal parameters 
        (cadr par-arg-pairs)    ; the actual arguments 
        '()                     ; bound-vars, see below
      )
    )
  )
)

; -------------------

;; SUBSTITUTE substitutes actual arguments for *free* references to the
;; corresponding formal parameters.  For example, given the expression
;;
;;  ((lambda (x y)
;;     ((lambda (x) (+ x y))
;;      (* x y)))
;;   5 8)
;;
;; the body of the procedure we're calling is
;;
;;     ((lambda (x) (+ x y))
;;      (* x y))
;;
;; and we want to substitute 5 for X and 8 for Y, but the result should be
;;
;;     ((lambda (x) (+ x 8))
;;      (* 5 8))
;;
;; and *NOT*
;;
;;     ((lambda (5) (+ 5 8))
;;      (* 5 8))
;;
;; The X in (* X Y) is a "free reference," but the X in (LAMBDA (X) (+ X Y))
;; is a "bound reference."
;;
;; To make this work, in its recursive calls, SUBSTITUTE keeps a list of
;; bound variables in the current subexpression -- ones that shouldn't be
;; substituted for -- in its argument BOUND.  This argument is the empty
;; list in the top-level call to SUBSTITUTE from APPLY-1.

;; Another complication is that when an argument value isn't a self-evaluating
;; expression, we actually want to substitute the value *quoted*.  For example,
;; consider the expression
;;
;;  ((lambda (x) (first x)) 'foo)
;;
;; The actual argument value is FOO, but we want the result of the
;; substitution to be
;;
;;  (first 'foo)
;;
;; and not
;;
;;  (first foo)
;;
;; because what we're going to do with this expression is try to evaluate
;; it, and FOO would be an unbound variable.

;; There is a strangeness in MAYBE-QUOTE, which must handle the
;; case of a primitive procedure as the actual argument value; these
;; procedures shouldn't be quoted.

(define (substitute exp params args bound)
  (cond 
    ; If it is a primitive data type, return it
    ((constant? exp) exp)
    ; If it is a variable
    ((symbol? exp)
      (if (memq exp bound)
        exp
        (lookup exp params args)
      )
    )
    ; If it is a quoted expression return it
    ((quote-exp? exp) exp)
    ; If there is a lambda function inside
    ((lambda-exp? exp)
      (list 
        'lambda
        (cadr exp)
        ; Substitute inside
        (substitute 
          (caddr exp) 
          params 
          args 
          (append bound (cadr exp))
        )
      )
    )
    (else 
      (map 
        (lambda 
          (subexp) 
          (substitute subexp params args bound)
        )
        exp
      )
    )
  )
)

(define (lookup name params args)
  (cond 
    ((null? params) name)
    ((eq? name (car params)) 
     (maybe-quote (car args))
    )
    (else 
      (lookup 
        name 
        (cdr params) 
        (cdr args)
      )
    )
  )
)

(define (maybe-quote value)
  (cond 
    ((lambda-exp? value) value)
    ((constant? value) value)
    ((procedure? value) value)  ; real Racket primitive procedure
    (else (list 'quote value))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
