#lang racket
(require berkeley)

; Load table
(require "./table.rkt")

; Another approach to the problem of type-handling is type inference. If, for instance, 
; a procedure includes the expression (+ n k), one can infer that n and k have numeric values. 
; Similarly, the expression(f a b) indicates that the value of f is a procedure. Write a procedure 
; called inferred-types that, given a definition of a Scheme procedure as argument, returns a list 
; of information about the parameters of the procedure. The information list should contain one element 
; per parameter; each element should be a two-element list whose first element is the parameter name and 
; whose second element is a word indicating the type inferred for the parameter. Possible types:

; - ? (the type can't be inferred)
; - procedure (the parameter appeared as the first word in an unquoted expression or as the first argument of map or every)
; - number (the parameter appeared as an argument of +, -, max, or min)
; - list (the parameter appeared as an argument of append or as the second argument of map or member)
; - sentence-or-word (the parameter appeared as an argument of first, butfirst, sentence, or member?, or as the second argument of every)
; - x (conflicting types were inferred)

; You should assume for this problem that the body of the procedure to be examined does not contain any 
; occurrences of if or cond, although it may contain arbitrarily nested and quoted expressions. (A more 
; ambitious inference procedure both would examine a more comprehensive set of procedures and could infer 
; conditions like "nonempty list".) 

; If you're really ambitious, you could maintain a database of inferred argument types and use it when 
; a procedure you've seen is invoked by another procedure you're examining!


(define (mark-as-type args type)
  (cond
    ; If the list of arguments is empty, stop analyzing
    ((null? args) '())
    ; If the current argument is a list, analize the type
    ; by calling inferred-types-helper
    ((pair? (car args))
      ; Analize the current argument
      (inferred-types-helper (list (car args)))
      ; Continue analyzing
      (mark-as-type (cdr args) type)
    )
    ; Is a parameter in the table
    ((get (car args))
      ; Update the type of the parameter
      (assign-type (car args) type)
      ; Continue analyzing
      (mark-as-type (cdr args) type)
    )
    ; Is not a parameter in the table
    (else
      ; Continue analyzing
      (mark-as-type (cdr args) type)
    )
  )
)

; Evaluates each expression on the body and decices
; which type it is, so it can mark the arguments of 
; the expression as a given type
(define (inferred-types-helper body)
  (cond
    ; If the body is empty stop parsing
    ((null? body) '())
    ; If the current element is a list
    ((pair? (car body))
      (cond
        ; Is it a procedure?
        ((proc-exp? (car body)) 
          ; If it is param in the table
          (if (get (caar body))
            (assign-type (caar body) 'procedure)
            '()
          )
          (inferred-types-helper (cdr (car body)))
        )
        ; Is it a list procedure: (append, member, etc)
        ((list-proc? (car body)) 
          ; Mark all of the arguments of the procedure as a list
          (mark-as-type (cdr (car body)) 'list)
          ; Continue analyzing
          (inferred-types-helper (cdr body))
        )
        ; Is it a list expression: ''(b c)
        ;((list-exp? (car body)) 
        ;  Mark all of the arguments of the procedure as a list
        ;  Note we use (cadr (car body)) because of the double quotes
        ;  (mark-as-type (cadr (car body)) 'list)
        ;  Continue analyzing
        ;  (inferred-types-helper (cdr body))
        ;)
        ; Is it a arithmetic expression: +, -, min, max
        ((arith-exp? (car body))
          ; Mark all of the arguments as a number
          (mark-as-type (cdr (car body)) 'number)
          ; Continue analyzing
          (inferred-types-helper (cdr body))
        )
        ; Is it a sentence-word procedure: sentence, first, etc
        ((sentence-or-word-exp? (car body))
          ; Mark all of the arguments as a sentence-or-word
          (mark-as-type (cdr (car body)) 'sentence-or-word)
          ; Continue analyzing
          (inferred-types-helper (cdr body))
        )
        ; If it is none, continue analyzing the next elements of the body
        (else
          (inferred-types-helper (cdr body))
        )
      )
    )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Starting procedure

(define (inferred-types proc)
  ; Load every parameter in the table
  (load-parameters (cdr (cadr proc)))
  ; Start the inferring process
  (inferred-types-helper (cddr proc))
  ; Return the type of each parameter
  (map
    (lambda 
      (param)
      ; Tuple of the name of the parameter
      ; and the type of the parameter (obtained
      ; from the hash table)
      (list param (get param))
    )
    ; List of parameters
    (cdr (cadr proc))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; AUX PROCEDURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Store all of the procedure's parameters
; in a table
(define (load-parameters params)
  (cond
    ((null? params) 'done)
    (else
      (put (car params) '?)
      (load-parameters (cdr params))
    )
  )
)

; Check if the type of the parameter has already
; been defined
(define (is-conflict? param type)
  (and
    ; Different from unknown type
    (not (eq? (get param) '?))
    ; Different from the type that is going to
    ; be assigned
    (not (eq? (get param) type))
  )
)

; Update the type of the parameter in 
; the table to type
(define (assign-type param type)
  ; If the type has already been defined
  (if (is-conflict? param type)
    ; Assign conflicted type
    (put param 'x)
    ; Assign regular type
    (put param type)
  )
)

; Generic procedure to check if 
; a expression of a given type
(define (type-checker operands)
  (lambda
    (exp)
    (and
      (pair? exp)
      (member? (car exp) operands)
    )
  )
)

; Is this a procedure
; i.e. (a b c), then "a" is a procedure
(define (proc-exp? exp)
  (and 
    (pair? exp)
    (or
      ; If the operator is a parameter in the table
      (get (car exp))
      (equal? (car exp) 'lambda)
    )
  )
)

; Is this an arithmetic expression, i.e. (+ a b), then a anb b are numbers
; Checks if the operand is +, -, max or min
(define arith-exp? (type-checker '(+ - max min)))
; Is this a list procedure, i.e. (append a b), then a anb b are list elements
; Checks if the operand is append, member or map
(define list-proc? (type-checker '(append member map every)))
; Is this a list expression i.e. '(a b), then a anb b are list elements
; Checks if the operand is '
(define list-exp? (type-checker '(quote)))
; Is this a sentence or word expression i.e. (first a b), then a anb b are words/sentences
; Checks if the operand is sentence, first, butfirst or member?
(define sentence-or-word-exp? (type-checker '(sentence first butfirst member?)))


;;;;;;;;;;;;;;;;;;;;;
; TEST

; Here's an example of what your inference procedure should return.

(inferred-types
    '(define (foo a b c d e f)
      (f 
        (append (a b) c '(b c))
        (+ 5 d)
        (sentence (first e) f)
      )
    ) 
)

; should return

; (
;   (a procedure) 
;   (b ?) 
;   (c list) 
;   (d number)
;   (e sentence-or-word) 
;   (f x)
; )

(inferred-types
    '(define (foo a c d e f)
      (every
        (lambda 
          (param)
          (sentence c (+ a e) (append e f))
        )
        f
      )
    ) 
)

; (
;   (a number) 
;   (c sentence-or-word) 
;   (d ?)
;   (e x) 
;   (f list)
; )

