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
    ((null? args) '())
    ; Is a pair: maybe a procedure
    ((pair? (car args))
      (inferred-types-helper (car args))
      (mark-as-type (cdr args) type)
    )
    (else
      (cond
        ; Is an argument and has a type already
        ((and 
            (get (car args)) 
            (not (eq? (get (car args)) '?))
          )
          (put (car args) 'x)
          (mark-as-type (cdr args) type)
        )
        ; Is an argument
        ((get (car args))
          (put (car args) type)
          (mark-as-type (cdr args) type)
        )
        ; Is not an argument
        (else
          (mark-as-type (cdr args) type)
        )
      )
    )
  )
)

(trace mark-as-type)

(define (load-parameters params)
  (cond
    ((null? params) 'done)
    (else
      (put (car params) '?)
      (load-parameters (cdr params))
    )
  )
)

(define (inferred-types-helper body)
  (if (get (car body))
    ; Then it is not a pair
    (cond
      ; Has it already been inferred
      ((not (eq? (get (car body)) '?))
        (put (car body) 'x)
        ; Continue analizyng
        (inferred-types-helper (cdr body))
      )
      ; Is it a procedure?
      ((proc-exp? body) 
        (put (car body) 'procedure)
        (inferred-types-helper (cdr body))
      )
    ) 
    (cond
      ((list-exp? (car body)) 
        (mark-as-type (cdr (car body)) 'list)
        (inferred-types-helper (cdr body))
      )
      ((sentence-or-word-exp? (car body))
        (mark-as-type (cdr (car body)) 'sentence-or-word)
      )
    )
  )
)

(define (inferred-types proc)
  ; Load every parameter in the table
  (load-parameters (cdr (cadr proc)))
  (inferred-types-helper (caddr proc))
  *the-table*
)

(define (proc-exp? exp)
  (and
    (pair? exp)
    (symbol? (car exp))
  )
)

(define (list-exp? exp)
  (and
    (pair? exp)
    (or
      (equal? (car exp) 'append)
      (equal? (car exp) 'member)
      (equal? (car exp) 'map)
    )
  )
)

(define (sentence-or-word-exp? exp)
  (and
    (pair? exp)
    (or
      (equal? (car exp) 'sentence)
      (equal? (car exp) 'first)
      (equal? (car exp) 'butfirst)
      (equal? (car exp) 'member?)
    )
  )
)

(trace proc-exp? inferred-types list-exp? inferred-types-helper sentence-or-word-exp?)

; Here's an example of what your inference procedure should return.

(inferred-types
    '(define (foo a b c d e f)
      (f 
        (append (a (a b)) c '(b c))
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

