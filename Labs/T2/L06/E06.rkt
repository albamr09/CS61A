#lang racket
(require berkeley)

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
  (if (pair? (car args))
    (cond
      ((list-exp? (caar args)) (mark-as-type (cadr args) 'list))
      (else
        'unk
      )
    )
    (cons
      (list (car args) type)
      (mark-as-type (cdr args) type)
    )
  )
)

(trace mark-as-type)

(define (inferred-types proc)
  (let
    (
      (params (cdr (cadr proc)))
      (body (caddr proc))
    )
    (if (pair? (car body))
      'pair
      (if (member? (car body) params)
        (cond
          ((proc-exp? (car body)) 
            (mark-as-type (cdr body) '?)
          )
        )
        'non-member
      )
    )
    ;(map
    ;  (lambda
    ;    (element)
    ;    (if (pair? element)
    ;      (inferred-types
    ;        (list
    ;          'define 
    ;          (append 
    ;            (list (caadr proc))
    ;            params
    ;          )
    ;          element
    ;        )
    ;      )
    ;      ; If it is not a pair
    ;      (if (member? element params)
    ;        (cond
    ;          ((proc-exp? element) 'p)
    ;          (else
    ;            #f
    ;          )
    ;        )
    ;        (cond
    ;          ((list-exp? element) 'l)
    ;        )
    ;      )
    ;    )
    ;  )
    ;  body
    ;)
  )
)

(define (proc-exp? exp)
  (symbol? exp)
)

(define (list-exp? exp)
  (or
    (equal? exp 'append)
    (equal? exp 'member)
    (equal? exp 'map)
  )
)

(trace proc-exp? inferred-types list-exp?)

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

