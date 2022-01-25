#lang racket
(require berkeley)

(define (attach-tag type-tag contents)
  ; Append the tag to the complex number
  (cons type-tag contents)
)

(define (type-tag datum)
  (if (pair? datum)
    ; First element of list is tag
    (car datum)
    (error "Bad tagged datum: TYPE-TAG" datum)
  )
)

(define (contents datum)
  (if (pair? datum)
    ; Second element of list is complex number
    (cdr datum)
    (error "Bad tagged datum: CONTENTS" datum)
  )
)

(define (rectangular? z)
  ; Is the tag = rectangular
  (eq? (type-tag z) 'rectangular)
)
(define (polar? z) 
  ; Is the tag = polar 
  (eq? (type-tag z) 'polar)
)

; Exports
(provide attach-tag type-tag contents rectangular? polar?)
