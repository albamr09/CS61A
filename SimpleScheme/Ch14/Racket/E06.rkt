#lang racket

(require berkeley)

; Write member

(define (member? x S)
  (cond
    ((empty? S) #f)
    ((equal? x (first S)) #t)
    (else (member? x (bf S)))
  )
)

(member? 1 '(1 2))
(member? 1 '())
(member? 1 '(3 4))
