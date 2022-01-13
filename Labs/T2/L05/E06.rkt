#lang racket
(require berkeley)

; The accumulate procedure is also known as fold-right, because it 
; combines the first element of the sequence with the result of 
; combining all the elements to the right. There is also a fold-left, 
; which is similar to fold-right, except that it combines elements working 
; in the opposite direction:

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter 
        (op result (car rest))
        (cdr rest)
      )
    )
  )
  (iter initial sequence)
)

(define (fold-right op initial sequence)
  (accumulate op initial sequence)
)

; What are the values of the following:

;(fold-right / 1 (list 1 2 3))
;; 3/2
;(fold-left / 1 (list 1 2 3))
;; 1/6
;(fold-right list nil (list 1 2 3))
;; (1 (2 (3 ())))
;(fold-left list nil (list 1 2 3))
;; (((() 1) 2) 3)

; Describe a property that op should satisfy to guarantee 
; that fold-right and fold-left will produce the same values for any sequence.

; To be conmutative
;(fold-right * 1 (list 1 2 3))
;; 6
;(fold-left * 1 (list 1 2 3))
;; 6
;(fold-right + 1 (list 1 2 3))
;; 7
;(fold-left + 1 (list 1 2 3))
;; 7


