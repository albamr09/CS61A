#lang racket

(require berkeley)

; Write the procedure that takes a sentence as
; argument and returns the same sentence but with any word that’s immediately followed
; by the same word removed:

(define (remove-adjacent-duplicates sent)
  (cond
    ((empty? sent) '())
    ((= (count sent) 1) (first sent))
    (else 
      (if (equal? (first sent) (first (bf sent)))
        (se (remove-adjacent-duplicates (bf sent)))
        (se (first sent) (remove-adjacent-duplicates (bf sent)))
      )
    )
  )
)

(remove-adjacent-duplicates '(y a b b a d a b b a d o o))
; (Y A B A D A B A D O)
(remove-adjacent-duplicates '(yeah yeah yeah))
; (YEAH)
