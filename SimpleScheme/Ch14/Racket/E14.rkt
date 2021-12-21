#lang racket
(require berkeley)

; Write a predicate that takes two sentences as arguments. It should
; return if two conditions are met: The two sentences must have the same number of
; words, and each word of the first sentence must have the same number of letters as the
; word in the corresponding position in the second sentence.

(define (same-shape-helper sent1 sent2)
    (cond
      ((empty? sent1) #t)
      ((= (count (first sent1)) (count (first sent2))) (same-shape-helper (bf sent1) (bf sent2)))
      (else #f)
    )
)

(define (same-shape? sent1 sent2)
    (if (equal? (count sent1) (count sent2))
        (same-shape-helper sent1 sent2)
        #f
    )
)
(same-shape? '(the fool on the hill) '(you like me too much))
; #T
(same-shape? '(the fool on the hill) '(and your bird can sing))
; #F
