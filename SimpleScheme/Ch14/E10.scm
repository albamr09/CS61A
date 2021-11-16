; Write the procedure that takes a sentence as
; an argument and returns the number of words in the sentence that are immediately
; followed by the same word:

(define (count-adjacent-duplicates sent)
    (cond
        ((empty? sent) 0)
        ((= (count sent) 1) 0)
        (else 
            (if (equal? (first sent) (first (bf sent)))
                (+ 1 (count-adjacent-duplicates (bf sent)))
                (+ 0 (count-adjacent-duplicates (bf sent)))
            )
        )
    )
)

(count-adjacent-duplicates '(y a b b a d a b b a d o o))
; 3 
(count-adjacent-duplicates '(yeah yeah yeah))
; 2
