; Write a procedure that takes a sentence of numbers
; as its argument. It should return if each number (other than the first) is the square
; of the number before it:


(define (progressive-squares? S)
    (cond
      ((empty? S) #t)
      ((= (count S) 1) #t)
        (else
            (if (= (first (bf S)) (square (first S)))
              (progressive-squares? (bf S))
              #f
            )
        )
    )
)

(progressive-squares? '(3 9 81 6561))
; #T
(progressive-squares? '(25 36 49 64))
; #F
