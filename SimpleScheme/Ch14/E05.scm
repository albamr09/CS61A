; Write a procedure that takes a sentence as its argument and
; returns the total number of letters in the sentence

(define (letter-count sent)
  (if (empty? sent)
    0
    ; sum the num letters of the first word
    (+ (count (first sent)) 
       (letter-count (bf sent))
    )
  )
)

(letter-count '(fixing a hole))
; 11
