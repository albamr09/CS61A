; Write the procedure , which returns the number of words in a sentence or
; the number of letters in a word.

(define (count x)
  (if (empty? x)
    0
    (+ 1 (count (bf x)))
  )
)

(count 'abs)
(count '(a b s))
(count "")
(count '())
