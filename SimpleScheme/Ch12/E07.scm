; Write a procedure that spells out the digits of a number:

(define (spell-digit digit)
  (item (+ 1 digit)
  '(zero one two three four five six seven eight nine))
)

(define (spell-number n)
  (if (empty? n)
    '()
    (se (spell-digit (first n)) (spell-number (bf n)))
  )
)

(spell-number 1971)
; (ONE NINE SEVEN ONE)
