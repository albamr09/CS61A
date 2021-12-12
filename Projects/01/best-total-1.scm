; The procedure should return the largest possible total that’s less than or equal to
; 21, if possible

(define (is-ace? card)
  (equal? (first card) 'A)
)

(define (value-card card)
  (if (number? (first card))
    (first card)
    10
  )
)

(define (best-total-helper hand n total best)
  (if (= (count lst) n)
    best
    (best-total-helper
      hand
      (+ 1 n)
      ; suma
    )
  )
)

(define (best-total hand)
  (best-total-helper hand 1 0 0)
)
