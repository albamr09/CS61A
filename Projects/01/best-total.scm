; The procedure should return the largest possible total that’s less than or equal to
; 21, if possible

(define (is-ace? card)
  (equal? (first card) 'A)
)

(define (value-card card)
  (if (number? (first card) 
    (first card)
    10
  )
)

(define (best-total-helper hand aces)
  (cond
  )
)

(define (best-total hand)
  (best-total-helper hand '())
)

(best-total '(ad 8s)) ; in this hand the ace counts as 11
; 19
(best-total '(ad 8s 5h)) ; here it must count as 1 to avoid busting
; 14
(best-total '(ad as 9h)) ; here one counts as 11 and the other as 1
; 21

