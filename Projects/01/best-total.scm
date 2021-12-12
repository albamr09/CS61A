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


(define (count-all-helper total-hand hand total best i)
  (cond
    ((= (count total-hand) 0)
      best
    )
    ((> i (count total-hand))
      (count-all-helper 
        (bf total-hand)
        (bf total-hand)
        0
        best
        1
      )
    )
    ((or (> total 21) (= (count hand) 0))
      (count-all-helper 
        total-hand
        (delete (item i total-hand) total-hand)
        0
        best
        (+ i 1)
      )
    )
    ((<= total 21)
      (if (> total best)
        (count-all-helper 
          total-hand
          (bf hand)
          (+ 
            (value-card (first hand)) 
            total
          )
          total
          i
        )
        (count-all-helper 
          total-hand
          (bf hand)
          (+ 
            (value-card (first hand)) 
            total
          )
          best
          i
        )
      )
    )
  )
)

(define (count-all hand)
  (cond 
    ((= (count hand) 0) 0)
    ((= (count hand) 1) (value-card (first hand)))
    (else 
      (count-all-helper 
        (bf hand)
        ; Hand without first card
        (bf hand) 
        ; Total  value
        (value-card (first hand)) 
        ; Best value
        (value-card (first hand))
        1
      )
    )
  )
)

; (define (best-total-helper hand aces)
;   (cond
;   )
; )
; 
; (define (best-total hand)
;   (best-total-helper hand '())
; )

; (best-total '(ad 8s)) ; in this hand the ace counts as 11
; ; 19
; (best-total '(ad 8s 5h)) ; here it must count as 1 to avoid busting
; ; 14
; (best-total '(ad as 9h)) ; here one counts as 11 and the other as 1
; ; 21

; (trace count-all-helper)
; (count-all '(ad 8s 8s 8s 5h))

