(define (odds-helper sent count)
  (cond
    ((empty? sent) '())
    ; is odd? return the first item and continue
    ((equal? (remainder count 2) 1) 
     (se 
       (first sent) 
       (odds-helper (bf sent) (+ count 1))
      )
    )
    ; is even? continue
    (else 
      (se 
        (odds-helper (bf sent) (+ count 1))
      )
    )
  )
)

(define (odds sent)
  (odds-helper sent 1)
)

(odds '(i lost my little girl))
; (I MY GIRL)
