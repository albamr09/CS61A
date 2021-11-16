; Write an exaggerate procedure which exaggerates sentences:

(define (exaggerate sent)
  (if(empty? sent)
    '()
    (se
      (cond 
        ((number? (first sent)) (* (first sent) 2))
        ((equal? (first sent) 'good) 'great)
        ((equal? (first sent) 'bad) 'terrible)
        (else (first sent))
      )
      (exaggerate (bf sent))
    )
  )
)

(exaggerate '(i ate 3 potstickers))
; (I ATE 6 POTSTICKERS)
(exaggerate '(the chow fun is good here))
; (THE CHOW FUN IS GREAT HERE)
