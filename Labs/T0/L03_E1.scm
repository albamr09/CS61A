; Write a count-ums that counts the number of times "um" appears in a sentence:

(define (count-ums sent)
  (cond
    ((empty? sent) 0)
    ((equal? (first sent) 'um) (+ 1 (count-ums (bf sent))))
    (else (count-ums (bf sent)))
  )
)

(count-ums '(today um we are going to um talk about the combining um method))
;3
