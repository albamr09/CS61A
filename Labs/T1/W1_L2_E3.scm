; Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two
; larger numbers

(define (findlarger x y z)
  (cond 
    ((and (> x y) (> x z)) x)
    ((and (> y x) (> y z)) y)
    (else z)
  )
)

(define (sum_larger x y z)
  (cond
    ((= (findlarger x y z) x) 
      (if (> y z) (+ x y) (+ x z))
    )
    ((= (findlarger x y z) y)
      (if (> x z) (+ x y) (+ y z))
    )
    ((= (findlarger x y z) z)
      (if (> x y) (+ x z) (+ y z))
    )
  )
)
