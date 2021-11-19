; Usage of lambdas
(define (f x y)
  (
    ; Definition of the anonymous procedure
    (
      ; Definition with formal arguments
      lambda (a b)
      ; Procedure's body
      (+ (* x (square a))
      (* y b)
      (* a b))
    )
    ; Arguments for the procedure
    (+ 1 (* x y))
    (- 1 y)
  )
)
