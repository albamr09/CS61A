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

(f 1 2)
; 4

; Usage of let
(define (f x y)
  (let 
    ; List of binded local variables
    (
      (a (+ 1 (* x y)))
      (b (- 1 y))
    )
    ; Body of let
    (+ (* x (square a))
    (* y b)
    (* a b))
  )
)

(f 1 2)
; 4
