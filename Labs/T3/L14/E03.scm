; Give all possible values of x that can result from executing

(define x 10)

(parallel-execute 
  ; P1
  (lambda () (set! x (* x x)))
  ; P2
  (lambda () (set! x (* x x x)))
)

; 1. Execute P1 completely -> x = 100 -> and execute P2 completely -> x = 100³ = 10⁵
; 2. Execute P2 completely -> x = 1000 -> and execute P1 completely -> x = 1000² = 10⁵
; 3. In P1 read x once -> x_1 = 10, execute P2 completely -> x = 1000 -> read x and set x in P1 -> x = x_1 * x = 10 * 1000 = 10⁴
; 4. In P1 read x twice -> x_1 = x_2 = 10, execute P2 completely -> x = 1000 -> and set x in P1 -> x= 10²
; 5. In P2 read x once -> x_1 = 10, execute P1 completely -> x = 100, read x twice -> x_2 = x_3 = 100 -> and set x in P2 -> x = x_1 * x_2 * x_3 = 10 * 100 * 100 = 10⁵
; 6. In P2 read x twice -> x_1 = x_2 = 10 -> execute P1 completely -> x = 100, read x -> x_3 = 100 once and set x in P2 x = x_1 * x_2 * x_3 = 10 * 10 * 100 * 10⁴

; Which of these possibilities remain if we instead use serialized procedures:

(define x 10)

(define s (make-serializer))

(parallel-execute 
  (s (lambda () (set! x (* x x))))
  (s (lambda () (set! x (* x x x))))
)

; Note now we serialize both procedures, that is the other cannot be executed while one is executing, so:

; The first and second process are doable beacause they never interleave one another. However the rest of them combine the execution of the procedures during runtime.
