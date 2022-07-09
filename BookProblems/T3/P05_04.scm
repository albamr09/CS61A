;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STREAMS AND DELAYED EVALUATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define an elementwise sum between two streams
(define (add-streams s1 s2) (stream-map + s1 s2))

; Define an elementwise product between a stream 
(define (scale-stream s n) 
  ; Stream of n's recursively
  (define scalar (cons-stream n scalar))
  ; Multiply the streams
  (stream-map
    *
    scalar
    s
  )
)

; The main problem is: in solve we need dy to be defined when we call the integral function, but dy 
; is defined in the line below, therefore:

; 1. When we create the stream integral, we define the first element to be initial-value (y0), which is 
;    the only value that is known
; 2. We pass dy as a delayed object, that will be evaluated when needed (that is, after we call solve at least once)
; 3. Lastly the rest of the elements of the integral stream are obtained by forcing the evaluation of
;    the delayed object dy (or delayed-integrand)

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
      ; Initial-value: only element defined explicitly
      initial-value
      ; Rest of elements of the stream are calculated with solve
      (let 
        ; When needed force the evaluation of delayed-integrand
        ((integrand (force delayed-integrand)))
        (add-streams (scale-stream integrand dt) int)
      )
    )
  )
  int
)


(define (solve f y0 dt)
  ; We use (delay dy) bacause it is not yet defined 
  (define y (integral (delay dy) y0 dt))
  ; Now dy is defined
  (define dy (stream-map f y))
  y
)


; We can demonstrate that the solve procedure works by approximating e ≈ 2:718 by computing the 
; value at y = 1 of the solution to the differential equation dy=dt = y with initial condition 
; y(0) = 1:

(print 
  (stream-ref 
    (solve 
      ; f
      (lambda (y) y)
      ; y0
      1
      ; dt
      0.001
    )
    1000
  )
)

; 2.7169239322359
