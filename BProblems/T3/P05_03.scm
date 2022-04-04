;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ITERATIVE PROCESSES AS STREAMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Recall that the idea is to generate a sequence of
; better and beer guesses for the square root of x by applying over and
; over again the procedure that improves guesses:

(define (average x y)
  (/ 
    (+ x y)
    2
  )
)

(define (sqrt-improve guess x)
  (average guess (/ x guess))
)

; We can generate the infinite stream of
; guesses, starting with an initial guess of 1:65

(define (sqrt-stream x)
  (define guesses
    (cons-stream
      ; Starting guess
      1.0
      (stream-map 
        ; Apply sqrt-improve
        (lambda (guess) (sqrt-improve guess x))
        ; From the first guess
        guesses
      )
    )
  )
  guesses
)

; (display-stream (sqrt-stream 2))
; Infinetly
; 1.41421356237309

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; APPROXIMATION OF PI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Another iteration that we can treat in the same way is to generate
; an approximation to π, where

; π/4 = 1 - 1/3 + 1/5 - 1/7 + ...

; Partial sums, from n=1 to infinity
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
    (stream-map 
      - 
      (pi-summands (+ n 2))
    )
  )
)

; Define a stream of ones recursively
(define ones (cons-stream 1 ones))

; Define an elementwise prodcut
; n: scalar the stream is multiplied by
; s: stream
(define (scale-stream s n) 
  (define sn (cost-stream 1 sn))
  (stream-map 
    * 
    ; Stream of n
    sn
    s
  )
)

;; PARTIAL SUMS IS MISSING

; (define pi-stream
;   ; Multiply pi by 4 (see series definition above)
;   (scale-stream 
;     ; Start creating stream of partial sums
;     ; for n=1, sn = 1
;     ; for n=2, sn = 1 - 1/3
;     ; for n=3, sn = 1 - 1/3 - 1/5
;     (partial-sums (pi-summands 1)) 
;     4
;   )
; )

; (display-stream pi-stream)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; APPROXIMATION OF PI WITH ACCELERATION SEQUENCES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PI-STREAM IS MISSING

; S_n+1 - (S_n+1 - S_n)^2/(S_n-1 - 2Sn + S_n+1)

(define (euler-transform s)
  (let 
    (
      (s0 (stream-ref s 0)) ; Sn−1
      (s1 (stream-ref s 1)) ; Sn
      (s2 (stream-ref s 2)) ; Sn+1
    ) 
    (cons-stream 
      (- 
        ; S_n+1
        s2 
        (/ 
          ; (S_n+1 - S_n)^2
          (square (- s2 s1))
          ; (S_n-1 - 2Sn + S_n+1)
          (+ s0 (* -2 s1) s2)
        )
      )
      ; Keep applying
      (euler-transform (stream-cdr s))
    )
  )
)

(display-stream (euler-transform pi-stream))
; 3.166666666666667
; 3.1333333333333337
; 3.1452380952380956
; 3.13968253968254
; 3.1427128427128435
; 3.1408813408813416
; 3.142071817071818
; 3.1412548236077655

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RERCURSIVENESS AND ACCELERATION SEQUENCES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We can accelerate the accelerated sequence, and recursively
; accelerate that, and so on. Namely, we create a stream of 
; streams (a structure we'll call a tableau)

(define (make-tableau transform s)
  (cons-stream 
    ; "original" stream (may already be accelerated, if it is not the first iteration)
    s 
    ; accelerate stream again with transform
    (make-tableau transform (transform s))
  )
)

; Finally, we form a sequence by taking the first term in each row of the
; tableau:
; s00 s01 s02 s03 s04
;     s10 s11 s12 s13 -> accelerated once
;         s20 s21 s22 -> accelerated twice
;             s30 s31 -> accelerated thrice

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s))
)

; Example with the pi sequence

; Accelerator: euler-transforma
; Series: pi-stream
(display-stream (accelerated-sequence euler-transform pi-stream))