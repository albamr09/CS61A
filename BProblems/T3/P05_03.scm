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

; (define (euler-transform s)
;   (let 
;     (
;       (s0 (stream-ref s 0)) ; Sn−1
;       (s1 (stream-ref s 1)) ; Sn
;       (s2 (stream-ref s 2)) ; Sn+1
;     ) 
;     (cons-stream 
;       (- 
;         ; S_n+1
;         s2 
;         (/ 
;           ; (S_n+1 - S_n)^2
;           (square (- s2 s1))
;           ; (S_n-1 - 2Sn + S_n+1)
;           (+ s0 (* -2 s1) s2)
;         )
;       )
;       ; Keep applying
;       (euler-transform (stream-cdr s))
;     )
;   )
; )
; 
; (display-stream (euler-transform pi-stream))
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
;; MISSING PI-STREAM
; (display-stream (accelerated-sequence euler-transform pi-stream))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INFINITE STREAMS OF PAIRS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MISSING INT-PAIRS

; Obtain all the integers i,j where i + j is a prime number
; (stream-filter
;   (lambda 
;     (pair) 
;     (prime? 
;       (+ (car pair) (cadr pair))
;     )
;   )
;   int-pairs
; )

; Where int-pairs all all the pairs of the form (i, j) where i and j are integers and i <= j
; The problem is generating the pairs of integers, because the streams are infinite
; So imagine we have two streams: S and T, such that the pairs are
; (S0, T0), (S0, T1), (S0, T2), ...
; (S1, T0), (S1, T1), (S1, T2), ...
; (S2, T0), (S2, T1), (S2, T2), ...
; ...

; To fetch unique pairs, we will only be concerned with the upper triangle:
; (S0, T0), (S0, T1), (S0, T2), ...
;           (S1, T1), (S1, T2), ...
;                     (S2, T2), ...
; ...

; We now divide the pairs into three sections:

; (S0, T0),|(S0, T1), (S0, T2), ...
; ---------------------------------
;          |(S1, T1), (S1, T2), ...
;          |          (S2, T2), ...
; ...      |

; One is (S0, T0)
; Another are the elements in the same row as (S0, T0), this not included
; And finally, the last section are the remaining elements

; This last section is formed, recursively, for each Sn with (list (stream-cdr S) (stream-cdr T))
; The second section is formed as follows:

;  >>> (stream-map 
;  >>>   (lambda 
;  >>>     (x) 
;  >>>     ; Make a pair with S0 (not necessarily) and x(=Tn), so we create:
;  >>>     ; 1. (S0, T1)
;  >>>     ; 2. (S0, T2)
;  >>>     ; 2. (S0, T3), etc
;  >>>     (list (stream-car s) x)
;  >>>   )
;  >>>   ; For each Tn in the stream T, excluding T0 with cdr
;  >>>   (stream-cdr t)
;  >>> )

; So we can form the stream of pairs like:

; - s and t are streams
(define (pairs s t)
  (cons-stream
    ; First section
    (list (stream-car s) (stream-car t))
    ; Second and third section
    (interleave
      ; Second section: explained above
      (stream-map 
        (lambda (x) (list (stream-car s) x))
        (stream-cdr t)
      )
      ; Third section: apply this procedure recursively*
      (pairs (stream-cdr s) (stream-cdr t))
    )
  )
)

; The recursion of the third section:

; For streams S and T, where {a1}, {a2}, {a3} are the different sections we defined
; > Starting point
; > With the model we define earlier
; > (S0, T0),|(S0, T1), (S0, T2), ...
; > ---------------------------------
; >          |(S1, T1), (S1, T2), ...
; >          |          (S2, T2), ...
; > ...      |
; > {a1} = (S0, T0) 
; > {a2} = (S0, T1), (S0, T2), ...
; > {a3} = (pairs (cdr S) (cdr T)) = (pairs (S1 S2 S3 ...) (T1 T2 T3 ...))
; >>> We enter the recursive call 
; >>> With the model we define earlier, we remove the first row, and move the divider 
; >>> one row under:
; >>>          (S1, T1), |(S1, T2), ...
; >>> ---------------------------------
; >>>                    |(S2, T2), ...
; >>> ...      
; >>> {a1} = (S1, T1) 
; >>> {a2} = (S1, T2), (S1, T3), ...
; >>> {a3} = (pairs (cdr S) (cdr T)) = (pairs (S2 S3 S4 ...) (T2 T3 T4 ...))

; How to combine the two last sections of the pairs:

; Because streams are infinite we want to take elements from the two streams alternatively
; instead of generating all the pairs from one stream. In our case this would mean: we do 
; not want to only feed our filter pairs from the second section, and not be able to 
; make use of the third section. But with interleave we alternate one pair of the second section
; with one pair of the third section.

(define (interleave s1 s2)
  ; If the stream is emtpy, there is nothing
  ; to alternate with, so return s2
  (if (stream-null? s1)
    s2
    ; Else make a stream made up of:
    ; 1. Element of s1
    ; 2. Rest of elements of s1 and s2, but not
    ;    s2 acts as s1, so the next call (stream-car)
    ;    will be applied over s2, and will be the next element after 
    ;    the element in step 1
    (cons-stream 
      (stream-car s1)
      (interleave s2 (stream-cdr s1))
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STREAMS AS SIGNALS: INTEGRATOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; For an input stream x=(x_i) an initial value C and an increment dt:

; S_i = C + sum(x_j*dt), from j=1 to i

; So:
; S_0 = C
; S_1 = C + x_1*dt
; S_2 = (C + x_1*dt) + x_2*dt = S1 + x_2*dt 
; S_3 = (C + x_1*dt + x_2*dt) + x_3*dt = S2 + x_3*dt 
; ...

; this returns the stream of values S=(S_i)

; - integrand = input stream x
; - initial-value = C
; - dt = dt
(define (integral integrand initial-value dt)
  ; Create the stream S
  (define int
    ; Make stream, where the first element: S_0 = C
    (cons-stream 
      initial-value
      ; Now for each i, with 1 <= i 
      ; S_i = S_(i-1) + x_i*dt
      (add-streams 
        ; x_i*dt = multiply every element x_i of the input stream x by dt
        (scale-stream integrand dt)
        ; S_(i-1)
        int
      )
    )
  )
  int
)
