#lang racket
(require berkeley)

; Load interval datatype
(require "./E07_1.rkt")

; Define a constructor make-center-percent that takes a center and 
; a percentage tolerance and produces the desired interval. You 
; must also define a selector percent that produces the percentage 
; tolerance for a given interval. The center selector is the same as 
; the one shown above.


; Make interval given a center and a percentage of
; tolerance. 

; Example:
; - o = 6.8
; - t = 10%
; width = 6.8 * (10/100) = 0.68
; so the interval is (6.8 +- 0.68)

; - o: center of interval
; - t: percentage of tolerance

; Constructor
(define (make-center-percent o t)
  (let
    (
     ; obtain what to add and substract
     (width (* o (/ t 100)))
    )
    (make-interval 
      ; lower limit
      (- o width)
      ; upper limit
      (+ o width)
    )
  )
)

; Selector
; - i: interval

(define (center i)
  ; Divide by two
  (/
    ; Sum both bounds
    (+
      (lower-bound i)
      (upper-bound i)
    )
    2
  )
)

(define (percentage i)
  (* 
    ; Obtain width
    (- 
      (center i)
      (lower-bound i)
    ) 
    ; Obtain the value of the 
    ; width relative to the total (percentage)
    (/ 100 (center i))
  )
)

(define interval (make-center-percent 6.8 10))
interval
; (6.12, 7.48)
(center interval)
; 6.8
(percentage interval)
; 10
