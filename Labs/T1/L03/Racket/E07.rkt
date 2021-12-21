#lang racket
; Load fixed point function
(require "../../../BProblems/T1/Racket/P03_03.rkt")

; Show that the golden ratio is a fixed point of the transformation x -> 
; 1 + 1/x, and use this fact to compute it by means of 
; the fixed-point procedure.

(fixed-point 
  (lambda 
    ; Formal parameters
    (x) 
    ; y = 1 + 1/x
    (+ 1 (/ 1 x))
  ) 
  ; Initial guess
  1.0
)

; 1.618033988749894
