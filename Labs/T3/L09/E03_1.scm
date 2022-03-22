;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; E03_1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Here last-pair is a procedure that returns the last pair in its argument:
(define (last-pair x)
  (if (null? (cdr x)) 
    x 
    (last-pair (cdr x))
  )
)

;Draw the box-pointer diagram of z

(define (make-cycle x)
  ; Sets x as the cdr of the last pair of 
  ; x
  (set-cdr! (last-pair x) x)
  x
)

(define z (make-cycle (list 'a 'b 'c)))

; (1) So z is defined as:
; (a, .) -> (b, .) -> (c, \)

; (2) When we call (last-pair) on z, we obtain: (c, \)
; (3) So, if we update the cdr of (c, \) to point to z, we get
; (c, .) -> (a, .) -> (b, .) -> (c, .) -> (a, .) -> (b, .) -> (c, .)
; we have created a circle where c (the last element) points to the start of the list 
; and this goes on to the infinity

; (print z)
; This will print to the infinity

;What happens if we try to compute (last-pair z)?
; It prints forever, because z has infinitely many elements
