;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 4.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SICP 3.16 Draw the 4 box-and-pointer diagrams.

(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ 
      (count-pairs (car x))
      (count-pairs (cdr x))
      1
    )
  )
)
		 
; a. Returns 3:

(print (count-pairs (list (list 1 2) (list 3 4) (list 5 6))))

; > (count-pairs ((1 2) (3 4) (5 6)))
; ----
; >> (count-pairs (1 2))
; >>> (count-pairs 1)
; <<< 0
; >>> (count-pairs 2)
; <<< 0
; << 1
; ----
; >> (count-pairs ((3 4) (5 6)))
; ----
; >>> (count-pairs (3 4))
; >>>> (count-pairs 3)
; <<<< 0
; >>>> (count-pairs 4)
; <<<< 0
; <<< 1
; ----
; >>> (count-pairs ((5 6)))
; -----
; >>>> (count-pairs (5 6))
; >>>>> (count-pairs 5)
; <<<<< 0
; >>>>> (count-pairs 6)
; <<<<< 0
; <<<< 1
; -----
; >>>>> (count-pairs ())
; >>>>> (count-pairs null)
; <<<<< 0
; >>>>> (count-pairs null)
; <<<<< 0
; <<<< 1
; < 4

; b. Returns 4:

; c. Returns 7:

; d. Never returns:

