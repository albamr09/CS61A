#lang racket
(require berkeley)

; This is representation is known as Church numerals, afer its inventor, Alonzo Church, the logician who invented the λ-
; calculus.

; Return always the identity function over x, regardless of the first argument (f)
(define zero 
  (lambda 
    (f) 
    (lambda 
      (x) 
      x
    )
  )
)

; Church successor function (a function on a Church numeral which returns the 
; next Church numeral in the series)
; - n: defined church numeral
(define (add-1 n)
  (lambda 
    ; Formal parameter
    (f) 
    (lambda 
      ; Formal parameter
      (x) 
      ; Apply f to get the next value
      ; of the sequence
      (f 
        ; Get current value of the sequence
        ; i.e. ((one inc) 0) gets a1
        ; where 0 is the first element of 
        ; the series: a0
        ((n f) x)
      )
    )
  )
)


;Define one and two directly (not in terms of zero and add-1). (Hint: Use substitution to evaluate (add-1 zero)). Give a 
; direct definition of the addition procedure + (not in terms of repeated application of add-1).

; Applies the first argument to the second argument: f(x)
(define one
  (lambda 
    (f)
    (lambda
      (x)
      (f x)
    )
  )
)

; Applies the first argument twice to the second argument: f(f(x))
(define two
  (lambda 
    (f)
    (lambda
      (x)
      (f (f x))
    )
  )
)

; Applies the first argument (f) n times to the second argument: f^n(x)
(define (get-church-n n)
  (lambda
    ; Function to apply recursively
    (f)
    (lambda
      ; Function f is applied to x
      (x)
      ; If n = 1, we obtain f^1(x)
      (if (= n 1)
        (f x)
        ; Else obtain f^n recursively, f^n = f(f^(n-1)(x))
        (f 
          ; Apply f again, and update n = n-1
          (((get-church-n (- n 1)) 
            ; Pass function
            f) 
          ; Pass parameter 
          x)
        )
      )
    )
  )
)


; Besides addition, invent multiplication and exponentiation of nonnegative integers. If you're really enthusiastic, see 
; if you can invent subtraction. (Remember, the rule of this game is that you have only lambda as a starting point.) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Addition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Given two nonnegative integers n and m, we have
;; f^{m+n} x = (f^m)(f^n(x)) 

(define (addition m n)
  (lambda
    ; Function
    (f)
    (lambda
      ; Value
      (x)
      ; Apply function m+n times to x
      (((get-church-n (+ m n)) f) x)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiplication
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Given two nonnegative integers n and m, we have
;; f^{mn} x = (f^m)^n (x) 

(define (multiplication m n)
  (lambda
    ; Function
    (f)
    (lambda
      ; Value
      (x)
      ; Apply function m*n times to x
      (((get-church-n (* m n)) f) x)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST

; Increment function
(define (inc x) (+ x 1))

((zero +) 1)
; 1
((one inc) 1)
; 2
((two inc) 1)
; 3
(((get-church-n 5) inc) 1)
; 6
(((add-1 one) inc) 0)
; 2
(((addition 4 2) inc) 0)
; 6
(((multiplication 4 2) inc) 0)
; 8
