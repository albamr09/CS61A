#lang racket
(require berkeley)


(define (install-rational-package)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Internal procedures
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;
  ;; Constructor
  ;;;;;;;;;;;;;;;

  (define (make-rat n d)
    (let 
      ((g (gcd n d)))
      ; Reduce it to its smallest factors
      ; by the means of gcd
      (cons (/ n g) (/ d g))
    )
  )

  ;;;;;;;;;;;;;;;
  ;; Selectors
  ;;;;;;;;;;;;;;;

  (define (numer x) (car x))
  (define (denom x) (cdr x))

  ;;;;;;;;;;;;;;;
  ;; Arithmetic
  ;;;;;;;;;;;;;;;

  (define (add-rat x y)
    (make-rat 
      ; Numerator
      (+ 
        (* (numer x) (denom y))
        (* (numer y) (denom x))
      )
      ; Denominator
      (* (denom x) (denom y))
    )
  )

  (define (sub-rat x y)
    (make-rat 
      ; Numerator
      (- 
        (* (numer x) (denom y))
        (* (numer y) (denom x))
      )
      ; Denominator
      (* (denom x) (denom y))
    )
  )

  (define (mul-rat x y)
    (make-rat 
      ; Numerator
      (* (numer x) (numer y))
      ; Denominator
      (* (denom x) (denom y))
    )
  )

  (define (div-rat x y)
    (make-rat 
      ; Numerator
      (* (numer x) (denom y))
      ; Denominator
      (* (denom x) (numer y))
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Interface to rest of the system
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Determine the type of x, append the tag rational
  (define (tag x) (attach-tag 'rational x))

  ;;;;;;;;;;;;;;;
  ;; Constructor
  ;;;;;;;;;;;;;;;

  (put 
    ; Operation name
    'make 
    ; Argument type
    'rational
    ; Procedure
    (lambda 
      (n d) 
      ; Determine the type of n/d by 
      ; tagging it rational
      (tag (make-rat n d))
    )
  )

  ;;;;;;;;;;;;;;;
  ;; Arithmetic
  ;;;;;;;;;;;;;;;

  (put 
    ; Operation name
    'add 
    ; Argument types
    '(rational rational)
    ; Determine the type of the result of add-rat
    ; by tagging it rational
    (lambda (x y) (tag (add-rat x y)))
  )

  (put 'sub '(rational rational)
    (lambda (x y) (tag (sub-rat x y)))
  )

  (put 'mul '(rational rational)
    (lambda (x y) (tag (mul-rat x y)))
  )

  (put 'div '(rational rational)
    (lambda (x y) (tag (div-rat x y)))
  )

  ; Finish installing
  'done
)

; Create tagged rational data object
(define (make-rational n d)
  ((get 'make 'rational) n d)
)
