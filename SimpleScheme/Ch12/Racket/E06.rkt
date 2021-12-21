#lang racket

(require berkeley)

; Write a GPA procedure. It should take a sentence of grades as its argument
; and return the corresponding grade point average:

(define (base-grade grade)
  (cond 
    ((equal? grade 'A) 4.0)
    ((equal? grade 'B) 3.0)
    ((equal? grade 'C) 2.0)
    ((equal? grade 'D) 1.0)
    (else 0.0)
  )
)

(define (grade-modifier mod)
  (cond 
    ((equal? mod '+) 0.33)
    ((equal? mod '-) -0.33)
    (else 0.0)
  )
)


(define (sum grades)
  (if (empty? grades)
    0
    (+ 
      (+ 
        (base-grade (first (first grades)))
        (grade-modifier (bf (first grades)))
      )
      (sum (bf grades))
    )
  )
)

(define (gpa grades)
  (/ (sum grades) (count grades))
)

(gpa '(A A+ B+ B))
; 3.67
