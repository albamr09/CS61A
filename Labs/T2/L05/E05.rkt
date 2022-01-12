#lang racket
(require berkeley)

; Load accumulate-n function
(require "./E04.rkt")

; Suppose we represent vectors v = (vi) as sequences of numbers, and 
; matrices m = (mi,j) as sequences of vectors (the rows of the matrix).
; With this representation, we can use sequence operations to concisely 
; express the basic matrix and vector operations.

; We can define the dot product as

(define (dot-product v w)
  (accumulate 
    ; Sum all multiplications
    + 
    ; Initial value
    0 
    ; Multiply each element of v by the element
    ; of w in the same position
    (map * v w)
  )
)

; Fill in the missing expressions in the following procedures for computing 
; the other matrix operations. (The procedure accumulate-n is defined in the 
; previous exercise)

(define (matrix-*-vector m v)
  (map 
    (lambda
      ; Each row vector in m
      (x)
      ; Multiply the row vector x by 
      ; the vector v
      (dot-product
        x
        v
      )
    )
    m
  )
)

;;;;;;;
;;;; EXAMPLE
;;;;;;;
; Given ((1 2 3) (4 5 6) (7 8 9))
; > (accumulate cons '() (1 4 7))
; > (accumulate-n cons '() ((2 3) (5 6) (8 9)))
; >> (accumulate cons '() (2 5 8))
; >> (accumulate-n cons '() ((3) (6) (9)))
; >>> (accumulate cons '() (3 6 9))
; >>> (accumulate-n cons '() (() () ()))
; <<< (cons (3 6 9) ())
; << (cons (2 5 8) (3 6 9))
; < (cons (1 4 7) ((2 5 8) (3 6 9)))

; so we have ((1 4 7) (2 5 8) (3 ))

(define (transpose mat)
  (accumulate-n 
    ; For every column, append the 
    ; "first element" of each row
    cons
    ; Initial value
    '()
    mat
  )
)

(define (matrix-*-matrix m n)
  (let 
    ; We obtain the transpose
    ; so we can do the dot product
    ; conmuting the position of the product
    ; instead of m*n is n^T*m
    ((cols (transpose n)))
    (map 
      (lambda
        ; For every row of m
        (x)
        ; Multiply the matrix by the row
        (matrix-*-vector 
          cols
          x
        )
      )
      m
    )
  )
)


;;; TEST

;(matrix-*-vector
;  (list (list 1 2) (list 3 4))
;  (list 1 1)
;)
;; (3 7)
;
;(matrix-*-vector
;  (list (list 1 2) (list 3 4))
;  (list 1 2)
;)
;; (5 11)

;(transpose
;  (list 
;    (list 1 2) 
;    (list 3 4)
;  )
;)
;; ((1 3) (2 4))
;
;(transpose
;  (list 
;    (list 1 2 3) 
;    (list 4 5 6)
;  )
;)
;; ((1 4) (2 5) (3 6))
;
;(transpose
;  (list 
;    (list 1 2 3) 
;    (list 4 5 6) 
;    (list 7 8 9)
;  )
;)
;; ((1 4 7) (2 5 8) (3 6 9))

;(matrix-*-matrix
;  (list
;    (list 1 2)
;    (list 3 4)
;  )
;  (list
;    (list 1 2)
;    (list 3 4)
;  )
;)
; ((7 10) (15 22))
