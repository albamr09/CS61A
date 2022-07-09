#lang racket
(require berkeley)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXAMPLE 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; -> (count-leaves '((1 2) 3 4))
; --> (count-leaves '(1 2))
; ---> (count-leaves 1)
; <--- return 1, because it is a single element
; ---> (count-leaves 2)
; <--- return 1, because it is a single element
; <-- return 2, the sum of 1 + 1
; --> (count-leaves '(3 4))
; ---> (count-leaves 3)
; <--- return 1, because it is a single element
; ---> (count-leaves 4)
; <--- return 1, because it is a single element
; <-- return 2, the sum of 1 + 1
; <- return 4, the sum of 2 + 2

(define (count-leaves x)
  (cond 
    ; If there is no element
    ((null? x) 0)
    ; If it is a single element (a leave)
    ((not (pair? x)) 1)
    (else 
      (+ 
        ; Count leaves of the leftmost branch
        (count-leaves (car x))
        ; Count leaves of the rest of the tree
        ; without the leftmost branch
        (count-leaves (cdr x))
      )
    )
  )
)

;; TEST
; (define x (cons (list 1 2) (list 3 4)))
; (count-leaves x)
; 4

; Takes as arguments a numeric factor and a tree whose
; leaves are numbers. It returns a tree of the same shape, where each
; number is multiplied by the factor.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXAMPLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; -> (scale-tree '((1 2) 3 4) 10)
; --> (scale-tree '(1 2) 10)
; ---> (scale-tree 1 10)
; <--- return 10 = 1*10 because tree is a single element
; ---> (scale-tree 2 10)
; <--- return 20 = 2*10 because tree is a single element
; <-- return '(10 20), the result of (cons 10 20)
; --> (scale-tree '(3 4))
; ---> (scale-tree 3)
; <--- return 30 = 3*10 because tree is a single element
; ---> (scale-tree 4)
; <--- return 40 = 4*10 because tree is a single element
; <-- return '(30 40), the result of (cons 40 30)
; <- return '((10 20) 30 40), the result of (cons (10 20) (30 40))

(define (scale-tree tree factor)
  (cond 
    ; If the tree is "empty"
    ((null? tree) nil)
    ; If the tree is composed of one element
    ; multiply the element by the factor
    ((not (pair? tree)) (* tree factor))
    (else 
      (cons 
        ; Scale the subtree that is the 
        ; leftmost brach
        (scale-tree (car tree) factor)
        ; Scale the rest of the tree 
        ; without the leftmost brach
        (scale-tree (cdr tree) factor)
      )
    )
  )
)

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
; (10 (20 (30 40) 50) (60 70))


(define (scale-tree-map tree factor)
  (map 
    ; Function to apply to each element of the
    ; argument
    (lambda 
      ; Formal parameter
      (sub-tree)
      ; If it is a branch, not a leave
      (if (pair? sub-tree)
        ; Scale the branch
        (scale-tree-map sub-tree factor)
        ; Else multiply the element by the factor
        (* sub-tree factor)
      )
    )
    ; Argument
    tree
  )
)

(scale-tree-map (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
; (10 (20 (30 40) 50) (60 70))
