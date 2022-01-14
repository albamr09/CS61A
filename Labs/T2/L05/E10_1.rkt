#lang racket
(require berkeley)

; Load Huffmann Encoded Trees ADT
(require "../../../BProblems/T2/P03_04.rkt")

; Define an encoding tree and a sample message:
(define sample-tree
  (make-code-tree 
    (make-leaf 'A 4)
    (make-code-tree
      (make-leaf 'B 2)
      (make-code-tree
        (make-leaf 'D 1)
        (make-leaf 'C 1)
      )
    )
  )
)

;;;;;;;;;
; SAMPLE TREE
;;;;;;;;;

;   ({A B D C} 8)
;     /     \
; (A 4) ({B D C} 4)
;         /     \ 
;     (B 2) ({D C} 2)
;             /   \ 
;          (D 1) (C 1)

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;Use the decode procedure to decode the message, and give
;the result.

(decode sample-message sample-tree)
; (A D A B B C A)

