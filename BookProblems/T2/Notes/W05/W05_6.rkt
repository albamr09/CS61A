#lang racket
(require berkeley)

; Load Tree ADT
(require "./W05_2.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PATH FINDING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Suppose we want to look up a place in the tree, and find 
; the path from the root node to that place

(define (find-place place tree)
  ; If the value of the current node equals the place
  (if (eq? place (datum tree))
    ; Return the path until now
    (cons (datum tree) '())
    ; Else
    (let 
      ; Explore the children of the node
      ; to get 'try', which is a path
      ((try (find-forest place (children tree))))
      ; If the path 'try' is not null
      (if (not (null? try))
        ; Concatenate the root with the 
        ; path from the children
        (cons (datum tree) try)
        ; Else return null path
        '()
      )
    )
  )
)

(define (find-forest place forest)
  ; If there are no children return
  ; null path
  (if (null? forest)
    '()
    ; Else
    (let 
      ; Explore the left-most child to find
      ; a path 'try'
      ((try (find-place place (car forest))))
      ; If the path 'try' is not null
      (if (not (null? try))
        ; return the path
        try
        ; else explore the rest of the children
        (find-forest place (cdr forest))
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define test-tree
;  (make-my-tree
;    'c
;    (list
;      (make-my-tree
;        'c1
;        (list
;          (make-my-tree
;            'c11
;            null
;          )
;          (make-my-tree
;            'c12
;            null
;          )
;        )
;      )
;      (make-my-tree
;        'c2
;        (list
;          (make-my-tree
;            'c21
;            null
;          )
;          (make-my-tree
;            'c22
;            null
;          )
;        )
;      )
;    )
;  )
;)
;
;(find-place 'c11 test-tree)
;; (c c1 c11)
;(find-place 'c22 test-tree)
;; (c c2 c22)
