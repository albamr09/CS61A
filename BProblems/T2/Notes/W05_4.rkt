#lang racket
(require berkeley)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TREE ADT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "./W05_2.rkt")

; Search procedures

;;;;;;;;;
; Depth first:  Look at a given node’s children before its siblings.
;;;;;;;;;

(define (depth-first-search tree)
  ; Print the value of the current node
  (print (datum-tree tree))
  (for-each 
    ; Apply depth-first-search on each
    ; child of the current node
    depth-first-search 
    (children-tree tree)
  )
)

;;;;;;;;;
; Breath first:  Look at the siblings before the children
;;;;;;;;;

(define (breadth-first-search tree)
  (bfs-iter (list tree))
)

; Helper: uses a queue to store which nodes
; have not yet been visited
(define (bfs-iter queue)
  ; If the queue is empty, we have finished
  (if (null? queue)
    'done
    ; Else get the first node (task)
    (let 
      ((task (car queue)))
      ; Get the value of the node
      (print (datum-tree task))
      (bfs-iter 
        ; Update the queue
        (append 
          ; Append the list without the current
          ; node
          (cdr queue) 
          ; to the children of the current node
          (children-tree task)
        )
      )
    )
  )
)

;;;;;;;;;
;; TEST
;;;;;;;;;

(define test-tree
  (make-my-tree
    'c
    (list
      (make-my-tree
        'c1
        (list
          (make-my-tree
            'c11
            null
          )
          (make-my-tree
            'c12
            null
          )
        )
      )
      (make-my-tree
        'c2
        (list
          (make-my-tree
            'c21
            null
          )
          (make-my-tree
            'c22
            null
          )
        )
      )
    )
  )
)

; (depth-first-search test-tree)
(breadth-first-search test-tree)
