#lang racket
(require berkeley)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BINARY TREE ADT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Constructor

(define (make-binary-tree entry children) 
  (cond
    ((null? entry) '())
    ((null? children) (list entry))
    (else
      (append (list entry) children)
    )
  )
)

; Selectors

(define (entry tree) (list-ref tree 0))
(define (left-branch tree) 
  (if (= (count tree) 3)
    (list-ref tree 1)
    null
  )
)
(define (right-branch tree) 
  (if (= (count tree) 3)
    (list-ref tree 2)
    null
  )
)

; For binary trees, within the general category of depth-first 
; traversals, there are three possible variants:

; - Preorder: Look at a node before its children.

(define (pre-order tree)
  (cond 
    ((null? tree) '())
    (else 
      ; Print node value
      (print (entry tree))
      ; Explore left branch
      (pre-order (left-branch tree))
      ; Explore right branch
      (pre-order (right-branch tree))
    )
  )
)

; - Inorder: Look at the left child, then the 
; node, then the right child.

(define (in-order tree)
  (cond 
    ((null? tree) '())
    (else 
      ; Explore left-branch
      (in-order (left-branch tree))
      ; Print node value
      (print (entry tree))
      ; Explore right-branch
      (in-order (right-branch tree)) 
    )
  )
)

; - Postorder: Look at the children before the node.

(define (post-order tree)
  (cond 
    ((null? tree) '())
    (else 
      ; Explore left-branch
      (post-order (left-branch tree))
      ; Explore right-branch
      (post-order (right-branch tree))
      ; Print node value
      (print (entry tree)) 
    )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define test-tree
;  (make-binary-tree
;    'c
;    (list
;      (make-binary-tree
;        'c1
;        (list
;          (make-binary-tree
;            'c11
;            null
;          )
;          (make-binary-tree
;            'c12
;            null
;          )
;        )
;      )
;      (make-binary-tree
;        'c2
;        (list
;          (make-binary-tree
;            'c21
;            null
;          )
;          (make-binary-tree
;            'c22
;            null
;          )
;        )
;      )
;    )
;  )
;)

;(entry test-tree)
;; c
;(left-branch test-tree)
;; (c1 (c11) (c12))
;(right-branch (left-branch test-tree))
;; (c12)
;(right-branch test-tree)
;; (c2 (c21) (c22))
;(pre-order test-tree)
;(in-order test-tree)
;(post-order test-tree)
