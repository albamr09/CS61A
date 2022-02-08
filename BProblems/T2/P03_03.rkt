#lang racket
(require berkeley)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SETS AS UNORDERED LISTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; One way to represent a set is as a list of its elements in which no 
; element appears more than once. This empty set is represented by the
; empty list. 

; COMPLEXITY: O(N)

(define (element-of-set? x set)
  (cond 
    ; If the set is empty return false
    ((null? set) false)
    ; If x = first element of the set return true
    ((equal? x (car set)) true)
    ; Else keep going through the set - first element
    ; of the set
    (else (element-of-set? x (cdr set)))
  )
)

; (element-of-set? 1 (list 1 2 3))
; #t

; If the object to be adjoined is already in 
; the set, we just return the set.

; COMPLEXITY: O(N)

(define (adjoin-set x set)
  (if (element-of-set? x set)
    ; If the element is in the set
    ; just return the set
    set
    ; else add it to the set
    (cons x set)
  )
)

; (adjoin-set 1 (list 2 3))
; (1 2 3)


; Computes the intersection of two sets, which is the set 
; containing only elements that appear in both arguments.

; COMPLEXITY: O(N²)

(define (intersection-set set1 set2)
  (cond 
    ; If one of the set is empty, the intersection
    ; is the empty set
    ((or (null? set1) (null? set2)) '())
    ; If the first element of set1 is in set2
    ((element-of-set? (car set1) set2)
      ; Add the first element of set1
      ; to the intersection of (set1 - first element) 
      ; and set2
      (cons 
        (car set1) 
        (intersection-set (cdr set1) set2)
      )
    )
    ; if not simply get the intersection of 
    ; (set1 - first element) and set2
    (else (intersection-set (cdr set1) set2))
  )
)

; (intersection-set (list 1 2 3) (list 1 3 4))
; (1 3)

; One way to speed up our set operations is to change the representation
; so that the set elements are listed in increasing order.
; One advantage of ordering shows up in element-of-set?: In checking for the presence of 
; an item, we no longer have to scan the entire set. If we reach a set element that is 
; larger than the item we are looking for, then we know that the item is not in the set

; COMPLEXITY: O(N)

(define (element-of-set-ordered? x set)
  (cond 
    ; If the set is empty, x is not in the set
    ((null? set) false)
    ; If x = first element of the set, x is in the set
    ((= x (car set)) true)
    ; If x < first element of the set, x is not in the set
    ((< x (car set)) false)
    ; Else keep searching on (set - firs element)
    (else (element-of-set-ordered? x (cdr set)))
  )
)

; (element-of-set-ordered? 1 (list 1 2 3))
; #t

; Begin by comparing the initial elements, x1 and x2, of the two sets. 
;
; - If x1 = x2, then that gives an element of the intersection, and the rest
; of the intersection is the intersection of the cdr-s of the two sets. 
; - If x1 < x2. Since x2 is the smallest element
; in set2, x1 cannot appear anywhere in set2 and hence is not in the intersection. 
; - If x2 < x1 then the intersection is given by the intersection of set1
; with the cdr of set2.

; COMPLEXITY: O(N)

(define (instersection-set-ordered set1 set2)
  (if (or (null? set1) (null? set2))
    ; If one of the sets is empty, the intersection
    ; is empty
    '()
    (let 
      ; Get the first elements of set1 and set2
      ((x1 (car set1)) (x2 (car set2)))
      (cond 
        ; If x1 = x2, add to the intersection
        ((= x1 x2)
          (cons 
            ; Add x1 to the intersection of 
            x1 
            ; (set1 - x1) and (set2 - x2)
            (instersection-set-ordered (cdr set1) (cdr set2))
          )
        )
        ; If x1 < x2, do not add to the intersection
        ((< x1 x2)
          ; Do not add anything to the intersection
          ; of (set1 - x1) and (set2)
          (instersection-set-ordered (cdr set1) set2)
        )
        ((< x2 x1)
          ; Do not add anything to the intersection
          ; of (set1) and (set2 - x2)
          (instersection-set-ordered set1 (cdr set2))
        )
      )
    )
  )
)

; (instersection-set-ordered (list 1 2 3) (list 1 3 4))
; (1 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SETS AS BINARY TREES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Each node of the tree holds one element of the set, called the “entry”
; at that node, and a link to each of two other (possibly empty) nodes.

; The "left” link points to elements smaller than the one at the node, 
; and the “right” link to elements greater than the one at the node.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Example: given the set {1, 3, 5, 7, 9, 11}

;       7
;      / \
;     3   9 
;    / \   \
;   1   5   11

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We can represent trees by using lists. Each node will be a list of three
; items: the entry at the node, the left subtree, and the right subtree.

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right)
)

; COMPLEXITY: O(log N), provided the tree is balanced

(define (element-of-set-tree? x set)
  (cond 
    ; If the set is emtpy, x is not on the set
    ((null? set) false)
    ; If x = element of current node, then 
    ; x is in the set
    ((= x (entry set)) true)
    ; If x < element of current node, search
    ; the left subtree
    ((< x (entry set))
      (element-of-set-tree? x (left-branch set))
    )
    ; If x < element of current node, search
    ; the right subtree
    ((> x (entry set))
      (element-of-set-tree? x (right-branch set))
    )
  )
)

; Let's define the tree of the example
(define tree 
  (make-tree
    ; node
    7
    ; left brach
    (make-tree
      ; node
      3
      ; left branch
      (make-tree 1 null null)
      ; right branch
      (make-tree 5 null null)
    )
    (make-tree
      ; node
      9
      ; left branch
      null
      ; right branch
      (make-tree 11 null null)
    )
  )
)
; (element-of-set-tree? 4 tree)
; ; #f
; (element-of-set-tree? 7 tree)
; ; #7


; To adjoin an item x, 
; (1) Compare x with the node entry to determine whether x should be 
; added to the right or to the left branch, 
; (2) Piece this newly constructed branch together with the original 
; entry and the other branch. 
; (3) If x is equal to the entry, we just return the node. If we are 
; asked to adjoin x to an empty tree, we generate a tree that has x 
; as the entry and empty right and left branches.

; COMPLEXITY: O(log N), provided the tree is balanced

(define (adjoint-set-tree x set)
  (cond 
    ; If the set is empty, create a tree with 
    ; the left and right subtree empty
    ((null? set) (make-tree x '() '()))
    ; If x = (current node), do not add x 
    ; to the set
    ((= x (entry set)) set)
    ; If x < (current node), append x to
    ; the left subtree of the current node
    ((< x (entry set))
      (make-tree 
        ; do not change current node
        (entry set)
        ; append to left subtree
        (adjoint-set-tree x (left-branch set))
        ; do not change right subtree
        (right-branch set)
      )
    )
    ; If x > (current node), append x to
    ; the right subtree of the current node
    ((> x (entry set))
      (make-tree 
        ; do not change current node
        (entry set) 
        ; do not change left subtree
        (left-branch set)
        ; append to the right subtree
        (adjoint-set-tree x (right-branch set))
      )
    )
  )
)

; (adjoint-set-tree 4 tree)

; Exports
(provide entry left-branch right-branch make-tree adjoint-set-tree)
