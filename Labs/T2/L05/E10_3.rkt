#lang racket
(require berkeley)

; Load Huffmann tree TAD
(require "../../../BProblems/T2/P03_04.rkt")

; make-leaf-set is the procedure given above that transforms the list of pairs into an ordered set of leaves. 
; successivemerge is the procedure you must write, using make-codetree to successively merge the smallest-weight elements
; of the set until there is only one element le, which is the desired Huffman tree. (is procedure is slightly tricky, but
; not really complicated. If you find yourself designing a complex procedure, then you are almost certainly doing something 
; wrong. You can take significant advantage of the fact that we are using an ordered set representation.)

(define (adjoin-set x set)
  (cond 
    ((null? set) (list x))
    ; if weight of x < weight first element in the list
    ; return (x A), where A is the set
    ((< (weight x) (weight (car set))) (cons x set))
    ; else keep comparing with the rest of the set and concatenate
    ; (A[1] adjoin-set(x, A - A[1])), where A[1] is the first element of A
    (else 
      (cons 
        (car set)
        (adjoin-set x (cdr set))
      )
    )
  )
)

; Obtain an ordered set of leaves

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let 
      ; Compare each pair (leaf)
      ; with the leaves after it (cdr pairs)
      ((pair (car pairs)))
      (adjoin-set 
        ; is this leaf smaller than the 
        ; leaves after it (ordered with make-leaf-set)
        (make-leaf 
          (car pair)        ; symbol (or set of symbols)
          (cadr pair)       ; frequency
        )                
        (make-leaf-set (cdr pairs))
      )
    )
  )
)

; The following procedure takes as its argument a list of symbol-frequency pairs (where no symbol
; appears in more than one pair) and generates a Huffman encoding tree according to the Huffman algorithm.

; Algotrithm described in "../../../BProblems/T2/P03_04.rkt"


(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs))
)

(define (successive-merge leaf-set) 
   ; inserts tree into tree-set such that the result remains 
   ; ordered with respect to the weights  
  (define (insert tree tree-set) 
    (cond 
      ((null? tree-set) (list tree)) 
      ; If the tree's weight is less than the first element
      ; of the tree-set, set tree as the first element
      ((< (weight tree) (weight (car tree-set))) 
       (cons tree tree-set)
      ) 
      (else 
        ; Else compare with the next element of the set
        (cons 
          (car tree-set) 
          (insert tree (cdr tree-set))
        )
      )
    )
  ) 

  (cond

    ((null? (cdr leaf-set)) (car leaf-set)) 
    (else 
      (successive-merge 
        (insert 
          (make-code-tree (car leaf-set) (cadr leaf-set)) 
          (cddr leaf-set)
        )
      )
    )
  )
)



;;; TEST

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

sample-tree
(generate-huffman-tree (list '(A 8) '(B 3) '(C 1) '(D 1) '(E 1) '(F 1) '(G 1) '(H 1)))
