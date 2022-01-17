#lang racket
(require berkeley)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Huffmann Encoded Tree ADT
;;;;;;;;;;;;;;;;;;;;;;;;;;;

; At each non-leaf node of the tree there is a set containing all the symbols 
; in the leaves that lie below the node. In addition, each symbol at a leaf 
; is assigned a weight (which is its relative frequency), and each non-leaf 
; node contains a weight that is the sum of all the weights of the leaves 
; lying below it. 

;;;;;;;;;;;;;;;;;;;;;
;;; EXAMPLE
;;;;;;;;;;;;;;;;;;;;;

; A Huffman tree for the where the weights at the leaves indicate that the tree 
; was designed for messages in which A appears with relative frequency 8, B with 
; relative frequency 3, and the other leers each with relative frequency 1.

; ({A B C D E F G H} 17)
;    /              \
; (A 8) ({B C D E F G H} 9) 
;          /           \
;         /         ({E F G H} 4)
;        /           /        \
; ({B C D} 5)     ({E F} 2)    \
;     /   \         /  \        \
; (B 3) ({C D} 2) (E 1) (F 1) ({G H} 2)
;         /  \                  /    \
;     (C 1) (D 1)             (G 1) (H 1)

;;;;;;;;;;;;
;;; ENCODING
;;;;;;;;;;;;

; 1. Start at the root
; 2. Is the current symbol in the set of symbols of the node 
;   of the left branch 
;   2.1. If yes, we move to the left branch, and we add a 1
;   2.2. If not, we move to the right branch, and we add a 1
; 3. Is the current node a leaf?
;   3.1. If yes, go to step 4
;   3.2. If not, go to step 2
; 4. Is there a next symbol?
;   4.1. If yes, go to step 1
;   4.1. If not, return the encoded string

; PROCEDURE DEFINED ON "../../Labs/T2/L05/E10_2.rkt"

;;;;;;;;;;;;
;;; GENERATING HUFFMANN TREES
;;;;;;;;;;;;

; The algorithm for generating a Huffman tree is very simple. The idea
; is to arrange the tree so that the symbols with the lowest frequency
; appear farthest away from the root.

; 1. Select two leaves from the set with the smallest weights.
; 2. Remove them from the set 
; 3. Replace the leaves with a node whose set of symbols is the 
;   result of mergin the symbols of the removed leaves and the 
;   weight is the sum of the weights in the leaves
; 4. Does the set only have one node?
;   4.1. If yes, return the tree
;   4.2. If not, go to step one with the updated set


; Initial leaves {(A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)}
; (C 1) and (D 1)
; Merge {(A 8) (B 3) ({C D} 2) (E 1) (F 1) (G 1) (H 1)}
; (E 1) and (F 1)
; Merge {(A 8) (B 3) ({C D} 2) ({E F} 2) (G 1) (H 1)}
; (G 1) and (H 1)
; Merge {(A 8) (B 3) ({C D} 2) ({E F} 2) ({G H} 2)}
; ({E F} 2) and ({G H} 2)
; Merge {(A 8) (B 3) ({C D} 2) ({E F G H} 4)}
; (B 3) and ({C D} 2)
; Merge {(A 8) ({B C D} 5) ({E F G H} 4)}
; ({B C D} 5) and ({E F G H} 4)
; Merge {(A 8) ({B C D E F G H} 9)}
; (A 8) and ({B C D E F G H} 9)
; Final merge {({A B C D E F G H} 17)}



; CONSTRUCTORS 

; A tree is made up of 
; - left branch
; - right branch
; - on the node:
;   - set of symbols under the tree
;   - total weight of the tree

(define (make-code-tree left right)
  (list 
    left
    right
    ; Set of symbols
    (append 
      (symbols left) 
      (symbols right)
    )
    ; Total weight
    (+ 
      (weight left) 
      (weight right)
    )
  )
)

; Leaves of the tree are represented by a list consisting of the symbol
; leaf, the symbol at the leaf, and the weight:
(define (make-leaf symbol weight) 
  (list 'leaf symbol weight)
)

; SELECTORS 

; Get the symbol of the leaf
(define (symbol-leaf x) (cadr x))
; Get the weight of the leaf
(define (weight-leaf x) (caddr x))

; Get the right/left branches of a tree
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

; Obtain the set of symbols under the tree
(define (symbols tree)
  ; If the tree is a leaf
  (if (leaf? tree)
    ; Obtain the symbol of the leaf
    (list (symbol-leaf tree))
    ; The list of symbols of the tree
    ; is the third element of the 
    ; data structure (list)
    (caddr tree)
  )
)

; Obtain the total weight (frequency) of the tree
; (all the weights of the leafs in the tree), stored in node
(define (weight tree)
  ; If the tree is a leaf
  (if (leaf? tree)
    ; Obtain the weight of the leaf
    (weight-leaf tree)
    ; The total weight of the tree is the 
    ; fourth element of the data structure 
    ; (list)
    (cadddr tree)
  )
)


; OTHER OPERATIONS 

(define (leaf? object) 
  (eq? (car object) 'leaf)
)

;;;;;;;;
;;;;; DECODING BIT STRINGS
;;;;;;;;

; 1. We begin at the root
; 2. Use the 0's and 1's to determine whether to move down the left
;     or the right branch
; 3. If we reach a leaf, return the symbol of the branch
; 4. Start again from the root until the string is empty

; - bits: string to decode
; - tree: defines the alphabet

(define (decode bits tree)
  ; - bits: list of remaining bits
  ; - current-branch: current position on the 
  ;                   tree
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let 
        (
          ; Determine whether to go to the right or
          ; left branch
          (next-branch
            (choose-branch 
              ; First bit of string
              (car bits) 
              current-branch
            )
          )
        )
        ; If the root node of the branch is a leaf
        (if (leaf? next-branch)
          ; Concatenate the symbol of the leaf
          ; with the result of the rest of the 
          ; string decoded
          (cons 
            ; Symbol of leaf
            (symbol-leaf next-branch)
            ; Decode rest of string
            (decode-1 
              ; Rest of bits
              (cdr bits) 
              tree
            )
          )
          ; Else keep searching, exploring the 
          ; determined branch
          (decode-1 
            ; Rest of bits
            (cdr bits) 
            ; Explore the selected branch
            next-branch
          )
        )
      )
    )
  )
  (decode-1 bits tree)
)

(define (choose-branch bit branch)
  ; Choose left if the bit = 0, else
  ; choose right
  (cond 
    ((= bit 0) (left-branch branch))
    ((= bit 1) (right-branch branch))
    (else (error "bad bit: CHOOSE-BRANCH" bit))
  )
)


; Exports 
(provide make-leaf make-code-tree decode left-branch right-branch leaf? symbol-leaf symbols weight)
