#lang racket
(require berkeley)

; Load generate-huffman-tree
(require "./E10_3.rkt")
; Load encode 
(require "./E10_2.rkt")

; Suppose we have a Huffman tree for an alphabet of n symbols, and that the relative frequencies of
; the symbols are 1, 2, 2², 2³, ..., 2^{n-1}. 
; Sketch the tree 
; for n = 5;
; for n = 10. 
;

; (list '(A 1) '(B 2) '(C 4) '(D 8) '(E 16))
; 1 ----
; '(
;     ((leaf A 1) (leaf B 2) (B A) 3) 
;     '(C 4) 
;     '(D 8) 
;     '(E 16)
; )
; 2 ----
; '(
;   (
;     (leaf C 4)
;     (
;       (leaf A 1) 
;       (leaf B 2) 
;       (B A) 
;       3
;     ) 
;     (C A B)
;     7
;   )
;   '(D 8) 
;   '(E 16)
; )
; 3 ----
; '(
;   (
;     (leaf D 8)
;     (
;       (leaf C 4)
;       (
;         (leaf A 1) 
;         (leaf B 2) 
;         (B A) 
;         3
;       ) 
;       (C A B)
;       7
;     )
;     (D C A B)
;     15
;   )
;   '(E 16)
; )
; 4 ----
; '(
;   (
;     (leaf E 16)
;     (
;       (leaf D 8)
;       (
;         (leaf C 4)
;         (
;           (leaf A 1) 
;           (leaf B 2) 
;           (B A) 
;           3
;         ) 
;         (C A B)
;         7
;       )
;       (D C A B)
;       15
;     )
;   )
;   (E D C A B)
;   21
; )

; In such a tree (for general n) how many bits
; are required to encode the most frequent symbol? The least
; frequent symbol?

(define 2n-tree 
  (generate-huffman-tree (list '(A 1) '(B 2) '(C 4) '(D 8) '(E 16) '(F 32)))
)

(define most-frequent
  (encode '(F) 2n-tree)
)

(define least-frequent
  (encode '(A) 2n-tree)
)

;(count most-frequent)
;; Most frequent: 1
;(count least-frequent)
;; Least frequent: n-1 = 5
