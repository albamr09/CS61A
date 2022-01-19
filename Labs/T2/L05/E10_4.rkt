#lang racket
(require berkeley)

; Import generate-huffman-tree
(require "./E10_3.rkt")
; Import encode
(require "./E10_2.rkt")

; The following eight-symbol alphabet with associated relative frequencies was 
; designed to efficiently encode the lyrics of 1950s rock songs. (Note that the 
; “symbols” of an “alphabet” need not be individual letters)

; A 2 GET 2 SHA 3 WAH 1
; BOOM 1 JOB 2 NA 16 YIP 9

; Use generate-huffman-tree (Exercise 2.69) to generate a
; corresponding Huffman tree, and use encode (Exercise 2.68)
; to encode the following message:

; Get a job
; Sha na na na na na na na na
; Get a job
; Sha na na na na na na na na
; Wah yip yip yip yip yip yip yip yip yip
; Sha boom

; How many bits are required for the encoding? What is the
; smallest number of bits that would be needed to encode this
; song if we used a fixed-length code for the eight-symbol
; alphabet?

(define rock-tree 
  (generate-huffman-tree
    (list '(A 2) '(GET 2) '(SHA 3) '(WAH 1) '(BOOM 1) '(JOB 1) '(NA 16) '(YIP 9))
  )
)

(define rock-message
  '(GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM
    )
)
(define encoded-rock-message 
  (encode
    rock-message
    rock-tree
  )
)

; How many bits are required for the encoding?
(count encoded-rock-message)
;
; What is the smallest number of bits that would be needed to encode this
; song if we used a fixed-length code for the eight-symbol
; alphabet?
(* 3 (count rock-message))

