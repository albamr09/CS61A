#lang racket
(require berkeley)

; The procedure accumulate-n is similar to accumulate except that it 
; takes as its third argument a sequence of sequences, which are all 
; assumed to have the same number of elements. It applies the designated 
; accumulation procedure to combine all the first elements of the sequences, 
; all the second elements of the sequences, and so on, and returns a sequence 
; of the results. For instance, if s is a sequence containing four sequences, 
; ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s) 
; should be the sequence(22 26 30). Fill in the missing expressions in the following 
; definition of accumulate-n:

; - op: operation to apply over every element of the sequences (i.e. +)
; - init: initial value of the accumulation (i.e. 0)
; - seqs: sequences (i.e. ((1 2) (3 4)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons 
        (accumulate 
          ; Operation to apply
          op 
          ; initial value
          init
          ; For each sequence x in seqs
          ; extract first element
          (map
            (lambda 
              (x)
              ; Get first element of sequence x
              (car x)
            )
            seqs
          )
        )
        (accumulate-n 
          ; Operation to apply
          op 
          ; Initial value
          init 
          ; For each sequence x in seqs
          ; extract all but first element
          (map
            (lambda 
              (x)
              ; Get all but first element of sequence x
              (cdr x)
            )
            seqs
          )
        )
      )
  )
)


;;; TEST

; (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
; (22 26 30)

; Exports
(provide accumulate-n)
