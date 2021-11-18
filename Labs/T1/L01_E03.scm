; Write a procedure dupls-removed that, given a sentence as input, returns the result of removing duplicate
; words from the sentence.

(define (dupls-removed-helper x seen)
  (cond 
    ((empty? x) seen)
    ((member? (first x) seen) (dupls-removed-helper (bf x) seen))
    (else (dupls-removed-helper (bf x) (se seen (first x))))
  )
)

(define (dupls-removed x) 
  (dupls-removed-helper x '())
)

;;This should output (c a d e b)
(dupls-removed '(a b c a e d e b)) 

;;This should output (a b c) 
(dupls-removed '(a b c)) 

;;This should output (b a) 
(dupls-removed '(a a a a b a a))
