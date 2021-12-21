#lang racket

(require berkeley)

; Take a word, move all the initial consonants to the end, and
; add “ay.”

(define (pig-latin wd)
  (if (member? (first wd) 'aeiou)
    (word wd 'ay)
    (pig-latin (word (bf wd) (first wd)))
  )
)

(pig-latin 'alabaster)
(pig-latin 'pastrami)
; ASTRAMAPAY

(define (explode wd)
  (if (empty? wd)
    '()
    (se (first wd) (explode (bf wd)))
  )
)

(explode 'dynamite)
; (D Y N A M I T E)

(define (letter-pairs wd)
  (if (<= (count wd) 1)
    '()
    (se 
      ; Select first two letters
      (word 
        (first wd) 
        (first (bf wd))
      ) 
      ; Continue making pairs without the first letter 
      (letter-pairs (bf wd))
    )
  )
)

(letter-pairs 'george)
; (GE EO OR RG GE)
