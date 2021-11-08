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
