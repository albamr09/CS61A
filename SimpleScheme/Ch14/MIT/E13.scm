; What does the procedure from Chapter 11 do if you invoke it with a word
; like “frzzmlpt” that has no vowels? Fix it so that it returns “frzzmlptay. 

(define (pig-latin-helper wd i)
    (cond
        ((equal? i (count wd)) wd)
        ((member? (first wd) 'aeiou) (word wd 'ay))
        (else
            (pig-latin-helper (word (bf wd) (first wd)) (+ i 1))
        )
    )
)

(define (pig-latin wd)
  (pig-latin-helper wd 0)
)

(pig-latin 'frzzmlpta)
(pig-latin 'frzzmlpt)
(pig-latin 'alabaster)
(pig-latin 'pastrami)
; ASTRAMAPAY
