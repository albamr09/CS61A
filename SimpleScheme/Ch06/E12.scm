; Make plural handle correctly words that end in but have a vowel before the y,
; such as boy. Then teach it about words that end in (box). 

(define (plural wd) 
  (cond
    ((equal? (last wd) 'y) 
      (if (member? (last (bl wd)) 'aeiou)
        (word wd 's)
        (word (bl wd) 'ies)
      )
    )
    ((equal? (last wd) 'x) (word wd 'es))
    (else (word wd 's))
  )
)

(plural 'boy)
(plural 'box)
(plural 'car)
(plural 'fly)

