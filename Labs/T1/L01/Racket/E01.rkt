#lang racket

(require berkeley)

; Modify the following procedure so that it correctly handles cases like (plural 'boy). It may help to define vowel?.

;; Exercise 1.
;; Modify the following procedure
(define (vowel? letter)
  (member? letter 'aeiou)
)

 (define (plural wd)
  (if (equal? (last wd) 'y)
    (if (vowel? (last (bl wd)))
         (word wd 's)
         (word (bl wd) 'ies)
    )
    (word wd 's)
  )
)

(plural 'boy) ; Should return boys
(vowel? 'z)
(vowel? 'a)
