; Write a procedure syllables that takes a word as its argument and returns the
; number of syllables in the word, counted according to the following rule: the number
; of syllables is the number of vowels, except that a group of consecutive vowels counts as
; one. For example, in the word “soaring,” the group “oa” represents one syllable and the
; vowel “i” represents a second one.

(define (remove-succesive-vowels wd)
  (cond
    ((empty? wd) (syllables ""))
    ((member? (first wd) 'aeiou) (remove-succesive-vowels (bf wd)))
    (else (syllables wd))
  )
)

(define (syllables wd)
  (cond
    ((empty? wd) 0)
    ((member? (first wd) 'aeiou) (+ 1 (remove-succesive-vowels (bf wd))))
    (else (+ 0 (syllables (bf wd))))
  )
)

(syllables 'soaring)
; 2

