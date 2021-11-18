; Write a procedure count-word that takes a sentence and
; a word as arguments and outputs the number of
; occurences of the input word in the sentence.

;; Exercise 5.
(define (count-word sent wd)
  (cond
    ((empty? sent) 0)
    ((equal? (first sent) wd) (+ 1 (count-word (bf sent) wd)))
    (else (count-word (bf sent) wd))
  )
)

(count-word '(i really really like 61as) 'really) ; 2
(count-word '(i lambda scheme) 'love) ; 0
