; Write a procedure that takes anything as its argument and returns one of
; the words word, sentence, number of boolean :

(define (type-of var)
  (cond
    ((number? var) 'number)
    ((boolean? var) 'boolean)
    ((sentence? var) 'sentence)
    (else 'word)
  )
)

(type-of '(getting better))
; SENTENCE
(type-of 'revolution)
; WORD
(type-of (= 3 3))
; BOOLEAN
