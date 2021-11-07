; Try the following and explain the result:
(define (ends word)
    (word (first word) (last word))
)

; Fails because it uses the keyword 'word' as a variable
(ends 'john)
