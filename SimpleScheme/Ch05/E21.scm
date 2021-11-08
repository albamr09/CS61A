; Write a procedure query that turns a statement into a question by swapping the
; first two words and adding a question mark to the last word:
(define (query sent)
    (
        se 
        (item 2 sent) 
        (first sent) 
        (bl (bf (bf sent))) 
        (word (last sent) '?)
    )
)

(query '(you are experienced))
; (ARE YOU EXPERIENCED?)
(query '(i should have known better))
; (SHOULD I HAVE KNOWN BETTER?)
