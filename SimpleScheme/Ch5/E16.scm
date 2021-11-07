; Write a procedure two-first that takes two words as arguments, returning a
; two-letter word containing the first letters of the two arguments.
; > (two-first ’brian ’epstein)
; BE
(define (two-first wd1 wd2)
    (
       word (first wd1) (first wd2) 
    )
)
; Now write a procedure two-first-sent that takes a two-word sentence as argument,
; returning a two-letter word containing the first letters of the two words.
; > (two-first-sent ’(brian epstein))
; BE

(define (two-first-sent sent)
    (
       word (first (first sent)) (first (item 2 sent))
    )
)
