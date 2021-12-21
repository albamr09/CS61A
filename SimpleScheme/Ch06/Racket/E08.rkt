#lang racket

(require berkeley)

; Write a procedure that works like this:
; > (indef-article ’beatle)
; (A BEATLE)
; > (indef-article ’album)
; (AN ALBUM)

(define (indef-article wd)
  (
    if (member? (first wd) 'aeiou)
    (se 'an wd)
    (se 'a wd)
  )
)

(indef-article 'beatle)
(indef-article 'album)

