; Write a procedure european-time to convert a time from American AM / PM
; notation into European 24-hour notation. Also write american-time , which does the
; opposite:

(strings-are-numbers #t)

(define (european-time t)
    (
        if (equal? (last t) 'am)
        (first t)
        (+ (first t) 12)
    )
)
(european-time '(8 am))
; 8
(european-time '(4 pm))
; 16
(european-time '(12 am))
; 24
