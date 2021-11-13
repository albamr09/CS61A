; > (f1 ’(a b c) ’(d e f))
; (B C D E)

(define (f1 sent1 sent2) 
    (
        bl (bf (se sent1 sent2))
    )
)

; > (f2 ’(a b c) ’(d e f))
; (B C D E AF)

(define (f2 sent1 sent2)
    (
        se (f1 sent1 sent2) (word (first sent1) (last sent2))
    )
)

; > (f3 ’(a b c) ’(d e f))
; (A B C A B C)

(define (f3 sent1 sent2)
    (
        se sent1 sent1
    )
)
; > (f4 ’(a b c) ’(d e f))
; BE

(define (second sent)
    (
        first (bf sent)
    )
)

(define (f4 sent1 sent2)
    (
       word (second sent1) (second sent2) 
    )
)
