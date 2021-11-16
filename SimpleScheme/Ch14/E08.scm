; Write expand, which takes a sentence as its argument. It returns a sentence similar
; to the argument, except that if a number appears in the argument, then the return value
; contains that many copies of the following word:

(define (expand-helper n wd)
  (if (= n 0)
    '()
    (se wd (expand-helper (- n 1) wd))
  )
)

(define (expand sent)
  (cond 
    ((= (count sent) 0) '())
    ((< (count sent) 1) (first sent))
    ((number? (first sent)) 
     (se 
       (expand-helper (first sent) (first (bf sent))) 
       (expand (bf (bf sent)))
      )
    )
    (else (se (first sent) (expand (bf sent))))

  )
)

(expand '(4 calling birds 3 french hens))
; (CALLING CALLING CALLING CALLING BIRDS FRENCH FRENCH FRENCH HENS)
(expand '(the 7 samurai))
; (THE SAMURAI SAMURAI SAMURAI SAMURAI SAMURAI SAMURAI SAMURAI)
