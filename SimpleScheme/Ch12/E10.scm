; Write a procedure that takes a word and a sentence as arguments and
; returns the same sentence, but with all copies of the given word removed:

(define (remove wd sent)
  (if (empty? sent)
    '()
    (se 
      (if (equal? wd (first sent))
        '()
        (first sent)
      )
      (remove wd (bf sent))
    )
  )
)

(remove 'the '(the song love of the loved by the beatles))
; (SONG LOVE OF LOVED BY BEATLES)
