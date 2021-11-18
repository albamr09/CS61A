; Write a procedure switch that takes a sentence as
; its argument and returns a sentence in which every
; instance of the words I or me is replaced 
; by you, while every instance of you is 
; replaced by me except at the beginning of the 
; sentence, where it's replaced by I. 

(define (switch-helper sent)
  (cond
    ((empty? sent) '())
    ; If the first word of the sentence is an I or me change to you
    ((or (equal? (first sent) 'I) (equal? (first sent) 'me)) 
      (se 'you (switch-helper (bf sent)))
    )
    ; If it is a you change to me
    ((equal? (first sent) 'you) 
      (se 'me (switch-helper (bf sent)))
    )
    ; Else keep the word
    (else (se (first sent) (switch-helper (bf sent))))
  )
)
;; Exercise 8.
(define (switch sent)
  ; Check on the first iteration if the first word is you
  (cond
    ((empty? sent) '())
    ((equal? (first sent) 'you) 
     ; Change you to I and use the helper
      (se 'I (switch-helper (bf sent)))
    )
    ; Do not change nothing and use the helper
    (else (se (first sent) (switch-helper (bf sent))))
  )
)
(switch '(You told me that I should wake you up)) 
; Outputs: (i told you that you should wake me up)
