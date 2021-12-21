#lang racket

(require berkeley)

; Remove duplicates
(define (remdup-helper sent seen)
  (cond 
    ((empty? sent) '())
    ((member? (first sent) seen) 
     (se (remdup-helper (bf sent) seen))
    )
    (else 
      (se (first sent) 
          (remdup-helper (bf sent) 
                         (se (first sent) seen)
          )
      )
    )
  )
)

(define (remdup sent)
  (remdup-helper sent '())
)

(remdup '(ob la di ob la da))
; (OB LA DI DA)
