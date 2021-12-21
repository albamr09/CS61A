#lang racket
(require berkeley)
(require sugar)

; The procedure plays n games with a given strategy and returns 
; the number of games that the customer won minus the number that 
; s/he lost.  

; Load twenty-one procedure
(require "./00-twenty-one.rkt")

; The procedure plays n games and return the number of 
; times the customer won minus the number of times the 
; customer lost
; - strategy: whether the customer decides to take
;             a new card or not
; - n: number of times to play
; - score: times won - times lost
(define (play-n-helper strategy n score)
  (if (= n 0)
    ; If we have played n times return the score
    score
    ; Else keep playing
    (play-n-helper 
      strategy 
      (- n 1)
      ; + 1 if the customer wins
      ; -1 if the customer looses
      ; 0 if it is a tie
      (+ score (twenty-one strategy))
    )
    
  )
)

(define (play-n strategy n)
  (play-n-helper strategy n 0)
)

; Exports
(provide play-n)

