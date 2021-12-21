#lang racket

(require berkeley)

; Write a procedure that takes a number of seconds as its argument
; and returns a more useful description of that amount of time:

(define (describe-time t)
  (cond
    ((<= t 60) (se t 'seconds))
    ((<= t 3600) (se (/ t 60.0) 'minutes))
    ((<= t 86400) (se (/ t 3600.0) 'hours))
    ((<= t 31536000) (se (/ t 86400.0) 'days))
    ((<= t 315576000) (se (/ t 31557600.0) 'years))
    ((<= t 3155760000) (se (/ t 315576000.0) 'decades))
    ((<= t 31557600000) (se (/ t 3155760000.0) 'centuries))
    (else '(That is going too far for me!))
  )
)

(describe-time 45)
; (45 SECONDS)
(describe-time 930)
; (15.5 MINUTES)
(describe-time 30000000000)
; (9.506426344208686 CENTURIES)
