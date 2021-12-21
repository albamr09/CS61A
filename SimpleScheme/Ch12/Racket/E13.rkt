#lang racket

(require berkeley)

; Write a new version of the procedure from Exercise 6.14.
; Instead of returning a decimal number, it should behave like this:

; > (describe-time 22222)
; (6 HOURS 10 MINUTES 22 SECONDS)
; > (describe-time 4967189641)
; (1 CENTURIES 57 YEARS 20 WEEKS 6 DAYS 8 HOURS 54 MINUTES 1 SECONDS)

(define (time i)
  (item i '(3155760000 31557600 604800 86400 3600 60))
)

(define (time-name i)
  (item i '(centuries years weeks days hours minutes))
)

(define (round-time t i)
  (/ 
    (- t 
       (remainder t (time i))
    ) 
    (time i)
  )
)

(define (print-time? t i)
  (if (> t 0)
    (se t (time-name i))
    '()
  )
)

(define (recursive-time t i)
  (if(< t 60)
    (se t 'seconds)
    (se 
      (print-time? (round-time t i) i) 
      (recursive-time 
        (remainder t (time i)) 
        (+ 1 i)
      )
    )
  )
)

(define (describe-time t)
  (recursive-time t 1)
)

(describe-time 22222)
; (6 HOURS 10 MINUTES 22 SECONDS)
(describe-time 4967189641)
; (1 CENTURIES 57 YEARS 20 WEEKS 6 DAYS 8 HOURS 54 MINUTES 1 SECONDS)
