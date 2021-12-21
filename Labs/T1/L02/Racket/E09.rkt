#lang racket
(require berkeley)

; Last week you wrote procedures squares, that squared each number in its argument 
; sentence, and saw pigl-sent, that pigled each word in its argument sentence. 
; Generalize this pattern to create a higher-order procedure called every that applies an arbitrary 
; procedure, given as an argument, to each word of an argument sentence.

(define (every fn sent)
  (if (empty? sent)
    ; If empty do no append anything and return
    '()
    ; Else append the image of the first elemento f sent under fn and call 
    ; recursively every removing the first element of sent
    (se (fn (first sent)) (every fn (bf sent)))
  )
)

(every square '(1 2 3 4)) ;; (1 4 9 16)
(every first '(nowhere man)) ;; (n m)
