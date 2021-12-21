#lang racket
(require berkeley)
(require sugar)

; Load functions to obtain the best score overall
(require "./10-aces.rkt")

; The procedure should return the largest possible total that’s less than or equal to
; 21, if possible
(define (best-total hand)
  ; Start the search with i = 1, best = 0, and ace-hand empty
  (generate-all-hands hand 1 0 #f)
)

; Exports
(provide best-total)


; (best-total '(ad as 9h)) ; here one counts as 11 and the other as 1
; 21
; (best-total '(ad 8s 5h)) ; here it must count as 1 to avoid busting
; 14
; (best-total '(ad 8s))
; 19
