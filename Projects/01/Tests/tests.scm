; Load strategies
(load "./02-stop-at-17-strategy.scm")
(load "./04-dealer-sensitive.scm")
(load "./05-stop-at-n.scm")
(load "./06-valentine.scm")

(load "./03-play-n.scm")

; See if the customer takes a new card
(trace valentine)
(trace best-total)
; See the scores
(trace best-total-helper)
; See when the match ends
(trace play-n-helper)

; (play-n stop-at-17 1)
; (play-n dealer-sensitive 1)

; The customer does not take another card and wins
; (play-n (stop-at 15) 1)
; The customer does not take another card and wins
; (play-n (stop-at 16) 1)
; The customer takes another card and looses
; (play-n (stop-at 17) 1)

; When n = 3 there is an ace
; (play-n valentine 10)
