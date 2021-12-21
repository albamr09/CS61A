#lang racket
(require berkeley)

; Load strategies
(require "./02-stop-at-17-strategy.rkt")
(require "./04-dealer-sensitive.rkt")
(require "./05-stop-at-n.rkt")
(require "./06-valentine.rkt")
(require "../07-suit.rkt")
(require "../08-majority.rkt")
(require "../09-reckless.rkt")

; Load method to play n times
(require "./03-play-n.rkt")


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ; Test strategy stop-at-17
; 
; (play-n stop-at-17 1)
; 
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ; Test strategy dealer-sensitive
; (play-n dealer-sensitive 1)
; 
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ; Test strategy stop-at
; (play-n (stop-at 15) 1)
; (play-n (stop-at 16) 1)
; (play-n (stop-at 17) 1)
; 
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ; Test strategy valentine
; ; Keeps playing: best-total < 17
; (valentine '(10d 6d) '5d)
; ; Stops playing: best-total > 17
; (valentine '(10d 8d) '5d)
; ; Keeps playing: there is a heart (h) and best-total < 19
; (valentine '(10h 8d) '5d)
; ; Stops playing: there is a heart (h) and best-total > 19
; (valentine '(10h 9d) '5d)
; ; Stops playing because there is a heart and the best-total = 21 > 19
; (valentine '(10h J) '5d)
; ; (play-n valentine 10)
; 
; 
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ; Test strategy suit-strategy
; ; Executing commenting one an uncommenting the other to see the same behaviour
; (play-n valentine 1)
; ; Redefine valentine using the generalization
; (play-n
;   (suit-strategy 'h (stop-at 19) (stop-at 17))
;   1
; )
; 
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ; Test strategy majority
; ; Should return true, because stop-at 18 and stop-at 19 return true
; ((majority (stop-at 17) (stop-at 18) (stop-at 19)) '(10d 7d) '5s)
; 
; ;; Should return false, because stop-at 18 and stop-at 17 return false
; ((majority (stop-at 17) (stop-at 18) (stop-at 19)) '(10d 8d) '5s)
; 
; ;; Should return false, all strategies return false
; ((majority (stop-at 17) (stop-at 18) (stop-at 19)) '(10d 9d) '5s)
; 
; ;; Should return true, all strategies return true
; ((majority (stop-at 17) (stop-at 18) (stop-at 19)) '(10d 2d) '5s)
; 
; (play-n
;   ; Use three strategies and decide by what the majority says
;   (majority
;     valentine
;     (stop-at 14)
;     (stop-at 16)
;   )
;   1
; )
; 
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ; Test strategy reckless
; ; Should return true, because when the customer's card only was
; ; 10, the strategy returned true
; ((reckless (stop-at 15)) '(10h 7h) '4h)
; 
; ; Should return false, because when the customer's cards
; ; 10h and 7h, the strategy returned false
; ((reckless (stop-at 15)) '(10h 7h 8d) '4h)

