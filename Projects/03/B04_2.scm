;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B04_2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 1. Add a method named person? to the person class, and have it always return #t
; 2. Similarly, places should have a place? method, and things a thing? method.
; 3. Add these type methods and change the implementation of the type predicate procedures

(load "adv-world.scm")

(print (person? Brian))
; #t
(print (person? art-gallery))
; #f
(print (person? bagel))
; #f
(print (thing? bagel))
; #t
(print (thing? Brian))
; #f
(print (thing? art-gallery))
; #f
(print (place? art-gallery))
; #t
(print (place? Brian))
; #f
(print (place? bagel))
; #f

(define cinema-ticket (instantiate ticket 10))
(print (person? cinema-ticket))
; #f
(print (thing? cinema-ticket))
; #t
(print (place? cinema-ticket))
; #f

(define walmart-parking (instantiate garage 'Walmarts-parking))
(print (person? walmart-parking))
; #f
(print (thing? walmart-parking))
; #f
(print (place? walmart-parking))
; #t
