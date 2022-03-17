;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B05_1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 1. Define a hotspot as a kind of place that allows network connectivity.
; 2. Each hotspot should have a name and a password as instantiation variables
; 3. The hotspot has a connect method with two arguments, a laptop (thing to be invented)
;     and a password
; 4. If the password is correct, and the laptop is in the hotspot, add it to a list of connected 
;     laptops otherwise, return an error.
; 5. When the laptop leaves the hotspot, remove it from the list.
; 6. Hotspots also have a surf method with two arguments, a laptop and a text string, such as
      ; "http://www.cs.berkeley.edu"
; 7. If the laptop is connected to the network, then the surf method should
;     (system (string-append "lynx " url))

(load "adv-world.scm")

(define Tima (instantiate thing 'Tima))
(define Kenichi (instantiate thing 'Kenichi))
(define Metropolis (instantiate hotspot 'Metropolis 'Duke-red))
(ask Metropolis 'appear Tima)

(ask Metropolis 'connect Tima 'Duke-red)
(print (ask Metropolis 'connections))
; (Tima)

(ask Metropolis 'surf Tima "google.com")
(ask Metropolis 'gone Tima)
(print (ask Metropolis 'connections))
; ()
(print (ask Metropolis 'things))
; ()

;;; TEST ERRORS

; (ask Metropolis 'connect Tima 'Duke-red)
; "The laptop you are trying to connect is out of range, as in, not in Metropolis"

; (ask Metropolis 'appear Tima)
; (ask Metropolis 'connect Tima 'Ziggurat)
; It sure seems the password is not correct miss

; (ask Metropolis 'gone Tima)
; Disappearing thing not here

; (ask Metropolis 'appear Kenichi)
; (ask Metropolis 'connect Kenichi 'Duke-red)
; (ask Metropolis 'connect Kenichi 'Duke-red)
; Oh... I do not understand, you are already connected

; (ask Metropolis 'surf Kenichi "google.com")
; You have to connect before you surf The Internet, hmm..
