(load "adv-world.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E03
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Sproul Hall has a particularly obnoxious exit procedure attached to 
; it. Fix sproul-hall-exit so that it counts how many times it gets 
; called, and stops being obnoxious after the third time.

; Remove old procedure
(ask s-h 'remove-exit-procedure sproul-hall-exit)

(define (sproul-hall-exit)
  (let 
    ((count 1))
    (lambda()
      ; While it has not been called three times
      (if (not(= count 3))
        (begin
          ; Update count value
          (set! count (+ count 1))
          (error "You can check out any time you'd like, but you can never leave")
        )
      )
    )
  )
)

; Add new procedure
(ask s-h 'add-exit-procedure (sproul-hall-exit))

; BH-Office to art-gallery
(ask Brian 'go 'east)
; art-gallery to Soda
(ask Brian 'go 'down)
; Soda to Pimentel 
(ask Brian 'go 'south)
; Pimentel to Haas-Business-School
(ask Brian 'go 'south)
; Haas-Business-School to sproul-hall
(ask Brian 'go 'west)

; Failure
; (ask Brian 'go 'west)
; ; Failure
; (ask Brian 'go 'west)
; ; Success
; (ask Brian 'go 'west)

