(load "./adv-world.scm")

;; 5. We have provided a definition of the Thing class that does not use the object-oriented programming syntax described in the handout. Translate it into the new notation.

(define-class (thing name)
  ; Instance variables
  (instance-vars
    (possessor 'no-one)
  )
  
  ; Methods
  (method (type) 'thing)
  (method (change-possessor new-possessor)
    (set! possessor new-possessor)
  )
)

;; 6. Write a procedure whereis that takes a person as its argument and returns the name 
;; of the place where that person is. 

(define (whereis obj)
  ; Check if the object is of type thing
  (if (person? obj)
    (print
      (ask (ask obj 'place) 'name)
    )
  )
)

(whereis Brian)
; BH-Office

;; Write a procedure owner that takes a thing as its argument and returns the name of the 
;; person who owns it. (Make sure it works for things that aren't owned by anyone.)

(define (owner obj)
  ; Check if the object is of type thing
  (if (thing? obj)
    (let
      ; Obtain the possessor
      ((possessor (ask obj 'possessor)))
      ; If the possesor exists/is a person
      (if (person? possessor)
        ; Obtain the name
        (print
          (ask possessor 'name)
        )
      )
    )
  )
)

; Create thing
; (define bagel (instantiate thing-class 'bagel))
; Change owner
(ask bagel 'change-possessor Brian)
(owner bagel)
; Brian
