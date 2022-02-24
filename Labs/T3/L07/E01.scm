; Load OOP library 
(load "../../../lib/obj.scm")

; Modify the person class given in the template to add a repeat method, which 
; repeats the last thing said. 

(define-class (person name)
  ; Define a variable to store the previous 
  ; thing that was said
  (instance-vars
    (previous-stuff '())
  )
  (method (say stuff)
    ; Update the last thing said
    (set! previous-stuff stuff)
    stuff
  )
  (method (ask stuff) 
    (ask self 
      'say (se '(would you please) stuff)
    )
  )
  (method (greet) 
    (ask self 
      'say (se '(hello my name is) name)
    )
  )
  ; Return the last thing that was said
  (method (repeat)
    previous-stuff
  )
)

(define brian (instantiate person 'brian))
; brian 

(ask brian 'repeat)
; () 

(ask brian 'say '(hello))
; (hello) 

(ask brian 'repeat)
; (hello) 

(ask brian 'greet)
; (hello my name is brian) 

(ask brian 'repeat)
; (hello my name is brian) 

(ask brian 'ask '(close the door))
; (would you please close the door) 

(ask brian 'repeat)
; (would you please close the door)
