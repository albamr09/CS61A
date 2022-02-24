; Load OOP library 
(load "../../../lib/obj.scm")

; Load person class
(load "./E01.scm")

; Suppose that we want to define a class called double-talker to represent people that 
; always say things twice. For example, take a look at the following dialog.

; > (define mike (instantiate double-talker 'mike))
; mike 

; > (ask mike 'say '(hello))
; (hello hello) 

; > (ask mike 'say '(the sky is falling))
; (the sky is falling the sky is falling)

; Consider the following three definitions for the double-talker class:

; -------------------

(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) 
    (se (usual 'say stuff) (ask self 'repeat))
  )
) 

(define mike (instantiate double-talker 'mike))
(ask mike 'say '(hello))

; Given
; > (ask mike 'say '(hello))
; Returns
; (hello)
; (hello)

; -------------------

(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) 
    (se stuff stuff)
  ) 
) 

(define mike (instantiate double-talker 'mike))
(ask mike 'say '(hello))

; Given
; > (ask mike 'say '(hello))
; Returns
; (hello hello)

; -------------------

(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) 
    (usual 'say (se stuff stuff))
  ) 
)

(define mike (instantiate double-talker 'mike))
(ask mike 'say '(hello))

; Given
; > (ask mike 'say '(hello))
; Returns
; (hello hello)

; -------------------

; Determine which of these definitions work as intended. Determine also for which messages 
; the three versions would respond differently.
