; Load OOP
(load "../../../lib/obj.scm")
; Load person class
(load "./E01.scm")

; We want to promote politeness among our objects. Write a class miss-manners that takes an object as 
; its instantiation argument. The new miss-manners object should accept only one message, namely please. 
; The arguments to the please message should be, first, a message understood by the original object, 
; and second, an argument to that message. (Assume that all messages to the original object require 
; exactly one additional argument.)

(define BH (instantiate person 'Brian))
; BH
(ask BH 'say 'hi!!)
; hi!!

(define-class (miss-manners object)
  ; ". args" obtains the list of arguments
  ; after please
  (method (please . args)
    (ask object 
      ; Obtain the first argument: operation name
      (car args) 
      ; Obtain the rest of the arguments: operators
      ; for the operation
      (cadr args)
    )
  )
)

(define fussy-BH (instantiate miss-manners BH))
; (ask fussy-BH 'greet)
; ERROR: NO METHOD SAY
(ask fussy-BH 'please 'say 'hi!!)
; hi!!
