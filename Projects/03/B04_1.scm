;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B04_1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 1. Create a class called basic-object that keeps a local variable called properties containing 
;     an attribute-value table
; 2. You'll modify the person, place and thing classes so that they will inherit from basic-object. 
;     This object will accept a message put so that the following call does the right thing
;     > (ask Brian 'put 'strength 100)
; 3. The basic-object should treat any message not otherwise recognized as a request for the attribute 
;     of that name, so 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGES
;; - We created the basic-object class
;; - Add basic-object as parent of person, place and thing
;; - We added a new attribute to the person class: strength
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "./adv-world.scm")

(print (ask Brian 'strength))
; 50
(ask Brian 'put 'strength 100)
(print (ask Brian 'strength))
; 100
(print (ask Brian 'charisma))
; #f

