; Load OOP library
(load "../../../lib/obj.scm")

; We define several classes to then see the translation in scheme syntax

(define-class (person) 
  (method (smell-flowers) 'Mmm!)
)
(define-class (fruit-lover fruit) 
  (method (favorite-food) fruit)
)

(define-class (banana-holder name)
  (parent (person) (fruit-lover 'banana))
  (class-vars (list-of-banana-holders '()))
  (instance-vars (bananas 0))
  (initialize
    (set! list-of-banana-holders (cons self list-of-banana-holders))
  ) 
  (method (get-more-bananas amount)
    (set! bananas (+ bananas amount))
  )
  (default-method 'sorry)
)

; Complete translation of a define-class for person
(show-class 'person)
; Complete translation of a define-class for fruit-lover
(show-class 'fruit-lover)
; Complete translation of a define-class for banana-holder
(show-class 'banana-holder)
