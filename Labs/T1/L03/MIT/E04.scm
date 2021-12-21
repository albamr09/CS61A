; Write type-check. Its arguments are a function, a type-checking predicate that returns #t 
; if and only if the datum is a legal argument to the function, and the datum.

(define (type-check f condition input)
  ; If the condition over the input is met
  (if (condition input)
    ; Return the value on the input under the function
    (f input)
    ; Return false if the data is not of the required type
    #f
  )
)
 
 
;; This should output #f
(type-check sqrt number? 'hello)
 
;; This should output 2
(type-check sqrt number? 4)
