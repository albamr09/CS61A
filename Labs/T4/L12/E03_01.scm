;  Suppose that (in ordinary applicative-order Scheme) we define unless as shown above and then define
; factorial in terms of unless as

; (define (factorial n)
;   (unless 
;     (= n 1)
;     (* n (factorial (- n 1)))
;     1
;   )
; )

; What happens if we attempt to evaluate (factorial 5)? Will our definitions work in a normal-order language?

; 1. We have to check if n == 1, so we have to evaluate n
;
; ((factorial 5)
;   (unless 
;     (= 5 1)
;     (* n (factorial (- n 1)))
;     1
;   )
; )
;
; 2. Thus n is now a local variable attached to the value "5"
; 3. Now, we have to apply a primitive operation "*" over n (whose value we know) and (factorial (- n 1)), which we still need to compute

; ((factorial 5)
;   (unless 
;     (= 5 1)
;     (* 5 (factorial (- n 1)))
;     1
;   )
; )

; 4. Again, because we have to check if n == 1, we have to evaluate (- n 1) = 4

; ((factorial 4)
;   (unless 
;     (= 4 1)
;     (* 4 (factorial (- 4 1)))
;     1
;   )
; )

; However in applicative order what will happen is Scheme will try to evaluate every argument until there are only primitive procedures and entities, hence:

; ((factorial 5)
;   (unless 
;     (= 5 1)
;     (* n (factorial (- n 1)))
;     1
;   )
; )

; Now, before applying *, Scheme evaluates (factorial (- n 1))

; ((factorial 4)
;   (unless 
;     (= 4 1)
;     (* n (factorial (- n 1)))
;     1
;   )
; )

; The same thins happens again and again, and Scheme does not evaluate the if until it has evaluated all of the arguments 


; Let's try it:

(define (unless condition usual-value exceptional-value)
  (if condition 
    exceptional-value 
    usual-value
  )
)

(define (factorial n)
  (unless 
    (= n 1)
    (* n (factorial (- n 1)))
    1
  )
)

(trace factorial)

; Now enter on the interpreter (factorial 5)
; Infinite loop
; (print (factorial 5))
