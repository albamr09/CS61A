#lang racket
(require berkeley)

; Coercion
; Transform a ordinary number to a complex number
(define (scheme-number->complex n)
  (make-complex-from-real-imag 
    ; Strip n of the tag, get the value
    (contents n) 
    ; Imaginary part is zero
    0
  )
)

; Apply generic using coercion
(define (apply-generic op . args)
  (let 
    ; Obtain the types of the arguments (i.e. (complex complex))
    ((type-tags (map type-tag args)))
    (let 
      ; Obtain the procedure for the operation name
      ; and the data type, given by the tags
      ((proc (get op type-tags)))
      ; If there exists a procedure for the pair, operation-type
      (if proc
        ; Apply the procedure
        (apply proc (map contents args))
        ; Else, check if the data type of some arg can be "coerced" into
        ; another type
        ; Only implemented for two arguments (for simplicity's sake)
        (if (= (length args) 2)
          (let 
            (
              ; Type of first arg
              (type1 (car type-tags))
              ; Type of second arg
              (type2 (cadr type-tags))
              ; Value of first arg
              (a1 (car args))
              ; Value of second arg
              (a2 (cadr args))
            )
            (let 
              (
                ; Obtain the procedures that transforms
                ; a1 to the type of a2
                (t1->t2 (get-coercion type1 type2))
                ; Obtain the procedures that transforms
                ; a2 to the type of a1
                (t2->t1 (get-coercion type2 type1))
              )
              (cond 
                ; If there exists a conversion procedure for a1 to a2's
                ; type, apply the operation given by op to a1 transformed and
                ; a2
                (t1->t2 (apply-generic op (t1->t2 a1) a2))
                ; If there exists a conversion procedure for a2 to a1's
                ; type, apply the operation given by op to a2 transformed and
                ; a1
                (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                ; If no such procedures exists, raise error
                (else 
                  (error 
                    "No method for these types"
                    (list op type-tags)
                  )
                )
              )
            )
          )
          ; Only implemented for a fixed number of args
          (error 
            "No method for these types"
            (list op type-tags)
          )
        )
      )
    )
  )
)
