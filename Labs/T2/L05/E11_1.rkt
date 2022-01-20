
#lang racket
(require berkeley)

; Programming by example: In some programming systems, instead of writing an algorithm, you give 
; examples of how you'd like the program to behave, and the language figures out the algorithm itself

(define (regroup pattern lst)
  (define (regroup-recursive pattern lst)
    (cond
      ((null? pattern) '())
      ((equal? (car pattern) '...) '())

      ((pair? (car pattern)) (regroup-pair pattern lst))
      ((> (car pattern) (count lst)) 
       (regroup-recursive (cdr pattern) lst)
      )
      (else
        (cons
          (list-ref
            lst
            (- (car pattern) 1)
          )
          (regroup-recursive (cdr pattern) lst)
        )
      )
    )
  )
  (cond
    ((pair? (car pattern)) (regroup-pair pattern lst))
    ((> (car pattern) (count lst)) '())
    ; Infinite pattern
    ((member? '... pattern)
      (append 
        (regroup-recursive pattern lst)
        (regroup (update-pattern pattern (obtain-offset pattern)) lst)
      )
    )
    ; Fixed pattern
    (else
      (regroup-recursive pattern lst)
    )
  )
)

(define (regroup-pair pattern lst)
  (define (regroup-pair-recursive pattern lst)
    (cond
      ((null? pattern) '())
      ((equal? (car pattern) '...) '())
      (else
        (cons
          (regroup (car pattern) lst)
          (regroup-pair-recursive (cdr pattern) lst)
        )
      )
    )
  )
  (cond
    ((null? pattern) '())
    ((> (caar pattern) (count lst)) '())
    ((member? '... pattern)
      (append 
        (regroup-pair-recursive pattern lst)
        (regroup (update-pattern pattern (obtain-offset pattern)) lst)
      )
    )
    (else
      (regroup-pair-recursive pattern lst)
    )
  )
)

(define (obtain-offset pattern)
  (cond
    ((or 
       (null? pattern) 
       (equal? (car pattern) '...)
      )  
      0
    )
    ((pair? (car pattern))
      (*
        (+
          (total-difference (car pattern))
          (obtain-offset (cdr pattern))
        )
        (- (count pattern) 1)
      )
    )
    (else
      (* (total-difference pattern) (- (count pattern) 1))
    )
  )
)

(define (total-difference pattern)
  (cond
    ((or 
       (= (count pattern) 1) 
       (equal? (cadr pattern) '...)
      ) 
      0
    )
    (else
      (+
        (-
          (cadr pattern)
          (car pattern)
        )
        (total-difference (cdr pattern))
      )
    )
  )
)

(define (update-pattern pattern offset)
  (map
    (lambda
      (x)
      (cond
        ((number? x)
          (+ x offset)
        )
        ((pair? x)
          (update-pattern x offset)
        )
        (else
          '...
        )
      )
    )
    pattern
  )
)


; (trace regroup regroup-pair obtain-offset total-difference)
(regroup '((1 2) (3 4) ...) '(the rain in spain stays mainly on the plain))
(regroup '((1 2) (2 3 ...) ...) '(the rain in spain stays mainly on the plain))
