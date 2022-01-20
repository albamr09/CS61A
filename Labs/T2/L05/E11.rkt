#lang racket
(require berkeley)

; Programming by example: In some programming systems, instead of writing an algorithm, you give 
; examples of how you'd like the program to behave, and the language figures out the algorithm itself:

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

(define (regroup pattern)
  (lambda
    (lst)
    (regroup-iter pattern lst (find-biggest-index pattern))
  )
)

(define (regroup-iter pattern lst offset)
  (cond
    ((and (not (pair? (car pattern))) (>= (select-index pattern) (count lst))) null)
    ((and (pair? (car pattern)) (>= (select-index (car pattern)) (count lst))) null)
    (else
      (filter
        not-null?
        (append
          (map
            (lambda
              (i)
              (cond
                ((pair? i)
                  (regroup-iter
                    i
                    lst
                    offset
                  )
                )
                ((and (number? i) (< (- i 1) (count lst)))
                  (list-ref lst (- i 1))
                )
                (else null)
              )
            )
            pattern
          )
          (if (is-fixed? pattern)
            '()
            (regroup-iter
              (update-pattern pattern offset)
              lst
              offset
            )
          )
        )
      )
    )
  )
)


;; AUX FUNCTIONS

(define (find-biggest-index pattern)
  (accumulate
    max
    ; Do not pass '... to max
    (filter
      number?
      ; Handle elements that are lists
      (map
        (lambda
          (x)
          ; If the element is a pair, find the 
          ; max of the pair, else return the element
          (if (pair? x)
            (find-biggest-index x)
            x
          )
        )
        pattern
      )
    )
  )
)

(define (select-index pattern)
  (- (car pattern) 1)
)

(define (not-null? x) 
  (not (null? x))
)

(define (is-fixed? pattern)
  (not (member? '... pattern))
)


;,(regroup-main '((1 3 ...) (2 4 ...)) '(1 2 3 4 7 8))
;,(regroup-main '(2 1 4 3 ...) '(the rain in spain stays mainly on the plain))
;,(regroup-main '((1 2) (3 4) ...) '(the rain in spain stays mainly on the plain))
(trace update-pattern)
(define pairup (regroup '((1 2 ...) (2 3 ...) ...)))
(pairup '(the rain in spain stays mainly on the plain))
; ((the rain) (in spain) (stays mainly) (on the))
