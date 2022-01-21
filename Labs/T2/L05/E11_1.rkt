
#lang racket
(require berkeley)

; Programming by example: In some programming systems, instead of writing an algorithm, you give 
; examples of how you'd like the program to behave, and the language figures out the algorithm itself

(define (regroup pattern)
  (lambda
    (lst)
    (regroup-single pattern lst)
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REGROUP NON PAIR PATTERN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Apply a pattern of the form (1 2 3 4 ...)

(define (regroup-single pattern lst)

  ; Apply a pattern block (i.e. (1 2 3 4)) to the list
  (define (regroup-single-recursive pattern lst)
    (cond
      ; If the pattern is null finish
      ((null? pattern) '())
      ; If current element is '... finish (last symbol)
      ((equal? (car pattern) '...) '())
      ; If the first element of the pattern is a list
      ; then the pattern is of the form ((1 2 ...) (3 4 ...))
      ((pair? (car pattern)) (regroup-pair pattern lst))
      ; If the first element of the pattern (index) overflows the list
      ; then do not map and try the next element
      ((> (car pattern) (count lst)) 
       (regroup-single-recursive (cdr pattern) lst)
      )
      ; Else map the element of the list by the index in the pattern
      (else
        (cons
          ; Obtain element on index (car pattern)
          (list-ref
            lst
            ; - 1 because the index starts on 0, not 1
            (- (car pattern) 1)
          )
          ; Continues mapping
          (regroup-single-recursive (cdr pattern) lst)
        )
      )
    )
  )
  (cond
    ; If the first element of the pattern is a list
    ; then the pattern is of the form ((1 2 ...) (3 4 ...))
    ((pair? (car pattern)) (regroup-pair pattern lst))
    ; If the first element of the pattern overflows the list
    ; then stop regrouping
    ((> (car pattern) (count lst)) '())
    ; If the current element is '... 
    ; the pattern needs to be updated periodically
    ((member? '... pattern)
      (append 
        ; Apply the pattern 
        (regroup-single-recursive pattern lst)
        ; Once applied, continue applying the updated pattern
        (regroup-single (update-pattern pattern (obtain-offset pattern)) lst)
      )
    )
    ; If '... is not in the pattern then we do not need to update it
    (else
      ; Apply the pattern
      (regroup-single-recursive pattern lst)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REGROUP PAIR PATTERN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Apply a pattern of the form ((1 2 ..) (3 4 ...) ...)

(define (regroup-pair pattern lst)
  (define (regroup-pair-recursive pattern lst)
    (cond
      ; If the pattern is null finish
      ((null? pattern) '())
      ; If the first element of the pattern is '..., then we
      ; have reached the end
      ((equal? (car pattern) '...) '())
      ; Else continue applying the pattern
      (else
        (cons
          ; Apply the first subpattern (i.e. (1 2 ...))
          (regroup-single (car pattern) lst)
          ; Continues applying recursively (i.e. (3 4 ...) ...)
          (regroup-pair-recursive (cdr pattern) lst)
        )
      )
    )
  )
  (cond
    ; If the pattern is null finish
    ((null? pattern) '())
    ; If the first element of the first subpattern overflows the list
    ; finish
    ((> (caar pattern) (count lst)) '())
    ; If the current element is '... 
    ; the pattern needs to be updated periodically
    ((member? '... pattern)
      (append 
        ; Apply first batch of the pattern: ((1 2 ..) (3 4 ...) ...)
        (regroup-pair-recursive pattern lst)
        ; Continue applying after updating the pattern: ((5 6 ...) (7 8 ...) ...)
        (regroup-single (update-pattern pattern (obtain-offset pattern)) lst)
      )
    )
    ; If '... is not in the pattern then we do not need to update it
    (else
      ; Apply the pattern: ((1 2 ..) (3 4 ...) ...)
      (regroup-pair-recursive pattern lst)
    )
  )
)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UPDATE PATTERN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Calculate the offset to update the pattern
(define (obtain-offset pattern)
  (cond
    ((or 
       (null? pattern) 
       (equal? (car pattern) '...)
      )  
      0
    )
    ; If the first element is a pair
    ; sum all of the total differences between the subpatterns
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
      ; The offset is the differences between every two elements
      ; multiplied by the number of elements (-1 because '... is not
      ; counted as elements)
      (* (total-difference pattern) (- (count pattern) 1))
    )
  )
)

; Obtain the differences between every two elements
(define (total-difference pattern)
  (cond
    ; If there is only one element left
    ; or '... is one of the pair finish
    ((or 
       (= (count pattern) 1) 
       (equal? (cadr pattern) '...)
      ) 
      0
    )
    (else
      ; Accumulate differences recursively
      (+
        ; Obtain difference between adyacen elements
        (-
          (cadr pattern)
          (car pattern)
        )
        ; Obtain rest of differences
        (total-difference (cdr pattern))
      )
    )
  )
)

; Update every element of the pattern, by summing
; the offset

(define (update-pattern pattern offset)
  (map
    (lambda
      ; For every element x in pattern
      (x)
      (cond
        ; If it is a number sum
        ((number? x)
          (+ x offset)
        )
        ; If the element is a list update the subpattern
        ((pair? x)
          (update-pattern x offset)
        )
        ; else x = '... then map it to '...
        (else
          '...
        )
      )
    )
    pattern
  )
)


;(define pairup (regroup '((1 2) (3 4) ...)))
;(pairup '(the rain in spain stays mainly on the plain))
;(define overlap (regroup '((1 2) (2 3) ...)))
;(overlap '(the rain in spain stays mainly on the plain))
;(define tails (regroup '((1 2 ...) (2 3 ...) ...)))
;(tails '(the rain in spain stays mainly on the plain))
;(define swap (regroup '(2 1 4 3 ...)))
;(swap '(the rain in spain stays mainly on the plain))
;(define split (regroup '((1 3 ...) (2 4 ...))))
;(split '(the rain in spain stays mainly on the plain))
