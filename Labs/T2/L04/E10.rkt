#lang racket
(require berkeley)

; Write the procedure cxr-function that takes as its argument a word starting with c, ending with r,
; and having a string of letters a and/or d in between, such as cdddadaadar. It should return the 
; corresponding function.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXAMPLE
; ((cxr-function 'cddr) (list 1 2 3))
; -> ((cxr-function 'ddr) x)
; --> ((cxr-function 'dr) (cdr x)), (cdr x) = '(2 3)
; ---> ((cxr-function 'r) (cdr (cdr x))), (cdr (cdr x)) = '(3)
; ----> ((cxr-function ') (cdr (cdr x))), (cdr (cdr x)) = '(3)
; ----> wd is empty, so return '(3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXAMPLE
; ((cxr-function 'cdar) (list 1 2 3))
; -> ((cxr-function 'ddr) x)
; --> ((cxr-function 'dr) (cdr x)), (cdr x) = '(2 3)
; ---> ((cxr-function 'r) (car (cdr x))), (car (cdr x)) = 2
; ----> ((cxr-function ') (car (cdr x))), (car (cdr x)) = 2
; ----> wd is empty, so return 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cxr-function wd)
  (lambda
    ; Formal parameter
    (x)
    (cond 
      ; If there are no more letters return result
      ((empty? wd) x)
      ; If the next letter is d
      ((equal? (first wd) 'd)
        (
         ; Remove first element of wd
         (cxr-function (bf wd)) 
         ; Update the formal parameter by applying 
         ; cdr to the current formal parameter
         (cdr x)
        )
      )
      ; If the next letter is a
      ((equal? (first wd) 'a)
        (
          ; Remove first element of wd
          (cxr-function (bf wd)) 
          ; Update the formal parameter by applying 
          ; car to the current formal parameter
          (car x)
        )
      )
      ; Else if it is another letter just remove
      ; first element of wd
      (else ((cxr-function (bf wd)) x))
    )
  )
)

((cxr-function 'cddr) (list 1 2 3))
; 3
((cxr-function 'cdar) (list 1 2 3))
; 2
((cxr-function 'cdddddr) (list 1 2 3 4 5 6 7 8 1 2 3 4))
; (6 7 8 1 2 3 4) 
((cxr-function 'cdddddar) (list 1 2 3 4 5 6 7 8 1 2 3 4))
; 6 


