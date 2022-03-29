;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; E08_1: SICP 3.19
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Write a procedure that examines a list and determines whether it contains a cycle, that is, whether a
; program that tried to find the end of the list by taking successive cdrs would go into an infinite loop.

; Load make-cycle function
(load "E03_1.scm")

; Given a list with a cycle:

; (a, .) -> (b, .) -> (c, .)
;  ^                   |
;  |___________________|

; To check if the list contains a cycle, for each element
; we check if its cdr points to an element before it in the list.
; In order to achieve that we store in an auxiliary list the elements 
; we have already examined (the ones that are before in the list)

(define (cycle? lst)
  (define (loop lst seen)
    (cond
      ; If it is emtpy, no element points to one before it
      ((empty? lst) #f)
      ; If it is not a list, there is no cycle
      ((not (pair? lst)) #f)
      (else
        ; If the current element is in the already examined list
        ; there is a loop
        (if (member? (car lst) seen)
          #t
          ; Keep going through the list, and update
          ; the already examined elements list
          (loop (cdr lst) (append seen (list (car lst))))
        )
      )
    )
  )
  (loop lst '())
)

(define x (list 1 2 3))
(print (cycle? x))
; #f
(print (cycle? z))
; #t

; This is based on the Robert Floyd cycle detection algorithm, whose basic idea is: given an array, if there
; is a cycle there is an element x_i in the list such that x_i = x_2i, for some index i.
; So, in our case, to check if there is a cycle we keep two pointers, one would represent x_i (= slow), and
; the other would represent x_2i (= fast), and by calling iteratively we find an i, such that x_i = x_2i (slow = fast). If no such x_i exists, and the list ends, then there is no cycle.

(define (has-loop? x) 
  (define (check slow fast) 
    (cond 
      ; If both are the same, then there is a cycle
      ((eq? slow fast) #t) 
      ; If some any of the pointers are null, the list is not infinite, so
      ; there is not a cycle
      ((or 
        (null? (cdr fast)) 
        (null? (cddr fast))) 
        #f
      ) 
      ; Else keep going through the list
      (else 
        (check (cdr slow) (cddr fast))
      )
    )
  ) 
  ; Start going through the list
  (check x (cdr x))
) 

(print (has-loop? x))
; #f
(print (has-loop? z))
; #t
