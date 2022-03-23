;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 4.2.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SICP 3.17 Write a correct version of count-pairs. (refer to E04_1)


; This method counts all of the lists given a list, including itself, so given the list ((1 2) (3 4) (5 (6))), the number of lists are 5

(define (count-pairs x)
  (if (not (pair? x))
    0
    (+
      (count-pairs (car x))
      (if ;(or
            (and
              (= (count (cdr x)) 1)
              (not (pair? (cadr x)))
            )
          ;  (= (count (cdr x)) 0)
          ;)
        0
        (count-pairs (cdr x))
      )
      1
    )
  )
)

; This method counts the number of sublists inside a list, without recursiveness, so
; given the list ((1 2) (1 2) (1 2)), the number of sublists are 3

; (define (count-pairs-1 x)
;   (define (loop lst)
;     (cond
;       ; If the list is empty
;       ((null? lst) 0)
;       ; If the first item of the list is 
;       ; a pair update the count by one
;       ((pair? (car lst))
;         (+ 
;           1
;           ; Keep on counting on the rest of the elements of 
;           ; the list
;           (loop (cdr lst))
;         )
;       )
;     )
;   )
;   (loop x)
; )

(print (count-pairs (list 1)))
; 1
(print (count-pairs (list (list 1 2) (list 3 4) (list 5 6))))
; 4
(print (count-pairs (list (list 1 2) (list 3 4) (list 5 (list 6)))))
; 5

; (print (count-pairs-1 (list (list 1 2) (list 1 2) (list 2))))
; 2
