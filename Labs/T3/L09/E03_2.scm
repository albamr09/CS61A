;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; E03_2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given the procedure: 

(define (mystery x)
  (define (loop x y)
    ; If there are no more elements
    ; in x
    (if (null? x)
      ; Return null
      y
      (let 
        ; Obtain the pointer to the second
        ; part of the list x
        ((temp (cdr x)))
        ; Update the second element of the list x to
        ; y, which in this case are the last i elements
        ; of x in reverse order
        (set-cdr! x y)
        ; Continue reversing the elements
        (loop temp x)
      )
    )
  )
  (loop x '())
)

;; Example
;; Given a list x = (1 2 3), and y = '()
;; > Set temp = (2 3)
;; > Update cdr of x, so x = (1)
;; >> Set x = temp = (2 3) and y = x = (1), and call (loop x y)
;; >> Set temp = (3)
;; >> Update cdr of x, so x = (2 1)
;; >>> Set x = temp = (3) and y = x = (2 1), and call (loop x y)
;; >>> Set temp = null
;; >>> Update cdr of x, so x = (3 2 1)
;; >>>> Set x = temp = null and y = x = (3 2 1), and call (loop x y)
;; <<<< Return y, because x is null 
;; (3 2 1)
  

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

;What does mystery do in general?
; It reverses a list

;Draw the box-pointer diagram of v before the call to mystery, 
;v after the call to mystery, and w

;;; BEFORE

; (a, .) -> (b, .) -> (c, .) -> (d, \)

;;; DURING

; 1. (set-cdr! x y)
; (a, .) -> () 
; ----------------------
; Now x = temp = (cdr (a b c d)) = (b, .) -> (c, .) -> (d, \)
; and y = x = (a, .) -> ()
; 2. (set-cdr! x y)
; (b, .) -> (a, .) -> () 
; ----------------------
; Now x = temp = (cdr (b c d)) = (c, .) -> (d, \)
; and y = x = (b, .) -> (a, .) -> ()
; 3. (set-cdr! x y)
; (c, .) -> (b, .) -> (a, .) -> () 
; ----------------------
; Now x = temp = (cdr (c d)) = -> (d, \)
; and y = x = (c, .) -> (b, .) -> (a, .) -> ()
; 3. (set-cdr! x y)
; (d, .) -> (c, .) -> (b, .) -> (a, .) -> () 
; ----------------------
; Now x = temp = (cdr (d)) = -> null
; And so the algorithm finishes

;;; AFTER

; (d, .) -> (c, .) -> (b, .) -> (a, \)

;What would be printed as the values of v and w?

(print v)
; a
; Because as you can see is only in the first step that we modify v (= x), 
; and we set the rear pointer of the list to null, so now v = (a, \)

(print w)
; (d c b a)
