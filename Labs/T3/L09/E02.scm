;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; E02
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Provide the arguments for the two set-cdr! operations in the blanks below to produce the indicated 
; effect on list1 and list2. Do not create any new pairs; just rearrange the pointers to the existing ones.


(define list1 (list (list 'a) 'b))
; list1 

(define list2 (list (list 'x) 'y))
; list2 

(set-cdr! (car list2) (cdr list1))
;okay 

(set-cdr! (car list1) (car list2))
;okay 

(print list1)
; ((a x b) b) 

(print list2)
; ((x b) y)

; After filling in the blanks in the code above and producing the specified effect on list1 
; and list2, draw a box-and-pointer diagram that explains the effect of evaluating the expression 
; (set-car! (cdr list1) (cadr list2)).

; Given 
; - (cdr list1) = ((b))
; - (cadr list2) = 'y

; list1 is represented as follows
; (. , .) -> (b, \)
;  |
;  (a, .) 
;      |
;   (car list2) = (x, (cdr list1)) = (x, .) -> (b, \) 

; So after (set-car! (cdr list1) (cadr list2)):
;
; (. , .) -> (cdr list1) = (y, \)
;  |
;  (a, .) 
;      |
;   (car list2) = (x, (cdr list1)) = (x, .) -> (y, \) 

(set-car! (cdr list1) (cadr list2))
(print list1)
; ((a x y) y)
