;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; E03
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Consider the following:

(define (enumerate-interval low high) 
  (if (> low high) 
    '() 
    (cons low (enumerate-interval (+ low 1) high)) 
  ) 
)

(define (stream-enumerate-interval low high) 
  (if (> low high) 
    the-empty-stream 
    (cons-stream low (stream-enumerate-interval (+ low 1) high)) 
  ) 
)

; What's the difference between the following two expressions?

(define s (delay (enumerate-interval 1 3)))
; Because this is only a delayed list:
; s = (delay (1 2 3))
; So when we access s, we evaluate the whole list (1 2 3)

; (print (force s))

(define t (stream-enumerate-interval 1 3))
; Because this is a stream, it has the form:
; s = (1 (delay (2 (delay 3))))
; Now, if we evaluate s, we get first 1
; If we evaluate again we get (2 (delay 3))
; And finally if we evaluate again we get 3

; (print (car t))
; ; 1
; (print (force (cdr t)))
; ; (2 (promise 3))
; (print 
;   ; 2nd evaluation
;   (force (cdr 
;     ; 1st evaluation
;     (force (cdr t))
;   ))
; )
; ; 3
; 
; (print 
;   ; 3rd evaluation
;   (force (cdr 
;     ; 2nd evaluation
;     (force (cdr 
;       ; 1st evaluation
;       (force (cdr t))
;     ))
;   ))
; )
; ; '()
