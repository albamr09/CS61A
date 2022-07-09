(load "./streams.scm")

(define s1 (cons-stream 0 (cons-stream 1 (cons-stream 2 the-empty-stream))))

(print (stream-car s1))
; 0
(print (stream-cdr s1))
; [promise 1 2]

(print (stream-ref s1 0))
; 0
(print (stream-ref s1 1))
; 1

; Multiply the elements of the stream by 2
(stream-for-each (lambda (x) (* 2 x)) s1)

; Multiply the elements of the stream by 10
(define s2 (stream-map (lambda (x) (* 10 x)) s1))
(display-stream s2)
; (0 10 20)

(display-stream s1)
; (0 1 2)

