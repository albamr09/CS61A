;;Lesson 10

;;3.53
#|Describe the elements of the stream


|#

;;3.54
(define (mul-streams s1 s2)
  (error "Not yet implemented"))

;;3.55
(define (partiam-sums stream)
  (error "Not yet implemente"))

;;3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

#| Uncomment this after you defined it 
(define S (cons-stream 1 (merge <??> <??>)))
|#

;;3.64
(define (stream-limit stream tolerance)
  (error "Not yet implemented"))

;;3.66
#| Explain


|#

;;3.68
#| Explain



|#


;;Exercise 6
(define (fract-stream lst)
  (error "Not yet implemented"))


;;Optional Challenge Problem.
;;Note: this is NOT extra credit. Only do it if you want more practice.
#|
  Your explanation Here
|#
