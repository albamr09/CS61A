; Louis Reasoner tries to evaluate the expression (magnitude z). To his surprise, instead of the answer 5 
; he gets an error message from apply-generic, saying there is no method for the 
; operation magnitude on the types (complex). He shows this interaction to Alyssa 
; P. Hacker, who says
;
; ``The problem is that the complex-number selectors were never defined for complex numbers, just for polar and 
; rectangular numbers. All you have to do to make this work is add the following to the complex package:''

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

; Describe in detail why this works. As an example, trace through all the procedures 
; called in evaluating the expression (magnitude z) where z is the object shown in figure 2.24. 
; In particular, how many times is apply-generic invoked? What procedure is dispatched to in each case?

; Now when we try to get the magnitude we do:
;
; (magnitude z)
;
; (1) And the apply-generic does (get 'magnitude '(complex)), this returns the above specified procedure
; "magnitude".

; (2) Now this procedure is invoked:
; (apply magnitude (contents z))
; Where (contents z), is the z data object stripped of the complex tag.

; (3) As we have seen, the magnitude procedure is, in the internal procedures of the complex package 
; another generic procedure, so it calls apply-generic again. Which yields:
; (get 'magnitude '(rectangular)). 
; Notice that now the tag is not complex, that is because we are working with the contents of z, that is
; a rectangular representation of a complex number (i.e. (complex (rectangular 1 3)) becomes (rectangular 1 3)).
