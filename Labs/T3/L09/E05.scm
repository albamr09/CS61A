;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; E05
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Write vector-append, which takes two vectors as arguments and returns a new vector 
; containing the elements of both arguments, analogous to append for lists.

;Exercise 5. Write vector-append.
(define (vector-append v1 v2)
	(let
		(
			(len1 (vector-length v1))
			(len2 (vector-length v2))
		)

		; Define loop procedure to set n elements in new-vector object
		; from the index i with an offset (with regards to the new-vector index)
		; the next procedure is called when we are finished setting the n elements
		(define (loop new-vector v i n offset next)
			; If the index is bigger than n, we have finished appending
			(if (>= i n)
				; Call the next function
				(next new-vector)
				; Else keep appending
				(begin
					; Update the element in new-vector[i + offset] to be equal to v[i]
					(vector-set! new-vector (+ i offset) (vector-ref v i))
					; Keep on appeding, after updating the index i to i + 1
					(loop new-vector v (+ i 1) n offset next)
				)
			)
		)

		(loop 
			(make-vector (+ len1 len2))
			v1 
			0																				; i
			len1																		; n 
			0																				; offset 
			(lambda																	; next: append v2
				(new-vector) 
				(loop 
					new-vector 
					v2 
					0																		; i 
					len2																; n 
					len1																; offset 
					(lambda (new-vector) new-vector)		; next: return vector
				)
			)
		)
	)
)

(print (vector-append (vector 1 2 3) (vector 1 333 3)))
; (1 2 3 1 333 3)
