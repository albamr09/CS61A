;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; E04_3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SICP 3.21 Explain what Eva Lu Ator is talking about, and what happened with
;Ben's examples.  Then define print-queue.

; Import queue TAD
(load "../../../BProblems/T3/P03_02.scm")

(define q1 (make-queue))
(print (insert-queue! q1 'a))
; ((a) a)
(print (insert-queue! q1 'b))
; ((a b) b)
(print (delete-queue! q1))
; ((b) b)
(print (delete-queue! q1))
; (() b)


#| What happened with Ben's examples?

As was explained in Bproblems/T3/P03_02.scm a queue is described by a front pointer and a 
rear pointer. 

Example: 
Given a queue q of the form (a, b, c)
q -> (. , .)--------------┑
      | #front-pointer    | #rear-pointer
      |                   |
     (a, .) -> (b, .) -> (c, .)

So when you call (print p), you print (car p) = (a b c) and (cdr p) = (c)
Both of these are concatenated in the output: ((a b c) c)

|#
; Implement the definition of print-queue
;Make sure you use display to print the queue.

(define (print-queue q)
	; Print only the front pointer: as you can see from the example
	; it contains the whole list
	(print (car q))
)

(define q2 (make-queue))
(print-queue (insert-queue! q2 'a))
; (a)
(print-queue (insert-queue! q2 'b))
; (a b)
(print-queue (delete-queue! q2))
; (b)
(print-queue (delete-queue! q2))
; ()
