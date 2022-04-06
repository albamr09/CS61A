;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2
;; SICP 3.5.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Evaluating this expression produces an error:

(stream-cdr (stream-cdr (cons-stream 1 '(2 3))))

Explain why.

|#

#| 

When we create the stream we create:

(cons
  1
  b
)

Where b=(delay '(2 3)), so b equals:

(lambda
  ()
  '(2 3)
)

So in the first (stream-cdr) we are doing:

(force
  (cdr 
    (cons
      1
      b
    )
  )
)

Which translates into:

(force
  b
)

Subtituting b:

(force
  (lambda
    ()
    '(2 3)
  )
)

Because force is implemented as follows:

(define (froce proc)
  (proc)
)

When we apply force, what we do is evaluate the lambda expression:

(lambda
  ()
  '(2 3)
)

And we obtain '(2 3). However if we apply (stream-cdr) again over '(2 3):

(force
  (cdr 
    '(2 3)
  )
)

Which equals:

(force
  3
)

So now what we are doing is equivalent to doing in the interpreter:

> (3)

Which returns an error, because 3 is not a procedure

|#
