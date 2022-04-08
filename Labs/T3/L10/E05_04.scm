;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; E05_04
;; SICP 3.53
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Without running the program, describe the
; elements of the stream defined by

; Given the procedure add streams
(define (add-streams s1 s2) (stream-map + s1 s2))

(define s 
  (cons-stream 1 (add-streams s s))
)

#|Describe the elements of the stream

>>> 1

s = {
  1
  (delay 
    add-streams ...
  )
}

>>> 2: call (cdr of s) = (add-streams s s)

Therefore we call add-stream
(add-streams
  s
  s
) 
= 
(stream-map
  +
  s
  s
)
=

Because s and s are not emtpy (refer to stream-map in BProblems/T3/P05_01/streams.scm or Labs/T3/L10/E05_01.scm for the version with a list of streams as inputs):
- proc: +
- argstreams: '(s s)

=
(cons-stream
  (+ (stream-car s) (stream-car s))
  Keep doing the map on the rest of objects when asked
  (delay
    (stream-map
      (cons
        (stream-cdr s)
        (stream-cdr s)
      )
    )
  )
)

So:

s = {
  1,
  (+ 1 1) = 2
}

>>> 3: call (cdr of (cdr of s)) = (cdr (add-streams s s))

With t = (add-streams s s) = {2 , (delay ...)}

If we call (cdr (add-streams s s)) we have to evaluate the delayed object specified above:

(delay
  (stream-map
    (cons
      (stream-cdr s)
      (stream-cdr s)
    )
  )
)

So if s = {1, 2, (delay ...)}, then s' = (stream-cdr s) = {2, (delay ...)}, we call:

(stream-map
  '(
      {2, (delay ...)}  
      {2, (delay ...)}
  )
)

And like we did above when calling stream-map:

(cons-stream
  (+ (stream-car s') (stream-car s'))
  Keep doing the map on the rest of objects when asked
  (delay
    (stream-map
      (cons
        (stream-cdr s')
        (stream-cdr s')
      )
    )
  )
)

Because (stream-car s') = 2:

s = {
  1,
  2,
  (+ 2 2)=4
}

|#

(print (stream-ref s 0))
; 1
(print (stream-ref s 1))
; 2
(print (stream-ref s 2))
; 4
