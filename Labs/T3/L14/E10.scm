; Exercise 3: Mapreduce with Streams

; Our current mapreduce works with lists. Mapreduce in real life works with really large datasets: so large that a 
; list won't be able to contain them. One way to solve this is to have a mapreduce that works on streams instead of list. 

; What changed? Our map, sort-into-buckets, and filter works with streams now. What do you as a user need to provide? The mapper 
; and reducer, just like what you did previously. How do the mappers and reducers changed? they don't. The behavior of mapper and reducer 
; doesn't change. You can load the file and try 

; (mapreduce mapper reducer 0 all-songs) 

; where the mapper and reducer are ones you've defined in the lessons. They would work the same way. The only difference is 
; that if all-songs is large, our previous version will crash and run whereas our stream version would still be able to process it

; Load mapreduce stream version
(load "../../../lib/streammapreduce.scm")

(define result
  (mapreduce mapper reducer 0 all-songs)
)
(print result)

; ( 
;   (i . 4) 
;   [promise]
; )

(print (stream-ref result 0))
; (i . 4) 
(print (stream-ref result 1))
; (fell . 1) 
(print (stream-ref result 2))
; (saw . 1) 
