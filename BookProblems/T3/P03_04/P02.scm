; Load mapreduce definitions
(load "../../../lib/mapreduce.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAPREDUCE
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; Example word count

;;;;;;;;;;;;;;;;;;
; Input data
;;;;;;;;;;;;;;;;;;

(define song1   
  '(
    ((please please me) i saw her standing there)
    ((please please me) misery)
    ((please please me) please please me)
  )
)

(define song2   
  '( 
    ((with the beatles) it wont be long)
    ((with the beatles) all i have got to do)
    ((with the beatles) all my loving)
  )
)

(define song3   
  '( 
    ((a hard days night) a hard days night)
    ((a hard days night) i should have known better)
    ((a hard days night) if i fell)
  )
)

(define all-songs (append song1 song2 song3))
; ( 
;   ((please please me) i saw her standing there)
;   ((please please me) misery)
;   ((please please me) please please me)
;   ((with the beatles) it wont be long)
;   ((with the beatles) all i have got to do)
;   ((with the beatles) all my loving)
;   ((a hard days night) a hard days night)
;   ((a hard days night) i should have known better)
;   ((a hard days night) if i fell) 
; )


;;;;;;;;;;;;;;;;;;;;;;;;;;
; Create dictionary entry
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-kv-pair cons)
(define kv-key car)
(define kv-value cdr)

(print (kv-key '((please please me) i saw her standing there)))
; (please please me)
(print (kv-value '((please please me) i saw her standing there)))
; (i saw her standing there)

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Mapper for word count
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mapper input-kv-pair)
  (map 
    (lambda 
      ; Each word
      (wd) 
      ; Initialize frequency to one
      (make-kv-pair wd 1)
    ) 
    ; Input list of values
    (kv-value input-kv-pair)
  )
)

(print
  (mapper '((please please me) i saw her standing there))
)
; ((i . 1) (saw . 1) (her . 1) (standing . 1) (there . 1))

(print
  (mapper '((please please me) please please me))
)
; ((please . 1) (please . 1) (me . 1))

; Call mapper on all songs
(print 
  (map 
    ; Select value, not key and create list of pairs: (word, frequency=1)
    mapper 
    ; For each song
    all-songs
  )
)

;;;;; Note how the keys and values are organized here. The result is a list of buckets, 
;;;;; where a bucket is a list of kv-pair with the same keys.

(print (sort-into-buckets (map mapper all-songs)))

; '( 
;   ((i . 1) (i . 1) (i . 1) (i . 1))
;   ((saw . 1))
;   ((her . 1))
;   ((standing . 1))
;   . . .
;   ((all . 1) (all . 1))
;   ((have . 1) (have . 1))
;   . . . 
;   ((if . 1))  
;   ((fell . 1))
; )

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reducer for word count
;;;;;;;;;;;;;;;;;;;;;;;;;;

; Reducing one bucket
; (accumulate reducer 0 (map kv-value '((i . 1) (i . 1) (i . 1) (i . 1) (i . 1))))

; It simplifies to
; (accumulate reducer 0 '(1 1 1 1 1))


(define (reduce-bucket reducer base-value bucket)
    (make-kv-pair   
      ; Create resulting dictionary entry: {i: frequency}
      ; Obtain key of first element (i 1)
      (kv-key (car bucket))
      (accumulate 
        ; Function that accumulates
        reducer 
        ; Initial value
        base-value 
        ; Collect values from entries ((i 1) (i 1) ...) -> (1 1 ...)
        (map kv-value bucket)
      )
    )
)

;; The procedure reduce-bucket above reduces one bucket. Our result from the previous step, 
;; (sort-into-buckets (map mapper data)) is a list of buckets. To reduce a list of buckets, 
;; we can use map again.

;; Given an input 
; '( 
;   ((i . 1) (i . 1) (i . 1) (i . 1))
;   ((saw . 1))
;   ((her . 1))
;   ((standing . 1))
;   . . .
;   ((all . 1) (all . 1))
;   ((have . 1) (have . 1))
;   . . . 
;   ((if . 1))  
;   ((fell . 1))
; )
;; We obtain the output 
; '(
;   (i . 5)
;   (saw . 5)
;   ...
; )

(define (groupreduce reducer base-case buckets) 
  (map 
    ; For each bucket
    (lambda 
      (bucket) 
      ; Apply reduce-bucket to obtain: (bucket-key accumulated-frequency)
      (reduce-bucket 
        reducer 
        base-case 
        bucket
      )
    ) 
    ; Given the list of buckets
    buckets
  )
)

; Example of result

(print (groupreduce reducer 0 (sort-into-buckets (map mapper all-songs))))
; ( 
;   (i . 4) 
;   (saw . 1) 
;   (her . 1)
;   . . .
;   (misery . 1) 
;   (please . 2) 
;   (me . 1)
;   . . .
;   (all . 2) 
;   (have . 2)
; )
