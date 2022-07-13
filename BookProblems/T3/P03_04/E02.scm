; Load most frequent mapreduce
(load "./E01.scm")

; Our function above works, if we pass the pairs with the key as a word, and value as 
; a number. In real life, we might not have direct access to the word counts of each 
; word; we have to process that from the original document.


(define (real-most-frequent lst)
  ; Composition between word counter and maximum selector
  ; Select the word that appears the most
  (frequent-mapreduce 
    ; Counter the number of times each word appears
    (groupreduce 
      reducer 
      0 
      (sort-into-buckets (map mapper lst))
    )
  )
)

; (print (real-most-frequent all-songs))
; (i . 4)
