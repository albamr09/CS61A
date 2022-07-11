; Let's write another mapreduce function (We're not chaining yet). This time, our input has a key of 'words' and the value are numbers, 
; representing how many times they appeared in a document. We want our output to be a list of a single key-value pair where just like 
; the input, our key is a word and our value is a number such that it is the highest number encountered.

; Note: our solution isn't ideal, and it's a little contrived. It doesn't take advantage of the parallelism that mapreduce offers.

(define x (list (make-kv-pair her 1) (make-kv-pair i 4) (make-kv-pair saw 1))

; (most-frequent x) ; i appears the most
; ((i . 4))
