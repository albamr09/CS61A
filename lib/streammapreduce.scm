#|
ADT for making a key value pair
|#
(define make-kv-pair cons)
(define kv-key car)
(define kv-value cdr)


#|
A non-parallel implementation for mapreduce.
Implemented as streams as oppose to lists in order to support large inputs.
Note that the actual mapreduce will exploit parallelism wherever possible. (e.g. parallelizing the mapper and reducer)
|#

(define (mapreduce mapper reducer base-case data)
	(stream-groupreduce reducer base-case (stream-sort-into-buckets (stream-flatten (stream-map mapper data))))
)


#|
Converts a stream of lists of key-value pairs to a single stream of key-value pairs

Args:
    Streams of Lists of key-value pairs

Returns:
Streams of key-value pairs
|#
(define (stream-flatten strm)
  (if (equal? strm the-empty-stream)
      the-empty-stream
      (let ((list-of-kv (stream-car strm)))
	(interleave (list->stream list-of-kv) ;interleave instead of stream-append for efficiency
		    (stream-flatten (stream-cdr strm)))
	)
      )	       
  )

#|
Converts a list of key-value pairs to a stream of key-value pairs

Args:
	list of key value pairs

Returns:
	Streams of key value pairs
|#
(define (list->stream list-of-kv)
  (if (empty? list-of-kv)
      the-empty-stream
      (cons-stream (car list-of-kv)
		   (list->stream (cdr list-of-kv)))))

#| 
Takes a stream of key-value pairs, returns a stream of buckets (a stream of key-value pairs with the same keys).

Args:
    Streams of key-value pairs

Returns:
    Streams of streams of key-value pairs (inner stream has kv-pairs of the same key)
|#

(define (stream-sort-into-buckets strm)
  (if (equal? strm the-empty-stream)
    the-empty-stream
    (let* 
      ;car contains stream with same keys. cdr contains other elements of the streams
      (
        (key (kv-key (stream-car strm)))
	      (filtered (filter-by-key key strm))
      ) 	
      (cons-stream 
        (car filtered)
		    (stream-sort-into-buckets (cdr filtered))
      )
    )
  )
)

#|
A helper function to incraese efficiency in sorting into buckets. Returns a pair where the car is a bucket (stream of key-value pair with the same keys), and the cdr is a stream of everything else. This is done in a single sweep across the stream

Args:
    key: key (typically words or sentence or string)
    strm: A stream of key-value pairs

Returns:
    A pair with car: stream of elements with the same key. 
                cdr: stream of the rest of the elements
|#
(define (filter-by-key key strm)
  (define  output (cons the-empty-stream the-empty-stream)) ; car contains stream where pred is #t, cdr otherwise
  (define (loop strm)
    (if (equal? strm the-empty-stream)
	output
	(let ((kv (stream-car strm)))
	  (if (equal? key (kv-key kv))
	      (let ((prev (car output))) ;The let on (car output) is neccessary to force the evaluation. Otherwise, the stream will append to itself recursively
		(set-car! output (cons-stream kv
					      prev)))
	      (let ((prev (cdr output)))
		(set-cdr! output (cons-stream kv
					      prev))))
	  (loop (stream-cdr strm))
	  )
	)
    )
  (loop strm)
  )




#|
(2) groupreduce:
Takes a reducer, base case and buckets. Buckets is a stream of streams where the inner stream has key-value pairs with the same keys

Arguments:
    reducer:   Function of 2 arguments
    base-case: Should have the same type as the output of reducer
    buckets:   Stream of streams

Returns:
    Stream of key-value pairs

|#


(define (stream-groupreduce reducer base-case buckets)
  (stream-map (lambda (bucket) (stream-reduce-bucket reducer base-case bucket))
	      buckets)
)

#|
Arguments:
    reducer:   Function of 2 arguments
    base-case: Should have the same type as the output of reducer
    bucket:    Stream of key-value pairs

Returns:
    A key-value pair

|#
(define (stream-reduce-bucket reducer base-case bucket)
  (make-kv-pair (kv-key (car bucket))
		(stream-accumulate reducer base-case (stream-map kv-value bucket))))
