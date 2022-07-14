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
	(stream-groupreduce reducer base-case (stream-sort-into-buckets (stream-flatten (stream-map mapper data)))))


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
      (let* ((key (kv-key (stream-car strm)))
	     (filtered (filter-by-key key strm))) ;car contains stream with same keys. cdr contains other elements of the streams
	(cons-stream (car filtered)
		     (stream-sort-into-buckets (cdr filtered))))))

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




#|
 Example input, mapper and reducer for our mapreduce. Duplicated from corresponding EdX courseware. Converted lists to streams
A list of key-value pairs where the key is the song title and the value is a line from the song
|#

(define song1 (cons-stream (make-kv-pair '(please please me)
					 '(i saw her standing there))
			   (cons-stream (make-kv-pair '(please please me)
						      '(misery))
					(cons-stream (make-kv-pair '(please please me)
								   '(please please me))
						     the-empty-stream))))

(define song2 (cons-stream (make-kv-pair '(with the beatles)
					 '(it wont be long))
			   (cons-stream (make-kv-pair '(with the beatles)
						      '(all i have got to do))
					(cons-stream (make-kv-pair '(with the beatles)
								   '(all my loving))
						     the-empty-stream))))

(define song3 (cons-stream (make-kv-pair '(a hard days night)
					 '(a hard days night))
			   (cons-stream (make-kv-pair '(a hard days night)
						      '(i should have known better))
					(cons-stream (make-kv-pair '(a hard days night)
								   '(if i fell))
						     the-empty-stream))))

(define all-songs (stream-append song1 (stream-append song2 song3)))
			   

(define (mapper input-kv-pair)
  (map (lambda (wd) (make-kv-pair wd 1)) (kv-value input-kv-pair)))

(define (reducer num other-num)
	(+ num other-num))


#|
The following lines define more examples of mapreduce with larger data involving pokemons (approx 774 in size)
|#

(define (pokemon) (load "~cs61as/lib/mapreduce/pokemon_data")) ;loads data
(define (longest) ;returns the pokemon with longest name
  (define (mapper input-kv-pair)
    (list (make-kv-pair 'longest
			(cadr (kv-value input-kv-pair)))))
  (define (reducer name othername)
    (if (>= (count name) (count othername))
	name
	othername))
  (mapreduce mapper reducer "" data))


(define (shortest) ; returns the pokemon with shortest name
  (define (mapper input-kv-pair)
    (list (make-kv-pair 'shortest
			(cadr (kv-value input-kv-pair)))))
  (define (reducer name othername)
    (if (<= (count name) (count othername))
	name
	othername))
  (mapreduce mapper reducer "pikachu" data)) ;arbitary base case


(define (types) ; returns a stream where key is pokemon type and value is number of pokemon with that type
  (define (mapper input-kv-pair)
    (let ((types (cdddr (kv-value input-kv-pair))))
      (map (lambda (type) (make-kv-pair type 1))
	   types)))

  (define reducer reducer)
  (mapreduce mapper reducer 0 data))

#|
The following lines show more examples with different inputs
|#
(define (pokemonmoves) (load "~cs61as/lib/mapreduce/pokemonmoves_data")) ;loads data

(define (movetypes) ;returns a stream with keys move types, and values number of moves of those types
  (define (mapper input-kv-pair)
    (list (make-kv-pair (car (kv-value input-kv-pair))
			1)))
  (define reducer +)
  (mapreduce mapper reducer 0 data))


(define (movepower) ; returns a stream with move of highest power in respective category
  (define (mapper input-kv-pair)
    (list (make-kv-pair (cadr (kv-value input-kv-pair))
			(cons (kv-key input-kv-pair)
			      (caddr (kv-value input-kv-pair)))
			)))
  (define (reducer namepow1 namepow2)
    (let ((pow1 (cdr namepow1))
	  (pow2 (cdr namepow2)))
      (cond ((not (number? pow1)) namepow2)
	    ((not (number? pow2)) namepow1)
	    ((> pow1 pow2) namepow1)
	    (else namepow2))
    ))
  (mapreduce mapper reducer (cons ""  0) data))

(define (movepp) ; returns a stream with highest pp in respective category
  (define (mapper input-kv-pair)
    (list (make-kv-pair (cadr (kv-value input-kv-pair))
			(cons (kv-key input-kv-pair)
			      (caddr (cddr (kv-value input-kv-pair))))
			)))
  (define (reducer namepow1 namepow2)
    (let ((pow1 (cdr namepow1))
	  (pow2 (cdr namepow2)))
      (cond ((not (number? pow1)) namepow2)
	    ((not (number? pow2)) namepow1)
	    ((> pow1 pow2) namepow1)
	    (else namepow2))
    ))
  (mapreduce mapper reducer (cons ""  0) data))


(define (magikarp) ;generic call
  (mapreduce mapper reducer 0 data))

(define (mapreduce mapper reducer base-case data)
	(stream-groupreduce reducer base-case (stream-sort-into-buckets (stream-map mapper data))))



(define (stream-sort-into-buckets strm)
  (if (equal? strm the-empty-stream)
      the-empty-stream
      (let* ((key (kv-key (stream-car (stream-car strm))))
	     (sorted (magikarp-filter (lambda (kv) (equal? key (kv-key kv)))
				      strm)))
	(cons-stream (car sorted)
		     (stream-sort-into-buckets (stream-map (lambda (x) (list x))
							   (cdr sorted)))))))
	     
(define (magikarp-filter pred strm)
  (define  output (cons the-empty-stream the-empty-stream)) ; car contains stream where pred is #t, cdr otherwise
  (define (splash-filter lst)
    (if (not (empty? lst))
	(begin (if (pred (car lst))
		   (let ((prev (car output)))
		     (set-car! output (cons-stream (car lst)
						   prev)))
		   (let ((prev (cdr output)))
		     (set-cdr! output (cons-stream (car lst)
						   prev))))
	       (splash-filter (cdr lst)))))
	
  (define (loop strm)
    (if (equal? strm the-empty-stream)
	output
	(begin (splash-filter (stream-car strm))
	       (loop (stream-cdr strm)))
	
    )  
  )
  (loop strm)
)


