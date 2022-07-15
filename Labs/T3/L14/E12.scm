;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; MAPREDUCE EXAMPLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(define (pokemon) (load "./pokemon_data.scm")) ;loads data
(define (longest) ;returns the pokemon with longest name
  (define (mapper input-kv-pair)
    (list 
      (make-kv-pair 'longest
			  (cadr (kv-value input-kv-pair))
      )
    )
  )
  (define (reducer name othername)
    (if (>= (count name) (count othername))
	    name
	    othername
    )
  )
  (mapreduce mapper reducer "" data)
)


(define (shortest) ; returns the pokemon with shortest name
  (define (mapper input-kv-pair)
    (list 
      (make-kv-pair 'shortest
			  (cadr (kv-value input-kv-pair))
      )
    )
  )
  (define (reducer name othername)
    (if (<= (count name) (count othername))
	    name
	    othername
    )
  )
  ;arbitary base case
  (mapreduce mapper reducer "pikachu" data)
) 

; returns a stream where key is pokemon type and value is number of pokemon with that type
(define (types) 
  (define (mapper input-kv-pair)
    (let 
      ((types (cdddr (kv-value input-kv-pair))))
      (map 
        (lambda 
          (type) 
          (make-kv-pair type 1)
        )
	      types
      )
    )
  )
  (define reducer reducer)
	(stream-groupreduce reducer 0 (stream-sort-into-buckets (stream-flatten (stream-map mapper data))))
  ; (mapreduce mapper reducer 0 data)
)

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


