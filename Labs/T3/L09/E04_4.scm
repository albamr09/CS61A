;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; E04_4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Generalizing one- and two-dimensional tables, show how to implement a table in which values are
; stored under an arbitrary number of keys and different values may be stored under different numbers 
; of keys. The lookup and insert! procedures should take as input a list of keys used to access the table.

;SICP 3.25 Write lookup and insert!

; Load 1D table TAD
(load "../../../BProblems/T3/P03_03/table_1D.scm")

(define (lookup keys table)
	(cond
		; If there are no more keys in the table,
		; then "table" is the record that was found
		; from the keys given
		((null? keys) table)
		(else
			; If there are any keys in the list
			(let
				; Search the subtable stored in the given key
				((subtable (assoc (car keys) (cdr table))))
				; If an object is returned
				(if subtable
					; Keep on searching on the subtable with
					; the rest of the keys
					; subtable = (k_n *table* (...)), where (...) are the values stored
					; in the subtable and k_n is the key used to search this subtable
					; which is no longer necessary, that is why we do (cdr subtable)
					(lookup (cdr keys) (cdr subtable))  
					; Else return false, no record was found
					#f
				)
			)
		)
	)
)

(define (insert! keys value table)
	(cond
		; If the list of keys is null, there are no more keys to search
		((null? keys) 
			; Override the value of the record/table
			(set-cdr! table value)
		)
		(else
			(let
				((subtable (assoc (car keys) (cdr table))))
				; If there is a subtable, keep on searching with the 
				; rest of the keys
				(if subtable
					(insert! (cdr keys) value (cdr subtable))  
					; If there is not a subtable for the given key
					; If there is only one key left, we create a 
					; record not a subtable
					(if (= (count keys) 1) 
						; Update the pointer to the last record in the table
						(set-cdr! 
						  table
							; Create the record
						  (cons 
						    (cons (car keys) value)
						    (cdr table)
						  )
						)
						; Else, if the list of keys has more than one key
						(let
							; Create subtable, with no records
							((subtable
								(list 
								  (car keys)
									(make-table)
								)
							))
							; Update the pointer to the last record in the table, to
							; point to the subtable we created
							(set-cdr! 
							  table
							  (cons 
									subtable
							    (cdr table)
							  )
	 						)
							; Keep inserting in the subtable
							; We know subtable = (k_n *table* (...)), but to insert in a table
							; we must avoid having the key k_n in the table, so we do (cdr subtable)
							(insert! (cdr keys) value (cdr subtable))
						)
					)
				)
			)
		)
	)
)

; (trace insert!)


;;;;;;;;;;;;;;;;
; TEST 1D TABLE
;;;;;;;;;;;;;;;;

(define table (make-table))
(insert! (list 'a) 1 table)
(insert! (list 'c) 1 table)
(insert! (list 'f) 1 table)
(print table)
; (*table* ('a 1) ('c 1) ('f 1))
(print (lookup (list 'a) table))
; (a 1)
(print (lookup (list 'b) table))
; #f

;;;;;;;;;;;;;;;;
; TEST 2D TABLE
;;;;;;;;;;;;;;;;
(define table2 (make-table))

(insert! (list 's1 'a) 1 table2)
(insert! (list 's1 'c) 2 table2)
(insert! (list 's1 'f) 3 table2)
(insert! (list 's2 'b) 4 table2)
(insert! (list 's2 'd) 5 table2)
(insert! (list 's2 'e) 6 table2)
(print table2)
; (*table* (s1 *table* (a 1) (c 2) (f 3)) (s2 *table* (b 4) (d 5) (e 6)))

(print (lookup (list 's1 'a) table2))
; (a 1)
(print (lookup (list 's1 'b) table2))
; #f
(print (lookup (list 's2 'd) table2))
; (d 5)
(print (lookup (list 's2 'f) table2))
; #f
