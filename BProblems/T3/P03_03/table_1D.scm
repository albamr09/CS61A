;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ONE-DIMENSIONAL TABLE TAD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A one dimensional table is a list of pairs:
;; - The first element of the list is a dummy cell, to have a 
;;   pointer where to insert new elements
;; - Every other element is a pair of the form (en, .), where en points 
;;   to a pair (kn, vn)
;;    - kn: key of the cell
;;    - vn: value of the cell
;;   and . points to the next element of the table

;; Example: 
;; (table, .) -> (e1, .) -> (e2, /)
;;                |           |
;;             (k1, v1)    (k2, v2)

;;;;;;;;;;;;;;
;; CONSTRUCTOR
;;;;;;;;;;;;;;


(define (make-table)
  (list '*table*)
)

;;;;;;;;;;;;;;
;; SELECTORS
;;;;;;;;;;;;;;

(define (lookup key table)
  ; Let assoc search for the record, given a key
  (let 
    ((record 
      ; Avoid the first cell of table (dummy entry)
      (assoc key (cdr table))
    ))
    ; If the record returned is not the #f value
    (if record
      ; Return the value of the cell
      (cdr record)
      ; Else return false
      false
    )
  )
)


(define (assoc key records)
  (cond 
    ; If the table is empty
    ((null? records) false)
    ; If the key equals the key of the current cell, return the cell
    ((equal? key (caar records)) (car records))
    ; Keep on searching on the rest of the table
    (else (assoc key (cdr records)))
  )
)

;;;;;;;;;;;;;;
;; MUTATORS
;;;;;;;;;;;;;;

;;; Given a table
;; (table, .) -> (e1, .) -> (e2, /)
;; Where (table, .) is the dummy cell and (en, .) is a generic cell
;; Let kn be the key of the cell en and vn be the value of the cell en
;; - e1 points to a pair: (k1, v1)
;; - e2 points to a pair: (k2, v2)

;; To insert a key k3, with value v3
;; 1. Search if k3 is a key in the table
;; 2. Because it is not, create a new pair (k3, v3)
;; 3. Create a new cell (e3, .), where e3 points to (k3, v3), and . points to (e1, .)
;; 4. Update the rear-pointer of the dummy cell, so . points to (e3, .)
;;    (table, .) - > (e3, .)

;; To insert a key k2, with value v4
;; 1. Search if k2 is a key in the table
;; 2. Because it is, update the value of the record, so the record is now (k2, v4)

(define (insert! key value table)
  (let 
    ; Search for a record with the given key in the table
    ((record (assoc key (cdr table))))
    ; If it exists
    (if record
      ; Update the value of the cell (cdr of list in the cell)
      (set-cdr! record value)
      ; If it does not exist
      (set-cdr! 
        ; Add cell next to dummy cell in the table, that 
        ; is to say: the cdr of the dummy cell (table), points 
        ; to this new pair, and the cdr of the new pair
        ; points to what the pair table was pointing to before
        table
        ; Create new pair
        (cons 
          ; Create new cell: key - value
          (cons key value)
          ; Point to the first element of the table, now second
          (cdr table)
        )
      )
    )
  )
  'ok
)
