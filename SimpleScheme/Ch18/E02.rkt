#lang racket
(require berkeley)

; Suppose we change the definition of the tree constructor so that it uses
; instead of cons:

(define (make-tree-node datum children)
  (cond
    ((or (null? datum) (empty? datum)) null)
    ((or (null? children) (empty? children)) (list datum))
    (else
      (append (list datum) children)
    )
  )
)

; How do we have to change the selectors so that everything still works?

(define (datum-tree node)
  ; First element of the list
  (car node)
)
(define (children-tree node)
  ; Second element of the list
  (cdr node)
)

; Other operators

(define (leaf? node)
  ; Return true if the node
  ; has an empty list as children
  ; or the list is null
  (or 
    (null? (children-tree node)) 
    (empty? (children-tree node))
  )
)


; TEST

(define world-tree
  (make-tree-node 'world
    (list 
      (make-tree-node 'italy
        '(venezia riomaggiore firenze roma)
      )
      (make-tree-node '(united states)
        (list 
          (make-tree-node 'california
            '(berkeley (san francisco) gilroy)
          )
          (make-tree-node 'massachusetts
            '(cambridge amherst sudbury)
          )
          (make-tree-node 'ohio '(kent))
        )
      )
      (make-tree-node 'zimbabwe '(harare hwange))
      (make-tree-node 'china
        '(beijing shanghai guangzhou suzhou)
      )
      (make-tree-node
        '(great britain)
        (list
          (make-tree-node 'england '(liverpool))
          (make-tree-node 'scotland
            '(edinburgh glasgow (gretna green))
          )
          (make-tree-node 'wales '(abergavenny))
        )
      )
      (make-tree-node
        'australia
        (list
          (make-tree-node 'victoria '(melbourne))
          (make-tree-node '(new south wales) '(sydney))
          (make-tree-node 'queensland
            '(cairns (port douglas))
          )
        )
      )
      (make-tree-node 'honduras '(tegucigalpa))
    )
  )
)

; (car (children-tree world-tree))
; (ITALY (VENEZIA) (RIOMAGGIORE) (FIRENZE) (ROMA))

; exports
(provide make-tree-node datum-tree children-tree leaf?)
(trace leaf?)
(trace make-tree-node)
