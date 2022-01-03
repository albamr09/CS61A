#lang racket
(require berkeley)

; Suppose we change the definition of the tree constructor so that it uses
; instead of cons:

(define (make-node datum children)
  (list datum children)
)

; How do we have to change the selectors so that everything still works?

(define (datum node)
  ; First element of the list
  (list-ref node 0)
)
(define (children node)
  ; Second element of the list
  (list-ref node 1)
)


; TEST

(define world-tree
  (make-node 'world
    (list 
      (make-node 'italy
        '(venezia riomaggiore firenze roma)
      )
      (make-node '(united states)
        (list 
          (make-node 'california
            '(berkeley (san francisco) gilroy)
          )
          (make-node 'massachusetts
            '(cambridge amherst sudbury)
          )
          (make-node 'ohio '(kent))
        )
      )
      (make-node 'zimbabwe '(harare hwange))
      (make-node 'china
        '(beijing shanghai guangzhou suzhou)
      )
      (make-node
        '(great britain)
        (list
          (make-node 'england '(liverpool))
          (make-node 'scotland
            '(edinburgh glasgow (gretna green))
          )
          (make-node 'wales '(abergavenny))
        )
      )
      (make-node
        'australia
        (list
          (make-node 'victoria '(melbourne))
          (make-node '(new south wales) '(sydney))
          (make-node 'queensland
            '(cairns (port douglas))
          )
        )
      )
      (make-node 'honduras '(tegucigalpa))
    )
  )
)

; (car (children world-tree))
; (ITALY (VENEZIA) (RIOMAGGIORE) (FIRENZE) (ROMA))
