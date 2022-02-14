#lang racket

(require (rename-in graphics/turtles
           (split turtle-split)))

(provide (all-defined-out))

;; Code for integrating Racket's turtle graphics library

(define turtle-x -1)
(define turtle-y -1)
(define turtle-pen-down #f)

(define (cs)
  (turtles #t)
  (clear)
  (set! turtle-x (/ turtle-window-size 2))
  (set! turtle-y (/ turtle-window-size 2))
  (set! turtle-pen-down #f))

(define (penup)
  (set! turtle-pen-down #f))

(define (pendown)
  (set! turtle-pen-down #t))

(define (setxy x y)
  (let ((relative-x (- x turtle-x))
        (relative-y (* -1 (- y turtle-y))))
  (begin (if turtle-pen-down
             (draw-offset relative-x relative-y)
             (move-offset relative-x relative-y))
         (set! turtle-x x)
         (set! turtle-y y))))

(define (draw-line v1 v2)
  (penup)
  (setxy (- (* (xcor-vect v1) turtle-window-size) (/ turtle-window-size 2))
         (- (* (ycor-vect v1) turtle-window-size) (/ turtle-window-size 2)))
  (pendown)
  (setxy (- (* (xcor-vect v2) turtle-window-size) (/ turtle-window-size 2))
         (- (* (ycor-vect v2) turtle-window-size) (/ turtle-window-size 2))))

(define (export filename)
  (save-turtle-bitmap (string->path filename) 'png))

;; Code for the picture language

(define (flipped-pairs painter)
  (let 
    (
      (painter2 
        ; Put painter and the flipped version 
        ; side by side
        (beside 
          painter 
          ; Flip painter vertically
          (flip-vert painter)
        )
      )
    )
    ; Make a copy of painter2 (pair of painters flipped)
    ; and put the copy pair below the original pair
    (below painter2 painter2)
  )
)

(define (right-split painter n)
  (if (= n 0)
    painter
    (let 
      ; Procude a smaller copy
      (
        (smaller 
          (right-split 
            painter 
            (- n 1)
          )
        )
      )
      ; Put the orginal side by side with the copies
      (beside 
        ; Orginal
        painter 
        ; Put the copy below the same copy
        (below smaller smaller)
      )
    )
  )
)

(define (corner-split painter n)
  ; If n = 0 make no more splits
  (if (= n 0)
    painter
    (let 
      (
        ; Generate a top split
        (up (up-split painter (- n 1)))
        ; Generate a right split
        (right (right-split painter (- n 1)))
      )
      (let 
        (
          ; The top portions are two top splits (up)
          ; side by side
          (top-left (beside up up))
          ; The right portion are two right splits 
          ; one below the other
          (bottom-right (below right right))
          ; Cotinue splitting the right corner
          (corner (corner-split painter (- n 1)))
        )
        ; Put it all together
        (beside 
          ; Put the original below the top portion
          (below painter top-left)
          ; Put the right portion below the top 
          ; right corner (will recursively be splitted)
          (below bottom-right corner)
        )
      )
    )
  )
)

(define (square-limit painter n)
  (let 
    (
      ; Generate a corner split
      (quarter (corner-split painter n))
    )
    (let 
      (
        ; half = flipped corner split | corner split
        (half 
          (beside 
            ; Flip the corner split
            (flip-horiz quarter) 
            quarter
          )
        )
      )
      ; below: 
      ; flipped half 
      ; -----------
      ; half
      (below (flip-vert half) half)
    )
  )
)

; Abstraccion de flipped-pairs y square-limit: ambas colocan cuatro copiar
; de la imagen del pintor en un patrón cuadrado. La única diferencia es 
; cómo se orientan las copias.

; tl: top left transformation
; tr: top right transformation
; bl: bottom left transformation
; br: bottom right transformation

(define (square-of-four tl tr bl br)
  ; Define it as an anonymous procedure
  ; that takes the original painter as an argument
  (lambda 
    (painter)
    (let 
      (
        ; Define the top portion
        (top 
          ; Both the tops beside each other
          (beside 
            (tl painter) 
            (tr painter)
          )
        )
        ; Define the bottom portion
        (bottom 
          (beside 
            ; Both the bottoms besides each other
            (bl painter) 
            (br painter)
          )
        )
      )
      ; Put the bottom portion below the top portion
      (below bottom top)
    )
  )
)

; Now we can defien flipped-pairs and square-limit as:

; (define (flipped-pairs painter)
;   (let 
;     (
;       (combine4 
;         ; identity           | flipped vertically
;         ; ---------------------------------------
;         ; flipped vertically | identity
;         (square-of-four 
;           ; Do not transform
;           identity 
;           ; Flip it vertically
;           flip-vert 
;           ; Do not transform
;           identity 
;           ; Flip it vertically
;           flip-vert
;         )
;       )
;     )
;     ; Call the combinational procedure
;     ; with painter as the original
;     (combine4 painter)
;   )
; )

; (define (square-limit painter n)
;   (let 
;     ((combine4 
;       (square-of-four 
;         ; Flip horizontally the top left copy
;         flip-horiz 
;         ; Do not transform the top right copy
;         identity 
;         ; Rotate 180 the bottom left copy
;         rotate180 
;         ; Flip vertically the bottom right copy
;         flip-vert
;       )
;     ))
;     (combine4 (corner-split painter n))
;   )
; )

; --

(define (identity x) x)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
         (edge1-frame frame))
         (scale-vect (ycor-vect v)
         (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
  ((frame-coord-map frame) (start-segment segment))
  ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
  (painter
   (make-frame new-origin
         (sub-vect (m corner1) new-origin)
         (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
         (make-vect 0.0 1.0)
         (make-vect 1.0 1.0)
         (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
        (make-vect 0.5 0.5)
        (make-vect 1.0 0.5)
        (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
         (make-vect 1.0 0.0)
         (make-vect 1.0 1.0)
         (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
         (make-vect 0.0 0.0)
         (make-vect 0.65 0.35)
         (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
     (transform-painter painter1
            (make-vect 0.0 0.0)
            split-point
            (make-vect 0.0 1.0)))
    (paint-right
     (transform-painter painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0))))
      (lambda (frame)
  (paint-left frame)
  (paint-right frame)))))

;; End of picture language code

;; Exercise 1
; -
; Define the procedure up-split used by cornersplit. It is similar to right-split, except that it switches
; the roles of below and beside.

; --------
(define (up-split painter n)
  (if (= n 0)
    painter
    (let
      (
        ; Generate smaller painter recursively
        (smaller (up-split painter (- n 1)))
      )
      (below
        ; Put the smaller painters side by side
        (beside smaller smaller)
        ; The smaller painters are above the painter
        painter
      )
    )
  )
)

;; Exercise 2
; right-split and up-split can be expressed as instances of a general splitting operation. Define a 
; procedure split with the property that evaluating. 

(define (split major minor)
  (lambda
    (painter n)
    (let
      ; Generate smaller copy recursively
      ((smaller 
        ; Use generic splitting procedure
        ; with the major and minor operations defined
        ((split major minor)
          ; Pass the orginal and update the
          ; number of recursive steps remaining
          painter (- n 1)
        )
      ))
      ; Apply the major, minor operations in order over the 
      ; smaller copies and the orginal painter copy
      (major
        (minor smaller smaller)
        painter
      )
    )
  )
)

; Now we define right-split and up-split in terms of our general method split
; (define right-split (split beside below))
; (define up-split (split below beside))

;; Exercise 3

(define (make-vect major minor)
  (void "not yet implemented"))

(define xcor-vect
  "not yet implemented")

(define ycor-vect
  "not yet implemented")

(define (add-vect v1 v2)
  (error "not yet implemented"))

(define (sub-vect v1 v2)
  (error "not yet implemented"))

(define (scale-vect s v)
  (error "not yet implemented"))

;; Execise 4

; First definition of make-frame

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame
  "not yet implemented")

(define edge1-frame
  "not yet implemented")

(define edge2-frame
  "not yet implemented")

; Second definition of make-frame

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame-2
  "not yet implemented")

(define edge1-frame-2
  "not yet implemented")

(define edge2-frame-2
  "not yet implemented")

;; Exercise 5

(define make-segment
  "not yet implemented")

(define start-segment
  "not yet implemented")

(define end-segment
  "not yet implemented")

;; Exercise 6

(define outline-painter
  "not yet implemented")

(define x-painter
  "not yet implemented")

(define diamond-painter
  "not yet implemented")

(define wave-painter
  "not yet implemented")

;; Exercise 7

(define (flip-horiz painter)
  (error "not yet implemented"))

(define (rotate180 painter)
  (error "not yet implemented"))

(define (rotate270 painter)
  (error "not yet implemented"))

;; Exercise 8

(define (below painter1 painter2)
  (error "not yet implemented"))

(define (below-2 painter1 painter2)
  (error "not yet implemented"))

;; Exercise 9

; Modify wave-painter above (Exercise 6)

; Modify me!
(define (corner-split-2 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
      (right (right-split painter (- n 1))))
  (let ((top-left (beside up up))
        (bottom-right (below right right))
        (corner (corner-split-2 painter (- n 1))))
    (beside (below painter top-left)
      (below bottom-right corner))))))

; Modify me!
(define (square-limit-2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;; End of project
;; Don't touch anything below this

(define full-frame
  (make-frame (make-vect 0.5 0.5)
              (make-vect 1 0)
              (make-vect 0 1)))
