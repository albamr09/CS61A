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

; Abstraccion de flipped-pairs y square-limit: ambas colocan cuatro copias
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

; Now we can define flipped-pairs and square-limit as:

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

; Will be used to shift and scale images to fit the frame. The map transforms the unit square 
; into the frame by mapping the vector v = (x, y) to the vector sum 
; Origin(frame) + x · Edge1(frame) + y · Edge2(frame)
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      ; Origin(frame) 
      (origin-frame frame)
      (add-vect 
        ; x · Edge1(frame)
        (scale-vect 
          (xcor-vect v)
          (edge1-frame frame)
        )
        ; y · Edge2(frame)
        (scale-vect 
          (ycor-vect v)
          (edge2-frame frame)
        )
      )
    )
  )
)

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      ; For every segment in the list
      (lambda (segment)
        ; Draws a line between two specified points
        (draw-line
          ; Fit the starting point into frame
          ((frame-coord-map frame) (start-segment segment))
          ; Fit the ending point into frame
          ((frame-coord-map frame) (end-segment segment))
        )
      )
      segment-list
    )
  )
)

; Takes as arguments a painter and information on how to transform a frame and produces a new painter. The transformed painter,
; when called on a frame, transforms the frame and calls the original painter on the transformed 
; frame. e arguments to transform-painter are points (represented as vectors) that specify the corners 
; of the new frame: When mapped into the frame, the first point specifies the new frame’s origin and the other 
; two specify the ends of its edge vectors. Thus, arguments within the unit square specify a frame contained within 
; the original frame.

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let 
      ; Mapper for the frame
      ((m (frame-coord-map frame)))
      (let 
        ; Map the new origin (absolute new origin) to the frame
        ; to obtain the coordinates of the new origin (relarive to the frame)
        ((new-origin (m origin)))
        (painter
          (make-frame 
            new-origin
            ; Obtain the new edge coordinates relative to the frame
            (sub-vect 
              ; Transform the corner to fit the frame
              (m corner1) 
              ; Obtain the vector that connects the new origin
              ; and the mapped corner1
              new-origin
            )
            ; Obtain the new edge coordinates relative to the frame
            (sub-vect (m corner2) new-origin)
          )
        )
      )
    )
  )
)

; Transformations 

(define (flip-vert painter)
  ; From:
  ; e2
  ; |
  ; o__e1
  ; To:
  ; o--e1
  ; |
  ; e2
  (transform-painter 
    painter
    (make-vect 0.0 1.0) ; New origin: (0 0) -> (0 1)
    (make-vect 1.0 1.0) ; New end of edge 1: (1 0) -> (1 1)
    (make-vect 0.0 0.0) ; New end of edge 2: (0 1) -> (0 0) 
  )
)

; Define a painter that shrinks its image to the upperright quarter 
; of the frame it is given

(define (shrink-to-upper-right painter)
  ; From:
  ; e2
  ; |
  ; |
  ; o_____e1
  ; To:
  ;    e2
  ;    |
  ; ---o---e1
  ;    |
  (transform-painter 
    painter
    (make-vect 0.5 0.5) ; New origin
    (make-vect 1.0 0.5) ; New end of edge 1
    (make-vect 0.5 1.0) ; New end of edge 2
  )
)

; Rotate images counterclockwise by 90 degrees

(define (rotate90 painter)
  ; From:
  ; e2
  ; |
  ; o___e1
  ; To:
  ;      e1
  ;      |
  ; e2___o
  (transform-painter 
    painter
    (make-vect 1.0 0.0)
    (make-vect 1.0 1.0)
    (make-vect 0.0 0.0)
  )
)

; Squash images towards the center of the frame

(define (squash-inwards painter)
  (transform-painter 
    painter
    (make-vect 0.0 0.0)
    (make-vect 0.65 0.35)
    (make-vect 0.35 0.65)
  )
)

; Takes two painters, transforms them to paint in the left and right halves of an
; argument frame respectively, and produces a new, compound painter

(define (beside painter1 painter2)
  ; From: 
  ; e2
  ; |
  ; o---e1
  ; To:
  ; painter1|painter2
  ; e2      |e2
  ; |       ||
  ; o_____e1|o_____e1
  (let 
    ((split-point (make-vect 0.5 0.0)))
    (let 
      (
        (paint-left
          ; Get new painter from transformed frame
          (transform-painter 
            painter1
            ; Transform the frame
            (make-vect 0.0 0.0) ; Normal Origin
            split-point         ; Edge1 until split
            (make-vect 0.0 1.0) ; Normal Edge2
          )
        )
        (paint-right
          ; Get new painter from transformed frame
          (transform-painter 
            painter2
            ; Transform the frame
            split-point         ; Offset origin 0.5 on the x
            (make-vect 1.0 0.0) ; Normal edge1
            (make-vect 0.5 1.0) ; Offset edge2 0.5 on the x
          )
        )
      )
      ; Call the painters with the frame
      ; given as argument
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)
      )
    )
  )
)

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
; A two-dimensional vector v running from the origin to a point can be represented as a pair consisting
; of an x-coordinate and a y-coordinate. Implement a data abstraction for vectors by giving a constructor make-vect
; and corresponding selectors xcor-vect and ycor-vect. In terms of your selectors and constructor, implement 
; procedures add-vect, sub-vect, and scale-vect that perform the operations vector addition, vector 
; subtraction, and multiplying a vector by a scalar

(define (make-vect major minor)
  (cons major minor)
)

(define xcor-vect car)

(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect
    (+
      (xcor-vect v1)
      (xcor-vect v2)
    )
    (+
      (ycor-vect v1)
      (ycor-vect v2)
    )
  )
)

(define (sub-vect v1 v2)
  (make-vect
    (-
      (xcor-vect v1)
      (xcor-vect v2)
    )
    (-
      (ycor-vect v1)
      (ycor-vect v2)
    )
  )
)

(define (scale-vect s v)
  (make-vect
    (* s (xcor-vect v))
    (* s (ycor-vect v))
  )
)

; (define vect (make-vect 1 3))
; (define vect2 (make-vect 3 8))
; (xcor-vect vect)
; ; 1
; (ycor-vect vect)
; ; 3
; (add-vect vect vect2)
; ; (4 11)
; (sub-vect vect vect2)
; ; (-2 -5)
; (scale-vect 2 vect)
; ; (2 6)

;; Execise 4
; For each constructor supply the appropriate selectors to produce an implementation 
; for frames.

; First definition of make-frame

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)

(define edge1-frame cadr)

(define edge2-frame caddr)

; Second definition of make-frame

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2))
)

(define origin-frame-2 car)

(define edge1-frame-2 cadr)

(define edge2-frame-2 cddr)

; (define o (make-vect 1 1))
; (define e1 (make-vect 1 3))
; (define e2 (make-vect 2 5))
; (define f1 (make-frame o e1 e2))
; (define f2 (make-frame-2 o e1 e2))
; 
; (origin-frame f1)
; ; (1 1)
; (edge1-frame f1)
; ; (1 3)
; (edge2-frame f1)
; ; (2 5)
; 
; (origin-frame-2 f2)
; ; (1 1)
; (edge1-frame-2 f2)
; ; (1 3)
; (edge2-frame-2 f2)
; ; (2 5)

;; Exercise 5
; A directed line segment in the plane can be represented as a pair of vectors—the vector running from
; the origin to the start-point of the segment, and the vector running from the origin to the end-point of the segment.
; Use your vector representation from Exercise 2.46 to define a representation for segments with a constructor 
; make-segment and selectors start-segment and end-segment

(define make-segment cons)

(define start-segment car)

(define end-segment cdr)

; (define i (make-vect 1 1))
; (define o (make-vect 2 5))
; (define s (make-segment i o))
; 
; (start-segment s)
; ; (1 1)
; (end-segment s)
; ; (2 5)

;; Exercise 6
; Use segments->painter to define the following primitive painters:

; a. The painter that draws the outline of the designated frame.
; b. The painter that draws an “X” by connecting opposite corners of the frame.
; c. The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
; d. The wave painter.


(define outline-painter 
  (segments->painter
    (list
      (make-segment (make-vect 0 0) (make-vect 0 1))
      (make-segment (make-vect 0 1) (make-vect 1 1))
      (make-segment (make-vect 1 1) (make-vect 0 1))
      (make-segment (make-vect 0 1) (make-vect 0 0))
    )
  )
)

(define x-painter
  (segments->painter
    (list
      (make-segment (make-vect 0 0) (make-vect 1 1))
      (make-segment (make-vect 0 1) (make-vect 1 0))
    )
  )
)

(define diamond-painter
  (segments->painter
    (list
      (make-segment (make-vect 0.5 0) (make-vect 0.5 0.5))
      (make-segment (make-vect 0.5 0.5) (make-vect 0.5 1))
      (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
      (make-segment (make-vect 0 0.5) (make-vect 0.5 0))
    )
  )
)

(define wave-painter
  (segments->painter
   (list 
      (make-segment (make-vect 0 0.7) (make-vect 0.2 0.5))
      (make-segment (make-vect 0 0.5) (make-vect 0.2 0.3))
      (make-segment (make-vect 0.2 0.5) (make-vect 0.3 0.6))
      (make-segment (make-vect 0.3 0.6) (make-vect 0.4 0.6))
      (make-segment (make-vect 0.4 0.6) (make-vect 0.3 0.8))
      (make-segment (make-vect 0.3 0.8) (make-vect 0.4 1))
      (make-segment (make-vect 0.2 0.3) (make-vect 0.3 0.5))
      (make-segment (make-vect 0.3 0.5) (make-vect 0.4 0.4))
      (make-segment (make-vect 0.4 0.4) (make-vect 0.2 0))
      (make-segment (make-vect 0.5 0.3) (make-vect 0.4 0))
      (make-segment (make-vect 0.5 0.3) (make-vect 0.6 0))
      (make-segment (make-vect 0.6 1) (make-vect 0.7 0.8))
      (make-segment (make-vect 0.6 0.6) (make-vect 0.7 0.8))
      (make-segment (make-vect 0.6 0.6) (make-vect 0.7 0.6))
      (make-segment (make-vect 1 0.4) (make-vect 0.7 0.6))
      (make-segment (make-vect 0.7 0.4) (make-vect 1 0.3))
      (make-segment (make-vect 0.7 0.4) (make-vect 0.8 0))
    )
  )
)

; (define o (make-vect 0 0))
; (define e1 (make-vect 2 0))
; (define e2 (make-vect 0 2))
; (define f1 (make-frame o e1 e2))
; (outline-painter f1)
; (x-painter f1)
; (diamond-painter f1)
; (wave-painter f1)

;; Exercise 7
; Define the transformation flip-horiz, which
; flips painters horizontally, and transformations that rotate
; painters counterclockwise by 180 degrees and 270 degrees.

(define (flip-horiz painter)
  ; From:
  ; e2
  ; |
  ; o__e1
  ; To:
  ;     e2
  ;     |
  ; e1--o
  (transform-painter
    painter
    (make-vect 1.0 0.0) ; Origin o
    (make-vect 0.0 0.0) ; Edge1 e1
    (make-vect 1.0 1.0) ; Edge2 e2
  )
)

(define (rotate180 painter)
  ; From:
  ; e2
  ; |
  ; o__e1
  ; To:
  ; e1--o
  ;     |
  ;     e2
  (transform-painter
    painter
    (make-vect 1.0 1.0)
    (make-vect 0.0 1.0)
    (make-vect 1.0 0.0)
  )
)

(define (rotate270 painter)
  ; From:
  ; e2
  ; |
  ; o__e1
  ; To:
  ; o--e2
  ; |
  ; e1
  (transform-painter
    painter
    (make-vect 0.0 1.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)
  )
)

;; Exercise 8
; Define the below operation for painters. below takes two painters as arguments. The resulting 
; painter, given a frame, draws with the first painter in the bottom of the frame and with the 
; second painter in the top. Define below in two different ways—first by writing a procedure 
; that is analogous to the beside procedure given above, and again in terms of beside and suitable 
; rotation operations

(define (below painter1 painter2)
  ; From: 
  ; e2
  ; |
  ; o---e1
  ; To:
  ; painter1
  ; e2      
  ; |       
  ; o_____e1
  ; painter2
  ; e2        
  ; |
  ; o_____e1
  
  (let
    ((split-point (make-vect 0.0 0.5)))
    (let
      (
        (painter-up
          (transform-painter
            painter1
            split-point         ; Offset origin by 0.5 on the y
            (make-vect 1.0 0.5) ; Offset edge1 by 0.5 on the y
            (make-vect 0.0 1.0) ; Normal edge2
          )
        )
        (painter-down
          (transform-painter
            painter2
            (make-vect 0.0 0.0) ; Normal origin
            (make-vect 1.0 0.0) ; Normal edge1
            split-point         ; Normal edge2, but only until 0.5 on the y
          )
        )
      )
      (lambda
        (frame)
        (painter-up frame)
        (painter-down frame)
      )
    )
  )
)

(define (below-2 painter1 painter2)
  ; From: 
  ; e2
  ; |
  ; o---e1
  ; To (beside):
  ; painter1|painter2
  ; e2      |e2
  ; |       ||
  ; o_____e1|o_____e1
  ; To (rotate):
  ; painter1
  ; e2      
  ; |       
  ; o_____e1
  ; painter2
  ; e2        
  ; |         
  ; o_____e1  
  (rotate270 (beside painter1 painter2))
)

;; Exercise 9
; Make changes to the square limit of wave shown in Figure 2.9 by working at each of the 
; levels described above. In particular:

; a. Add some segments to the primitive wave painter of (to add a smile, for example)
; Modify wave-painter above (Exercise 6)

; b: Change the pattern constructed by corner-split (for example, by using only one copy of 
; the up-split and right-split images instead of two)

(define (corner-split-2 painter n)
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
          ; Cotinue splitting the right corner
          (corner (corner-split-2 painter (- n 1)))
        )
        ; Put it all together
        (beside 
          ; Put the original below the up image
          (below painter up)
          ; Put the right image below the top 
          ; right corner (will recursively be splitted)
          (below right corner)
        )
      )
    )
  )
)

; c. Modify the version of square-limit that uses squareof-four so as to assemble the 
; corners in a different pattern. (For example, you might make the big Mr. Rogers look 
; outward from each corner of the square.)

(define (square-limit-2 painter n)
  (let 
    ((combine4 
      (square-of-four 
        ; Do not transform the top left copy
        identity 
        ; Flip horizontally the top right copy
        flip-horiz 
        ; Flip vertically the bottom left copy
        flip-vert
        ; Rotate 180 the bottom right copy
        rotate180 
      )
    ))
    (combine4 (corner-split painter n))
  )
)

;; End of project
;; Don't touch anything below this

(define full-frame
  (make-frame 
    (make-vect 0.5 0.5)
    (make-vect 1 0)
    (make-vect 0 1)
  )
)
