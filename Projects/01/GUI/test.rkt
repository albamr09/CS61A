;#lang simply-scheme
#lang racket/gui

; Converting to string 
 (require racket/format)

;(+ 1 2)
;(first '(1 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define TITLE_FONT (make-object font% 20 'default 'normal 'bold))
(define WINDOW_TITLE "Twenty One")
(define W 800)
(define H 500)

; Game mode
(define NOJOKER "M0")
(define JOKER "M1")
(define MODE "")

; Strategy for the customer
(define STOP_AT '("S0" "Stop at"))
(define DEALER_SENSITIVE '("S1" "Dealer sensitive"))
(define VALENTINE '("S2" "Valentine"))
(define SUIT '("S3" "Suit"))
(define MAJORITY '("S4" "Majority"))
(define RECKLESS '("S5" "Reckless"))

(define STRATEGIES 
  (list (~a (second STOP_AT))
    (~a (second DEALER_SENSITIVE))
    (~a (second VALENTINE))
    (~a (second SUIT))
    (~a (second MAJORITY))
    (~a (second RECKLESS))))
 
; Remove reckless
(define RECKLESS_STRATEGIES (remove (~a (second RECKLESS)) STRATEGIES))

; Remove majority
(define MAJORITY_STRATEGIES (remove (~a (second MAJORITY)) STRATEGIES))

(define SUITS '("c" "h" "d" "s"))
; Remove suit
(define SUITS_STRATEGIES (remove (~a (second SUIT)) STRATEGIES))

(define STRATEGY (make-hash null))

; Do not let select inifinite strategies
(define CURR_ANIDATION 0)
(define MAX_ANIDATION 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Utility methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
; Dialog for Suit
;;;;;;;;;;;;;;;;;;;

(define (open-suit-dialog)

  (set! CURR_ANIDATION (+ CURR_ANIDATION 1))

  (define suit-dialog (new dialog%  [label "Suit Strategy"]
                [parent main-frame]
                [width 200]
                [height 200])
  )
   
  (define suit-radio-box (new radio-box%  [parent suit-dialog] 
                [label "Select a suit: "]
                [choices SUITS]
                [selection 0]
                [style '(horizontal vertical-label)]
                [horiz-margin 20]
                [vert-margin 20]))
  
  (select-button "Strategy with suit" suit-dialog #t
                 (lambda (button event)
                            (open-select-strategy-dialog SUITS_STRATEGIES main-frame "Strategy with suit")
                         ))
  
  (select-button "Strategy without suit" suit-dialog #t
                 (lambda (button event)
                            (open-select-strategy-dialog SUITS_STRATEGIES main-frame "Strategy without suit")
                         ))
  
  (select-button "Ok" suit-dialog #t
                 (lambda (button event)
                            (send suit-dialog show #f)
                            (set! CURR_ANIDATION (- CURR_ANIDATION 1))
                            (hash-set! STRATEGY 
                                       ; Key
                                       (~a CURR_ANIDATION) 
                                       ; Value
                                       (make-hash (list (cons (~a (second SUIT)) (send suit-radio-box get-item-label (send suit-radio-box get-selection)))))
                            )
                            (add-info (~a STRATEGY))
                         ))

  (send suit-dialog show #t)
)

;;;;;;;;;;;;;;;;;;;
; Dialog for Select a strategy
;;;;;;;;;;;;;;;;;;;

(define (open-select-strategy-dialog strategy-list parent label)

  (define select-strategy-dialog (new dialog%  [label label]
                [parent parent]
                [width 200]
                [height 200])
  )

 (for-each (lambda (element)
             (select-button element select-strategy-dialog
              ; enabled?
              (not (and (>= CURR_ANIDATION 3)
                        (member element (list (~a (second SUITS)) (~a (second MAJORITY)) (~a (second RECKLESS))))
                  )
              )
              ; Callback
              (lambda (button event)
                (select-screen element))
              ))
           strategy-list)

  (select-button "Ok" select-strategy-dialog #t
                 (lambda (button event)
                            (send select-strategy-dialog show #f)
                         ))

  (send select-strategy-dialog show #t)
)

;;;;;;;;;;;;;;;;;;;
; Dialog for Stop at
;;;;;;;;;;;;;;;;;;;

(define (open-stop-at-dialog)
  (define stop-at-dialog (new dialog%  [label "Stop At Strategy"]
                [parent main-frame]
                [width 200]
                [height 200])
  )

  (define stop-at-slider (new slider%  [parent stop-at-dialog] 
              [label "Select the value at which you want to stop picking cards"]
              [style '(horizontal vertical-label)]
              [horiz-margin 300]
              [vert-margin 20]
              [init-value 17]
              [min-value 1]
              [max-value 21]))
  
  (select-button "Ok" stop-at-dialog #t
                 (lambda (button event)
                            (send stop-at-dialog show #f)
                            (hash-set! STRATEGY 
                                       ; Key
                                       (~a CURR_ANIDATION) 
                                       ; Value
                                       (make-hash (list (cons (~a (second STOP_AT)) (send stop-at-slider get-value))))
                            )
                            (add-info (~a STRATEGY))
                         ))

  (send stop-at-dialog show #t)
)

(define (open-reckless-dialog)

  (set! CURR_ANIDATION (+ CURR_ANIDATION 1))

  (define reckless-dialog (new dialog%  [label "Reckless Strategy"]
                [parent main-frame]
                [width 200]
                [height 200])
  )
   
  (select-button "Select strategy" reckless-dialog #t
                 (lambda (button event)
                            (open-select-strategy-dialog RECKLESS_STRATEGIES main-frame "Select strategy")
                         ))

  (select-button "Ok" reckless-dialog #t
                 (lambda (button event)
                            (send reckless-dialog show #f)
                            (set! CURR_ANIDATION (- CURR_ANIDATION 1))
                            (hash-set! STRATEGY 
                                       ; Key
                                       (~a CURR_ANIDATION) 
                                       ; Value
                                       (make-hash (list (cons (~a (second RECKLESS)) #f)))
                            )
                            (add-info (~a STRATEGY))
                         ))

  (send reckless-dialog show #t)
)
(define (open-majority-dialog)

  (set! CURR_ANIDATION (+ CURR_ANIDATION 1))

  (define majority-dialog (new dialog%  [label "Majority Strategy"]
                [parent main-frame]
                [width 200]
                [height 200])
  )
   
  (select-button "First strategy" majority-dialog #t
                 (lambda (button event)
                            (open-select-strategy-dialog MAJORITY_STRATEGIES main-frame "First Strategy")
                         ))

  (select-button "Second strategy" majority-dialog #t
                 (lambda (button event)
                            (open-select-strategy-dialog MAJORITY_STRATEGIES main-frame "Second Strategy")
                         ))
  
  (select-button "Third strategy" majority-dialog #t
                 (lambda (button event)
                            (open-select-strategy-dialog MAJORITY_STRATEGIES main-frame "Third Strategy")
                         ))
  
  (select-button "Ok" majority-dialog #t
                 (lambda (button event)
                            (set! CURR_ANIDATION (- CURR_ANIDATION 1))
                            (send majority-dialog show #f)
                            (hash-set! STRATEGY 
                                       ; Key
                                       (~a CURR_ANIDATION) 
                                       ; Value
                                       (make-hash (list (cons (~a (second MAJORITY)) #f)))
                            )
                            (add-info (~a STRATEGY))
                         ))

  (send majority-dialog show #t)
)

(define (add-info msg)
  (define message (new message% [parent configuration-panel]
                          [label msg]
                          [vert-margin 5]))
  (send configuration-info-panel add-child message)
)

(define (enable-start-button)
  (send start-button enable #t)
)

(define (disable-start-button)
  (send start-button enable #f)
)

(define (select-screen strategy)
  (disable-start-button)
  (cond
    [(equal? strategy (~a (second STOP_AT))) (enable-start-button)
                                             (open-stop-at-dialog)]
    [(equal? strategy (~a (second SUIT))) (open-suit-dialog)]
    [(equal? strategy (~a (second MAJORITY))) (open-majority-dialog)]
    [(equal? strategy (~a (second RECKLESS))) (open-reckless-dialog)]
    [(equal? strategy (~a (second VALENTINE))) (enable-start-button)]
    [(equal? strategy (~a (second DEALER_SENSITIVE))) (enable-start-button)]
  )
)

(define (open new-screen old-screen parent)
  ; Hide old screen
  (send parent delete-child old-screen)
  ; Show new screen
  (send parent add-child new-screen)
)

(define (close-button label parent object)
  (new button% [parent parent]
               [label label]
               [vert-margin 10]
               ; callback procedure for a button click:
               [callback (lambda (button event)
                           (send object show #f)
                         )])
)

(define (select-button label parent enabled callback)
 (new button% [parent parent]
              [label label]
              [enabled enabled]
              [vert-margin 10]
              ; callback procedure for a button click:
              [callback callback])
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Window page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Make a frame by instantiating the frame% class
(define main-frame (new frame% [label WINDOW_TITLE] [width W] [height H]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; HOME page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define home-panel
  (new vertical-panel% [parent main-frame]
                [horiz-margin 20]
                [vert-margin 20]
                [alignment '(center top)])
)

(new message% [parent home-panel]
                          [label "Select version of the game"]
                          [font TITLE_FONT]
                          [vert-margin 50])

; Make a button in the frame
(select-button "Without JOKER" home-panel #t
               (lambda (button event)
                         (set! MODE NOJOKER)
                         (open configuration-panel home-panel main-frame)
                       ))

(select-button "With JOKER" home-panel #t
               (lambda (button event)
                         (set! MODE JOKER)
                         (open configuration-panel home-panel main-frame)
                       ))

; Exit button
(close-button "Exit" home-panel main-frame)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Configuration Screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define configuration-panel
  (new vertical-panel% [parent main-frame]
                [horiz-margin 20]
                [vert-margin 20]
                [style '(deleted)]
                [alignment '(center top)])
)

(new message% [parent configuration-panel]
                          [label "Select a strategy"]
                          [font TITLE_FONT]
                          [vert-margin 10]
                          [horiz-margin 10])

(define configuration-sub-panel
  (new horizontal-panel% [parent configuration-panel]
                [horiz-margin 20]
                [vert-margin 20]
                [alignment '(center center)])
)

(for-each (lambda (element)
            (select-button element configuration-sub-panel #t
                           (lambda (button event)
                             (select-screen element))
                           )
          )
          STRATEGIES
)

(define configuration-info-panel
  (new vertical-panel% [parent configuration-panel]
                [border 10]
                [horiz-margin 20]
                [vert-margin 20]
                [alignment '(center center)])
)


; Select button
(define start-button (new button% [parent configuration-panel]
             [label "Start"]
             [vert-margin 10]
             [enabled #f]
             ; callback procedure for a button click:
             [callback (lambda (button event)
                         ""
                         ;(select-screen)
                       )]))

; Back to Home button
(new button% [parent configuration-panel]
             [label "Home"]
             [vert-margin 10]
             ; callback procedure for a button click:
             [callback (lambda (button event)
                         (open home-panel configuration-panel main-frame)
                       )])

(send main-frame show #t)
