(load "../../lib/obj.scm")

;; ADV.SCM
;; This file contains the definitions for the objects in the adventure
;; game and some utility procedures.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (place name)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Instance variables
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (instance-vars
    (directions-and-neighbors '())
    (things '())
    (people '())
    (entry-procs '())
    (exit-procs '())
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Methods
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (method (type) 'place)
  (method (neighbors) (map cdr directions-and-neighbors))
  (method (exits) (map car directions-and-neighbors))

  (method (look-in direction)
    (let 
      ((pair (assoc direction directions-and-neighbors)))
      (if (not pair)
	      '()                     ;; nothing in that direction
	      (cdr pair)              ;; return the place object
      )
    )
  )           

  (method (appear new-thing)
    (if (memq new-thing things)
	    (error "Thing already in this place" (list name new-thing))
    )
    (set! things (cons new-thing things))
    'appeared
  )

  ; // E04_1: Send a notice to every person that is in the place
  (method (enter new-person)
    ; Check if the person is already in the place
    (if (memq new-person people)
	    (error "Person already in this place" (list name new-person))
    )
    ; Before updating the list of people in the place
    (for-each 
      ; Send notice message to everyone in the place
      (lambda 
        (person) 
        (ask person 'notice self)
      )
      ; List of people in the place
      people
    )
    ; Update the list of people in the place
    (set! people (cons new-person people))
    ; Call every enter procedure define for the place
    (for-each 
      (lambda (proc) (proc)) 
      entry-procs
    )
    'appeared
  )

  (method (gone thing)
    (if (not (memq thing things))
	    (error "Disappearing thing not here" (list name thing))
    )
    (set! things (delete thing things)) 
    'disappeared
  )
  
  (method (exit person)
    (for-each 
      (lambda (proc) (proc)) 
      exit-procs
    )
    (if (not (memq person people))
	    (error "Disappearing person not here" (list name person))
    )
    (set! people (delete person people)) 
    'disappeared
  )

  (method (new-neighbor direction neighbor)
    (if (assoc direction directions-and-neighbors)
	    (error "Direction already assigned a neighbor" (list name direction))
    )
    (set! directions-and-neighbors
	    (cons 
        (cons direction neighbor) 
        directions-and-neighbors
      )
    )
    'connected
  )

  ; // E04_2: may-enter?, always return true for a regular place
  (method (may-enter? person)
    #t
  )

  ; Methods for updating the list of procedures to call 
  ; when a person enters/exits the place
  (method (add-entry-procedure proc)
    (set! entry-procs (cons proc entry-procs))
  )
  (method (add-exit-procedure proc)
    (set! exit-procs (cons proc exit-procs))
  )
  (method (remove-entry-procedure proc)
    (set! entry-procs (delete proc entry-procs))
  )
  (method (remove-exit-procedure proc)
    (set! exit-procs (delete proc exit-procs))
  )
  (method (clear-all-procs)
    (set! exit-procs '())
    (set! entry-procs '())
    'cleared
  ) 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; E04_2: LOCKED PLACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class (locked-place name key-lock)

  ; Define the place class as the parent
  (parent (place name))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Instance variables
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (instance-vars
    (unlocked #f)
  )
  (method (may-enter? person) unlocked)
  (method (unlock key)
    (cond
      ; If the place is already unlocked
      (unlocked
        (print "Oh dear, just check the doorknob")
      )
      ; If the key is correct
      ((equal? key key-lock)
        (set! unlocked #t)
        (print (word "You have unlocked " name))
      )
      ; If the key is not correct
      (else
        (error "You cannot seriouly suppose that is the key...")
      )
    )
  )
  (method (lock key)
    (cond
      ; If the place is already locked
      ((not unlocked)
        (print "What are you doing, the place is already locked my friend")
      )
      ; If the key is correct
      ((equal? key key-lock)
        (set! unlocked #f)
        (print (word "You have locked " name))
      )
      ; If the key is not correct
      (else
        (error "I am sorry to disclose to you: that is not the key")
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PERSON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (person name place)
  ; Instance variables
  (instance-vars
    (possessions '())
    (saying "")
  )

  ; Instantiation variables
  (initialize
   (ask place 'enter self)
  )

  ; Methods
  (method (type) 'person)

  (method (look-around)
    (map 
      (lambda 
        (obj) 
        (ask obj 'name)
      )
	    (filter 
        (lambda 
          (thing) 
          (not (eq? thing self))
        )
		    (append 
          (ask place 'things) 
          (ask place 'people)
        )
      )
    )
  )

  (method (take thing)
    (cond 
      ((not (thing? thing)) (error "Not a thing" thing))
	    ((not (memq thing (ask place 'things)))
	      (error 
          "Thing taken not at this place"
		      (list (ask place 'name) thing)
        )
      )
	    ((memq thing possessions) (error "You already have it!"))
	    (else
	      (announce-take name thing)
	      (set! possessions (cons thing possessions))
	      ;; If somebody already has this object...
	      (for-each
	        (lambda 
            (pers)
	          (if (and (not (eq? pers self)) ; ignore myself
		          (memq thing (ask pers 'possessions)))
		          (begin
		            (ask pers 'lose thing)
		            (have-fit pers)
              )
            )
          )
	        (ask place 'people)
        )
	      (ask thing 'change-possessor self)
	      'taken
      )
    )
  )

  (method (lose thing)
    (set! possessions (delete thing possessions))
    (ask thing 'change-possessor 'no-one)
    'lost
  )

  (method (talk) (print saying))
  (method (set-talk string) (set! saying string))
  (method (exits) (ask place 'exits))
  (method (notice person) (ask self 'talk))

  (method (go direction)
    (let 
      ((new-place (ask place 'look-in direction)))
      (cond 
        ((null? new-place)
	        (error "Can't go" direction)
        )
	      (else
          ; Check if the place we are going is locked
          (if (ask new-place 'may-enter? self)
            ; If the place is not locked
            (begin
              ; Leave the previous place
	            (ask place 'exit self)
	            (announce-move name place new-place)
              ; Move the person's possessions to the old place to the
              ; new place
	            (for-each
	              (lambda 
                  (p)
		              (ask place 'gone p)
		              (ask new-place 'appear p)
                )
	              possessions
              )
              ; Update the place where the person is
	            (set! place new-place)
              ; Enter the place
	            (ask new-place 'enter self)
            )
            (error "Where do you think you are going?!?! The place is locked!")
          )
        )
      )
    )
  ) 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define thing
  (let 
    ()
    (lambda (class-message)
      (cond
        ((eq? class-message 'instantiate)
	        (lambda 
            (name)
	          (let 
              (
                (self '()) 
                (possessor 'no-one)
              )
	            (define (dispatch message)
	              (cond
	                ((eq? message 'initialize)
	        	        (lambda 
                      (value-for-self)
	        	          (set! self value-for-self)
                    )
                  )
	                ((eq? message 'send-usual-to-parent)
	        	        (error "Can't use USUAL without a parent." 'thing)
                  )
	                ((eq? message 'name) (lambda () name))
	                ((eq? message 'possessor) (lambda () possessor))
	                ((eq? message 'type) (lambda () 'thing))
	                ((eq? message 'change-possessor)
	        	        (lambda 
                      (new-possessor)
	        	          (set! possessor new-possessor)
                    )
                  )
	                (else (no-method 'thing))
                )
              )
	            dispatch
            )
          )
        )
        (else 
          (error "Bad message to class" class-message)
        )
      )
    )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation of thieves for part two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *foods* '(pizza potstickers coffee))

(define (edible? thing)
  (member? (ask thing 'name) *foods*)
)

(define-class (thief initial-name initial-place)
  ; Child of person class
  (parent (person initial-name initial-place))

  ; Instance variables
  (instance-vars
   (behavior 'steal)
  )

  ; Methods
  (method (type) 'thief)

  (method (notice person)
    (if (eq? behavior 'run)
	    (ask self 'go (pick-random (ask (usual 'place) 'exits)))
	    (let 
        ((food-things
	        (filter 
            (lambda (thing)
	    		    (and 
                (edible? thing)
	    		      (not (eq? (ask thing 'possessor) self))
              )
            )
	    	    (ask (usual 'place) 'things)
          )
        ))
	      (if (not (null? food-things))
	        (begin
	          (ask self 'take (car food-things))
	          (set! behavior 'run)
	          (ask self 'notice person)
          ) 
        )
      )
    )
  )
)

(define-class (police initial-name initial-place)

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this next procedure is useful for moving around

(define (move-loop who)
  (newline)
  (print (ask who 'exits))
  (display "?  > ")
  (let 
    ((dir (read)))
    (if (equal? dir 'stop)
	    (newline)
	    (begin 
        (print (ask who 'go dir))
	      (move-loop who)
      )
    )
  )
)


;; One-way paths connect individual places.

(define (can-go from direction to)
  (ask from 'new-neighbor direction to)
)


(define (announce-take name thing)
  (newline)
  (display name)
  (display " took ")
  (display (ask thing 'name))
  (newline)
)

(define (announce-move name old-place new-place)
  (newline)
  (newline)
  (display name)
  (display " moved from ")
  (display (ask old-place 'name))
  (display " to ")
  (display (ask new-place 'name))
  (newline)
)

(define (have-fit p)
  (newline)
  (display "Yaaah! ")
  (display (ask p 'name))
  (display " is upset!")
  (newline)
)


(define (pick-random set)
  (nth (random (length set)) set)
)

(define (delete thing stuff)
  (cond ((null? stuff) '())
	  ((eq? thing (car stuff)) 
     (cdr stuff)
    )
	  (else 
      (cons 
        (car stuff) 
        (delete thing (cdr stuff))
      )
    )
  )
)

(define (person? obj)
  (and 
    (procedure? obj)
    (member? (ask obj 'type) '(person police thief))
  )
)

(define (thing? obj)
  (and 
    (procedure? obj)
    (eq? (ask obj 'type) 'thing)
  )
)
