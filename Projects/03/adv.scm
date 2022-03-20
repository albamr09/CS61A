(load "../../lib/obj.scm")
(load "./tables.scm")

;; ADV.SCM
;; This file contains the definitions for the objects in the adventure
;; game and some utility procedures.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B04_1 BASIC OBJECT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class (basic-object)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Instance variables
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (instance-vars
    ; Save the properties of the class
    (properties (make-table))
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Methods
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Method to update the properties
  (method (put key value)
    (insert! key value properties)
  )

  ; Default method to get the properties
  (default-method
    (lookup message properties)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (place name)

  ; B04_1: Add basic object as parent
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Parent
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (parent (basic-object))

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

  ;; B04_2: Add type checking
  (method (place?) #t)

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

  ; // A04_1: Send a notice to every person that is in the place
  (method (enter new-person)
    ; Check if the person is already in the place
    (if (memq new-person people)
	    (error "Person already in this place" (list name new-person))
    )

    ;; B07: update people before notice, so police can
    ;; send thief to jail. If not, we send a person to jail
    ;; that is nowhere, because in go-directly-to, the thief.
    ;; 1. Leave the place we were before, remove thief from people list
    ;; 2. Enter new place, list of people stil not updated
    ;;    - Send notice to police
    ;;    - Police sends thief to jail, but thief is nowhere

    ; Update the list of people in the place
    (set! people (cons new-person people))
    ; Send notice message to everyone in the place
    (for-each 
      (lambda 
        (person) 
        ; Do not send notice message to oneself
        (if (not (eq? person new-person))
          (ask person 'notice new-person)
        )
      )
      ; List of people in the place
      people
    )
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

  ; // A04_2: may-enter?, always return true for a regular place
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
;; A04_2: LOCKED PLACE
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
;; A05: GARAGE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class (garage name)

  ; Define the place class as the parent
  (parent (place name))

  ; Serial number for tickets
  (class-vars (serial-num-counter 0))

  ; Store the tickets in a table
  (instance-vars
    (ticket-table (make-table))
  )

  (method (park vehicle)
    ; Get the owner of the vehicle
    (define owner (ask vehicle 'possessor))
    ; Check if it exists
    (if (not (eq? owner 'no-one))
      ; Check if the vehicle is in the garage
      (if (memq vehicle (usual 'things))
        (begin
          ; Generate serial number for the ticket
          (set! serial-num-counter (+ serial-num-counter 1))
          ; Save the ticket for the vehicle in the table
          ; - key: serial-num-counter
          ; - value: vehicle
          (insert! serial-num-counter vehicle ticket-table)
          ; Leave the car in the park
          (ask owner 'lose vehicle)
          ; The owner takes the ticket
          (let
            ; Create ticket
            ((parking-ticket
              (instantiate ticket serial-num-counter)
            ))
            ; Put the ticket in the parking
            (ask self 'appear parking-ticket)
            ; Take the ticket
            (ask owner 'take parking-ticket)
          )
        )
        (error "I am sorry, but you cannot a park a car if it is not in the park")
      )
      (error "Wait a minute! This car has no owner!")
    )
  )
  (method (unpark parking-ticket)
    ; Check if the object is a ticket
    (if (ticket? parking-ticket)
      ; Search the ticket in the table
      (let
        ; Obtain the serial number of the ticket
        ((ticket-number (ask parking-ticket 'number)))
        ; If there is a car for the ticket
        (if (lookup ticket-number ticket-table)
          (let
            ; Obtain the owner of the ticket
            ((owner (ask parking-ticket 'possessor)))
            ; Leave the ticket 
            (ask owner 'lose parking-ticket)
            ; Take the car
            (ask owner 'take 
              ; Car
              (lookup ticket-number ticket-table)
            )
            ; Reset the value for the entry on the table (not unparking twice)
            (insert! ticket-number #f ticket-table)
          )
          (error "Hmm, I cannot seem to find your car miss...")
        )
      )
      (error "Oh dear! This is not a ticket, you can only unpark with a ticket")
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B05_1: HOTSPOT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class (hotspot name password)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Parent
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (parent (place name))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Instance variables
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (instance-vars
    ; List of laptops connected
    (connections '())
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Methods
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (method (connect laptop pass)
    ; Check if laptop is already connected
    (if (memq laptop connections)
      (error "Oh... I do not understand, you are already connected")
      ; Check if password is correct
      (if (eq? pass password)
        ; Check if laptop is in the place
        (if (memq laptop (usual 'things))
          (set! connections (cons laptop connections))
          (error "The laptop you are trying to connect is out of range, as in, not in" name)
        )
        (error "It sure seems the password is not correct miss")
      )
    )
  )

  (method (gone laptop)
    ; If this gives and error, the next statement
    ; will not be executed
    (usual 'gone laptop)
    (set! connections (delete laptop connections)) 
  )

  (method (surf laptop url)
    ; Check if the laptop is connected to the network
    (if (memq laptop connections)
      ; Open in firefox
      (system (string-append "firefox " url " &"))
      (error "You have to connect before you surf The Internet, hmm...")
    )
  )

  ;; B05_2: Extend enter process to make user 
  ;; connect their laptop
  (method (enter user)
    ; Call parents enter
    (usual 'enter user)
    ; Get user's possessions
    (let
      ((possessions (ask user 'possessions)))
      (for-each
        (lambda
          (thing)
          ; If it is a laptop
          (if (eq? (ask thing 'type) 'laptop)
            (begin
              (print "Enter the passcode miss: ")
              (let
                ((pass (read)))
                ; Connect laptop of user
                (ask thing 'connect pass)
              )
            )
          )
        )
        possessions
      )
    )
    
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A06_1: JAIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (jail name)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Parent
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (parent (place name))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Instantiation variables
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (initialize
    ;; A06_2: Add control to leave
    (usual 'put 'no-exit? #t)
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Methods
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Override exit method from place class
  (method (exit person)
    (error "Ohoho, one does not leave prison that easily")
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A07_2: RESTAURANT 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (restaurant name food-kind food-price)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Parent
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (parent (place name))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Instance variables
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (instance-vars
    (food (instantiate food-kind))
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Methods
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (method (menu)
    ; Return list of: food name, food price
    (list (ask food 'name) food-price)
  )

  (method (sell person food-name)
    ; Check if person is in restaurant
    (if (memq person (usual 'people))
      ; Check if restaurant sells this food
      (if (eq? (ask food 'name) food-name)
        ;; E09: Check if the person is the police,
        ;;      if so, do not charge any money
        (if (eq? (ask person 'type) 'police)
          (instantiate food-kind)
          ; If not, charge normally
          (begin
            ; Check if person has enough money
            ; If so, the money is automatically updated in 
            ; pay-money method
            (if (ask person 'pay-money food-price)
              ; Create instance of food using the class
              ; passed as instace variable
              (instantiate food-kind)
              (begin
                (print "Your card has been declined ma'am")
                #f
              )
            )
          )
        )
        (begin
          (print "We do not sell this... thing you call " food-name)
          #f
        )
      )
      (begin
        (print "Oh nonono, we do not do delivery in here!")
        #f
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PERSON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (person name place)

  ; B04_1: Add basic object as parent
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Parent
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (parent (basic-object))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Instance variables
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (instance-vars
    (possessions '())
    (saying "")
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Instantiation variables
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (initialize
    (ask place 'enter self)
    ; B04_1: initialize the strength of a person
    ; on instantiation
    (usual 'put 'strength 50)
    ; A07_1 initialize the money of a person
    ; on instantiation
    (usual 'put 'money 100)
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Methods
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (method (type) 'person)

  ;; B04_2: Add type checking
  (method (person?) #t)

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
    ; If you can take then thing, it returns itself
    ; otherwhise it returns false
    (set! thing (ask thing 'may-take? self))
    ; If there is a thing to take
    (if thing
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
      (error "May the force be with you")
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
          ;; A06_1: Remove logic here, and move it to 
          ;; go-directly-to
          (ask self 'go-directly-to new-place)
        )
      )
    )
  ) 

  ;;; B03: take-all, the person takes all the things in 
  ;;; the current place, if not owned
  (method (take-all)
    (let
      ; Get the list of things in a place
      ((things (ask place 'things)))
      (for-each
        (lambda
          (thing)
          ; Check if they are not owned
          (if (eq? (ask thing 'possessor) 'no-one)
            ; Take the thing
            (ask self 'take thing) 
          )
        )
        things
      )
    )
  )

  ;;; A06_1: the person goes directly to a place (does not have)
  ;;; to be adyacent
  (method (go-directly-to new-place)
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

  ;; A07_1: get-money -> add to the amount of money of the person
  (method (get-money amount)
    ; Update the amount of money
    (usual 'put 'money
      (+
        ; Get person's money
        (usual 'money)
        amount
      )
    )
  )

  ;; A07_1: pay-money -> substract to the amount of money of 
  ;; the person
  (method (pay-money amount)
    (let
      ; Get person's money
      ((money (usual 'money)))
      (if (>= money amount)
        ; Update person's money and return #t
        (begin
          ; Use get-money with negative value
          (ask self 'get-money (* -1 amount))
          #t
        )
        ; Return false when person does not have enough money
        #f
      )
    )
  )

  ;;; A08: the person buys food
  (method (buy food-name)
    (let
      ; Buy the food from the restaurant
      ; If the place is not a restaurant, it does not have
      ; sell method and resturns #f
      ; If the thing is not a food, the resturant will no have
      ; it, and will return #f
      ((food 
         ;; Ask the restaurant to sell it to you
         (ask place 'sell self food-name)
      ))
      ; Check if the restaurant sold it with no problem
      (if food
        food
        (error "Well, I guess you should try another restaurant")
      )
    )
  )

  ;;; B06_3: the person eats food
  (method (eat)
    (define total-calories 0)
    ; Get the person's possesions
    (for-each
      (lambda
        (thing)
        ; If it is edible
        (if (edible? thing)
          (begin
            ; Add calories of all things
            (set! total-calories
              (+ total-calories (ask thing 'calories))
            )
            ; Remove thing from possesions
            (ask self 'lose thing)
            ; Remove thing from place
            (ask place 'gone thing)
          )
        )
      )
      possessions
    )
    ; Update strengh of person
    (usual
      'put
      'strength
      (+
        (ask self 'strength)
        total-calories
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
                ; B04_1: Add basic object as parent
                (parent (instantiate basic-object))
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
	        	        ;(error "Can't use USUAL without a parent." 'thing)
                    ; B04_1: call parent method
                    (parent message)
                  )
	                ((eq? message 'name) (lambda () name))
	                ((eq? message 'possessor) (lambda () possessor))
	                ((eq? message 'type) (lambda () 'thing))
                  ;; B04_2: Add type checking
	                ((eq? message 'thing?) (lambda () #t))
	                ((eq? message 'change-possessor)
	        	        (lambda 
                      (new-possessor)
	        	          (set! possessor new-possessor)
                    )
                  )
                  ;; B08: Compare strengh of owner and receiver
                  ;;      to determine if the receiver gets the thing
                  ;;      if so, return the thing itself
	                ((eq? message 'may-take?) 
                    (lambda 
                      (receiver) 
                      ; Check if thing has possessor
                      (if (eq? possessor 'no-one)
                        self
                        (let
                          (
                            ; Obtain strenghts
                            (owner-strengh (ask possessor 'strength))
                            (receiver-strengh (ask receiver 'strength))
                          )
                          ; Compare strenghts
                          (if (>= receiver-strengh owner-strengh)
                            ; If the receiver is more or equally strong
                            self
                            #f
                          )
                        )                      
                      )
                    )
                  )
	                ;(else (no-method 'thing))
                  ;; B04_2: Add typechecking
                  ;(else (lambda () #f))
                  ;; B06_1: Delegate to parent
                  (else (parent message))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A05: TICKET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ticket
  (let 
    ; Initialize the name to ticket
    ((name 'ticket))
    (lambda (class-message)
      (cond
        ; Instantiation
        ((eq? class-message 'instantiate)
	        (lambda 
            ; Parameters of the class
            (number)
	          (let 
              (
                (self '()) 
                ; Define the parent
                (parent (instantiate thing name))
              )
              ; Dispatch
	            (define (dispatch message)
	              (cond
                  ; Initialization of instance variables
	                ((eq? message 'initialize)
	        	        (lambda 
                      (value-for-self)
	        	          (set! self value-for-self)
                    )
                  )
                  ; Number of ticket
	                ((eq? message 'number) (lambda () number))
                  ; Deletation to the parent
	                (else (parent message))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B05_1: LAPTOP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (laptop name)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Parent
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (parent (thing name))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Methods
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (method (connect password) 
    ; Obtain the person that owns it
    (define owner (usual 'possessor))
    ; If it is owned by somebody
    (if (not(eq? owner 'no-one))
      ; Send connect message to place where it is
      (ask (ask owner 'place) 'connect self password)
      (error "This is confusing, this laptop has no owner, how can it connect itself?")
    )
  )

  (method (surf url)
    ; Obtain the person that owns it
    (define owner (usual 'possessor))
    ; If it is owned by somebody
    (if (not(eq? owner 'no-one))
      ; Send surf message to the place where it is
      (ask (ask owner 'place) 'surf self url)
      (error "Hah? This laptop does not have an owner!")
    )
  )

  (method (type) 'laptop)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B06_1: FOOD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (food name num-calories)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Parent
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (parent (thing name))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Instance variables
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (instance-vars
    (calories num-calories)
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Instantiation variables
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (initialize
    (usual 'put 'edible? #t)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B06_2: PASTA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (pasta)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Parent
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (parent (food 'pasta 150))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation of thieves for part two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *foods* '(pizza potstickers coffee))

(define (edible? thing)
  ;; B06_1: use food class to determine if thing is edible
  ; (member? (ask thing 'name) *foods*)
  (ask thing 'edible?)
)

(define-class (thief initial-name initial-place)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Parent
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (parent (person initial-name initial-place))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Instance variables
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (instance-vars
    (behavior 'steal)
    ;(behavior 'run)
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Instantiation variables
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (initialize
    ; Initialize strengh
    (usual 'put 'strength 150)
  )

  ; Methods
  (method (type) 'thief)

  (method (notice person)
    (if (eq? behavior 'run)
      ;; A06_2: Check if the place can be left
      (if (not (ask (usual 'place) 'no-exit?))
        (begin 
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
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B07: POLICE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class (police initial-name initial-place)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Parent
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (parent (person initial-name initial-place))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Class variables
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (class-vars 
    (azkaban (instantiate jail 'Azkaban))
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Instantiation variables
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (initialize
    ; Update talk value
    (usual 'set-talk "Crime Does Not Pay")
    ; Initialize strengh
    (usual 'put 'strength 200)
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Methods
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Override person's type method
  (method (type) 'police)

  ; Override person's notice method 
  (method (notice person) 
    ; Check if it is a thief
    (if (eq? (ask person 'type) 'thief)
      (begin
        ; The police talks
        (usual 'notice person)
        ; Take away the thief's possesions
        (for-each
          (lambda
            (thing)
            (ask self 'take thing)
          )
          (ask person 'possessions)
        )
        ; Ask thief to go to jail
        (ask person 'go-directly-to azkaban)
      )
    )
  )

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
  (list-ref set (random (length set)))
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
    ;; B04_2: Change type checking to use the class' method
    (ask obj 'person?)
    ; (member? (ask obj 'type) '(person police thief))
  )
)

(define (thing? obj)
  (and 
    (procedure? obj)
    ;; B04_2: Change type checking to use the class' method
    (ask obj 'thing?)
    ; (eq? (ask obj 'type) 'thing)
  )
)

;; B04_2: Add type checking for place
(define (place? obj)
  (and 
    (procedure? obj)
    (ask obj 'place?)
  )
)

;; A05: Check if object is a ticket
(define (ticket? obj)
  (and 
    (procedure? obj)
    (eq? (ask obj 'name) 'ticket)
  )
)

;;;;;;;;;;;
;; A05: Get a ticket from the a persons posessions

(define (get-ticket obj)
  (if (person? obj)
    ; Filter the person's possessions
    (filter-ticket 
      (ask obj 'possessions)
    )
  )
)

; Recursive process, stops when it finds a ticket
(define (filter-ticket tickets)
  (if (ticket? (car tickets))
    ; Return the ticket
    (car tickets)
    ; Keep searching
    (filter-ticket (cdr tickets))
  )
)
;;;;;;;;;;;
