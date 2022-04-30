;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLASS DEFINITIONS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;
; PYTHON BASE OBJECT
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class (py-obj)
  (method (true?) #f)
  (method (mutable?) #f)
  (method (none?) #f)
  (method (int?) #f)
  (method (float?) #f)
  (method (string?) #f)
  (method (bool?) #f)
  (method (list?) #f)
  (method (procedure?) #f)
  (method (primitive?) #f)
  (method (dictionary?) #f)
  (default-method
    (py-error 
      "AttributeError: objects of type "
	    (objtype self)
	    " have no method " message
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;
; PYTHON NONE OBJECT
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class (none)
  ; Parent is python object
  (parent (py-obj))
  ; Override check of none object
  (method (none?) #t)
  ; Set the type
  (method (type) '*NONE*)
  ; To string method
  (method (__str__) (make-py-string "None"))
)
; Create global none object
(define *NONE* (instantiate none))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; PYTHON STRING OBJECT
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class (py-string val)
  ; Parent is python object
  (parent (py-obj))
  ; Override check for true
  (method (true?) 
    (not (zero? (string-length val)))
  )
  ; Override type check
  (method (string?) #t)
  ; Set type
  (method (type) 'py-string)
  ; String methods
  (method (__str__) self)
  (method (__int__) (make-py-num (string->number val)))
  (method (__len__) (make-py-num (string-length val)))
  ; Add two strings
  (method (__add__)
    ; Create python primitive function
    (make-py-primitive
      '+
      (lambda 
        (other)
        (cond 
          ; Is the other object a string
          ((ask other 'string?)
	          (make-py-string 
              ; Append strings
              (string-append 
                ; This string's value (string literal)
                val 
                ; The other string's value (string literal)
                (ask other 'val)
              )
            )
          )
          ; Else throw error
	        (else 
            (py-error "TypeError: Cannot concatenate string and " (ask other 'type))
          )
        )
      )
    )
  )
  (method (__mul__)
    ; Create python primitive function
    (make-py-primitive
      '*
      (lambda 
        (other)
        ; Create string
        (make-py-string
          ; Concatenate strings
    	    (accumulate 
            string-append
            ; Starting string
		        ""
            ; Create a list made up of the two 
            ; strings
		        (make-list 
              (ask other 'val) 
              val
            )
          )
        )
      )
    )
  )
  ; usage: __getitem__(self, index), where index can be a range => [1, 2]
  (method (__getitem__ index)
    ;; does not handle three-index slices or inferred blanks
    (let 
      ; Get value of index
      ((n (ask (car index) 'val)))
      ; If n is not an integer
      (if (not (integer? n))
	      (py-error "TypeError: string indices must be integers, not " (objtype (car index)))
      )
      ; Else if n is not inside the range of the length of the string
      (if 
        (not 
          (and 
            (>= n (- (string-length val)))
		        (< n (string-length val))
          )
        )
        ; throw error
	      (py-error "IndexError: string index out of range: " n)
      )
      ; If n is in range but is negative, then the index = [-n], which indicates to take the
      ; last n elements instead of the n first
      (if (< n 0) 
        ; Sum len(val) to invert the selection of elements
        (set! n (+ n (string-length val)))
      )
      ; If the second index is null
      (if (null? (cdr index))
        ; Create the string corresponding to one element (1+)
	      (make-py-string 
          (substring val n (1+ n))
        )
        ; Else
	      (let 
          ; Obtain the next index
          ((m (ask (cadr index) 'val)))
          ; If it is not and integer: error
	        (if (not (integer? m))
		        (py-error "TypeError: string indices must be integers, not " (objtype (cadr index)))
          )
          ; If is not in range for the string
	        (if (not 
                (and 
                  (>= m (- (string-length val)))
		    	        (< m (string-length val))
                )
              )
		        (py-error "IndexError: string index out of range: " m)
          )
          ; If m is in range but is negative, then the index = [-m], which indicates to count the index
          ; for the end of the string
	        (if (< m 0) 
            ; Sum len(val) to invert the selection of elements
            (set! m (+ m (string-length val)))
          )
          ; If there is a third index throw error: not supporter
	        (if (not (null? (cddr index)))
		        (py-error "only one- and two- element slices implemented")
          )
          ; Create the substring made up from the characters in the string from n to m
	        (make-py-string (substring val n m)))
      )
    )
  )
  (method (__reversed__)
    (make-py-primitive
     'reversed
      (lambda 
        () 
        ; Create python string object
        (make-py-string 
          ; Create string from reversed list
          (list->string 
            ; Reverse list
            (reverse 
              ; Create list
              (string->list val))
          )
        )
      )
    )
  )
  (method (__sorted__)
    (make-py-primitive
      'sorted
      (lambda ()
        ; Create python list
        (make-py-list 
          (map 
            (lambda 
              (char) 
              ; Tranform each element of the list into
              ; a char
              (make-py-string (string char))
            )
            ; Make string a list and sort alfabetically
		  	    (sort (string->list val) char<?)
          )
        )
      )
    )
  )

  ; Create iterator for the string
  ; var-name: name for the substring created by the iterator
  ; block: sequence of expression executed when we initialize iterator
  (method (__iter__ var-name block env)
    (define (iter i)
      ; If the index i is bigger than the number of characteres in val
      (if (>= i (string-length val))
        ; Return null
	      *NONE*
        ; Else
	      (let 
          ((result 
             (eval-sequence
                ; Block we evaluate for every iteration
		    	      block
                ; Define the substring in the 
                ; environment
		    	      (begin 
                  (define-variable!
		    	    	    var-name
		    	    	    (make-py-string (substring val i (1+ i)))
		    	    	    env
                  )
		    	    	  env
                )
              )
          ))
	        (cond 
            ; If the result is a break
            ((eq? result '*BREAK*) '*BREAK*)
            ; If the result is return something
		        ((and 
                (pair? result) 
                (eq? (car result) '*RETURN*)
              ) 
             ; return result obtained
              result
            )
            ; Else keep iterating
		        (else (iter (1+ i)))
          )
        )
      )
    )
    (let 
      ; First iteration
      ((result (iter 0)))
      ; If the result is return something
      (if (and 
            (pair? result) 
            (eq? (car result) '*RETURN*)
          )
        ; return result
	      result
        ; else return nothing or null
	      *NONE*
      )
    )
  )

  ; Capitalize string method
  (method (capitalize)
    (make-py-primitve
      'capitalize
      (lambda 
        ()
        (make-py-string 
          ; Append the result
          (append-string 
            ; First letter capitalized
            (string (char-upcase (string-ref val 0)))
            ; Rest of string 
				    (substring val 1 (string-length val))
          )
        )
      )
    )
  )

  ; Method to check if strings are equal
  (method (__eq__)
    (make-py-primitive
      ; Primitive operator sign
      '==
      (lambda 
        ; "other" = other string
        (other) 
        (make-py-bool 
          ; Check if "val" of this string equals "val" of the other string
          (string=? val (ask other 'val))
        )
      )
    )
  )

  ; Method to check if strings are not equal
  (method (__ne__)
    (make-py-primitive
      ; Primitive operator sign
      '!=
      (lambda 
        ; "other" = other string
        (other) 
        (make-py-bool 
          ; Check if "val" of this string does not equal "val" of the other string
          (not (string=? val (ask other 'val)))
        )
      )
    )
  )

  ; Check if one string is "less" than the other
  (method (__lt__)
    (make-py-primitive
      ; Primitive operator sign
      '<
      (lambda 
        ; "other" = other string
        (other) 
        (make-py-bool 
          ; Check if "val" of this string is less than "val" of the other string
          (string<? val (ask other 'val))
        )
      )
    )
  )

  ; Check if one string is "greater" than the other
  (method (__gt__)
    (make-py-primitive
      ; Primitive operator sign
      '>
      (lambda 
        (other) 
        (make-py-bool 
          ; Check if "val" of this string is greater than "val" of the other string
          (string>? val (ask other 'val))
        )
      )
    )
  )

  ; Check if one string is "less" than or equal the other
  (method (__le__)
    (make-py-primitive
      ; Primitive operator sign
      '<=
      (lambda 
        (other) 
        ; Create python boolean
        (make-py-bool 
          ; Check if "val" of this string is less or equal than "val" of the other string
          (string<=? val (ask other 'val))
        )
      )
    )
  )

  ; Check if one string is "greater" than or equal the other
  (method (__ge__)
    (make-py-primitive
      ; Primitive operator sign
      '>=
      (lambda 
        (other) 
        ; Create python boolean
        (make-py-bool 
          ; Check if "val" of this string is greater or equal than "val" of the other string
          (string>=? val (ask other 'val))
        )
      )
    )
  )

  ; Check if the string ends with a given string
  (method (endswith)
    (make-py-primitive
      ; Primitive operator name
      'endswith
      (lambda 
        ; Given string
        (other)
        (let 
          (
            ; Obtain length of this string
            (len1 (string-length val))
            ; Obtain given string
	          (suffix (ask other 'val))
          )
	        (let 
            ; Obtain length of given string
            ((len2 (string-length (ask other 'val))))
            ; If the given string is bigger
	          (if (> len2 len1)
              ; This string cannot end with a bigger string
	            (make-py-bool #f)
              ; Else
	            (let 
                ; Obtain the last n characters of this string, where
                ; n equals the number of characters of the given string
                ((tail (substring val (- len1 len2) len1)))
                ; Check if the string made up of the n last character of this string
                ; and the given string are equal
	      	      (make-py-bool (string=? tail suffix))
              )
            )
          )
        )
      )
    )
  )

  ; Check if the string is alphanumeric
  (method (isalnum)
    (make-py-primitive
      'isalnum
      (lambda ()
        (make-py-bool 
          ; Check that every char in this string is alphanumeric 
          (accumulate 
            (lambda 
              ; a: element of first list #t
              ; b: element of second list -> map
              (a b) 
              (and a b)
            )
            ; First list
		  	    #t
            ; Second list: map every character to the boolean value result
            ; of the check of: is the character alphabetic or numeric?
		  	    (map 
              (lambda 
                (char) 
                (or 
                  (char-alphabetic? char)
		  	 			    (char-numeric? char)
                )
              )
              ; Make a list from the characters in the string
		  	      (string->list val)
            )
          )
        )
      )
    )
  )

  ; Check if the string is alphabetic
  (method (isalpha)
    (make-py-primitive
      'isalpha
      (lambda 
        ()
        (make-py-bool 
          ; accumulate returns true if every element of the 
          ; string is alphabetic
          (accumulate 
            (lambda 
              ; a: element of first list #t
              ; b: element of second list -> map
              (a b) 
              (and a b)
            )
		  		#t
		  		(map 
            ; Check if every character is alphabetic
            char-alphabetic? 
            ; Make list out of this string's characters
            (string->list val))
          )
        )
      )
    )
  )

  ; Check if the string is numeric 
  (method (isdigit)
    (make-py-primitive
      'isdigit
      (lambda 
        ()
        (make-py-bool 
          ; accumulate returns true if every element in the string is a digit
          (accumulate 
            (lambda 
              ; a: element of first list #t
              ; b: element of second list -> map
              (a b) 
              (and a b)
            )
		  		  #t
		  		  (map 
              ; Check if every character is a digit
              char-numeric? 
              ; Make list out of this string's characters
              (string->list val)
            )
          )
        )
      )
    )
  )

  ; Check if this string is lowercase 
  (method (islower)
    (make-py-primitive
      'islower
      (lambda ()
        (make-py-bool 
          ; Accumulate returns true if every character is lowercase
          (accumulate 
            (lambda 
              ; a: element of first list #t
              ; b: element of second list -> map
              (a b) 
              (and a b)
            )
		  		  #t
		  		  (map 
              ; Check if every character is lowercase
              char-lower-case? 
              ; Make up a list of this string's elements
              (string->list val)
            )
          )
        )
      )
    )
  )

  ; Check if this string is only spaces
  (method (isspace)
    (make-py-primitive
      'isspace
      (lambda ()
        (make-py-bool 
          ; accumulate returns true if every character in the string is a space
          (accumulate 
            (lambda 
              ; a: element of first list #t
              ; b: element of second list -> map
              (a b) 
              (and a b)
            )
			  	  #t
			  	  (map 
              ; Map to true if the given element is a space
              char-whitespace? 
              ; Make up a list of this string's characters
              (string->list val)
            )
          )
        )
      )
    )
  )

  ; Check if this string is in uppercase
  (method (isupper)
    (make-py-primitive
     'isupper
     (lambda 
        () 
        (make-py-bool 
          ; accumulate returns true if every element in the string is uppercase
          (accumulate 
            (lambda 
              ; a: element of first list #t
              ; b: element of second list -> map
              (a b) 
              (and a b)
            )
			   	  #t
			   	  (map 
              ; Map to true if the given char is uppercase
              char-upper-case?
              ; Make up a list from the string's characters
			   	    (string->list val)
            )
          )
        )
      )
    )
  )

  ; Method to make the string lowercase
  (method (lower)
    (make-py-primitive
      'lower
      (lambda ()
        ; Create a new python string
        (make-py-string 
          ; Convert the list to a string
          (list->string 
            (map 
              (lambda 
                ; For each character
                (char) 
                ; Make it lowercase
                (char-downcase char)
              )
              ; Make up a list of this string's characters
	            (string->list val)
            )
          )
        )
      )
    )
  )

  ; Check if this string starts with a given string
  (method (startswith)
    (make-py-primitive
      'startswith
      (lambda 
        ; Given string
        (other)
        (let 
          (
            ; Length of this string
            (len1 (string-length val))
            ; Given string literal value
	          (suffix (ask other 'val))
          )
	        (let 
            ; Given string length
            ((len2 (string-length (ask other 'val))))
            ; If the length of the given string is greater,
            ; this string cannot start with the given string
	          (if (> len2 len1)
              ; Return false
	            (make-py-bool #f)
              ; Else
	            (let 
                ; Obtain the first n characters (=len2)
                ((tail (substring val 0 len2)))
                ; Check if the first n characters equal the given string
	  	          (make-py-bool (string=? tail suffix))
              )
            )
          )
        )
      )
    )
  )

  ; Method to make this string uppercase
  (method (upper)
    (make-py-primitive
      'upper
      (lambda 
        ()
        ; Create python string
        (make-py-string 
          ; Convert the list of uppercase characters to string
          (list->string 
            (map 
              (lambda 
                ; For each character
                (char) 
                ; Make it uppercase
                (char-upcase char)
              )
              ; Make list out of the characters in this string
              (string->list val)
            )
          )
        )
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;
; PYTHON NUMBER OBJECT
;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Parent class for py-int and py-float.  Never constructed, only exists to
 ;; provide wrappers for Scheme number functions to avoid duplicate code.
(define-class (py-num val)
  ;;; Parent class is python object 
  (parent (py-obj))
  (method (true?) (not (zero? val)))
  ; Make string out of a number
  (method (__str__) (make-py-string (number->string val)))
  ; Add two numbers
  (method (__add__)
    (make-py-primitive
      '+
      (lambda 
        ; The other number 
        (other) 
        (make-py-num 
          ; Sum the value of this number and the value of the 
          ; other number
          (+ val (ask other 'val))
        )
      )
    )
  )
  ; Substract two numbers
  (method (__sub__)
    (make-py-primitive
      '-
      (lambda 
        ; The other number
        (other) 
        (make-py-num 
          ; Substract the value of the other number to this number
          (- val (ask other 'val))
        )
      )
    )
  )
  ; Multiply two numbers
  (method (__mul__)
    (make-py-primitive
      '*
      (lambda 
        ; The other number
        (other) 
        (make-py-num 
          ; Multiply the value of this number by the value of the other number
          (* val (ask other 'val))
        )
      )
    )
  )
  ; Divide two numbers
  (method (__div__)
    (make-py-primitive
      '/
      (lambda 
        ; The other number
        (other) 
        (make-py-num 
          ; Divide the value of this number by the value of the other number
          (/ val (ask other 'val))
        )
      )
    )
  )
  ; Raise this number to the power of other number
  (method (__pow__)
    (make-py-primitive
      '**
      (lambda 
        ; The other number
        (other) 
        ; Raise the value of this number to the value of the other number
        (make-py-num (expt val (ask other 'val)))
      )
    )
  )
  ; Check if two numbers are equal
  (method (__eq__)
    (make-py-primitive
      '==
      (lambda 
        ; The other number
        (other) 
        (make-py-bool 
          ; Check if the value of this number equals the value of the other number
          (= val (ask other 'val))
          )
        )
      )
  )
  ; Check if two numbers are different
  (method (__ne__)
    (make-py-primitive
      '!=
      (lambda 
        ; The other number
        (other) 
        (make-py-bool 
          ; Check if the value of this number is not equal to the value of the other number
          (not (= val (ask other 'val)))
        )
      )
    )
  )
  ; Check if this number is greater than another number
  (method (__gt__)
    (make-py-primitive
      '>
      (lambda 
        ; The other number
        (other) 
        (make-py-bool 
          ; Check if the value of this number is greater than the value of the other number
          (> val (ask other 'val))
        )
      )
    )
  )
  ; Check if this number is less than another number
  (method (__lt__)
    (make-py-primitive
      '<
      (lambda 
        ; The other number
        (other) 
        (make-py-bool 
          ; Check if the value of this number is less than the value of the other number
          (< val (ask other 'val))
        )
      )
    )
  )
  ; Check if this number is greater than or equal to another number
  (method (__ge__)
    (make-py-primitive
      '>=
      (lambda 
        ; The other number
        (other) 
        (make-py-bool 
          ; Check if the value of this number is greater than or equal to 
          ; the value of the other number
          (>= val (ask other 'val))
        )
      )
    )
  )
  ; Check if this number is less than or equal to another number
  (method (__le__)
    (make-py-primitive
      '<=
      (lambda 
        ; The other number
        (other) 
        (make-py-bool 
          ; Check if the value of this number is less than or equal to the 
          ; value of the other number
          (<= val (ask other 'val))
        )
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;
; PYTHON INTEGER OBJECT
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class (py-int val)
  ; Parent: python number
  (parent (py-num val))
  ; Override integer check
  (method (int?) #t)
  ; Set type of object
  (method (type) 'py-int)
  (method (__int__) self)
  ; Convert integer to float
  (method (__float__)
    ; Create python float object
    (instantiate py-float (exact->inexact val))
  )
  ; Divide two numbers
  (method (__div__)
    (make-py-primitive
      '/
      (lambda 
        ; The other number
        (other)
        ; If the other number is an integer
        (if (ask other 'int?)
          ; Obtain only the integer part of the result of the division
	        (make-py-num (quotient val (ask other 'val)))
          ; Divide normally
	        (make-py-num (/ val (ask other 'val)))
        )
      )
    )
  )
  ; Obtain this number modulo another number
  (method (__mod__)
    (make-py-primitive
      '%
      (lambda 
        ; The other number
        (other)
        (make-py-num 
          ; Obtain the value of this number modulo de value of the other number
          (modulo val (ask other 'val))
        )
      )
    )
  )
  ; Obtain the integer part of an integer = the integer itself
  (method (__trunc__) self)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;
; PYTHON FLOAT OBJECT
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class (py-float val)
  ; Parent python number
  (parent (py-num val))
  ; Override the float check
  (method (float?) #t)
  ; Set the type of the object
  (method (type) 'py-float)
  ; Convert float to int
  (method (__int__) 
    ; Create python integer object
    ; from the integer part of the float
    (make-py-num (truncate val))
  )
  ; Convert to float
  (method (__float__) self)
  ; Obtain this number modulo another number
  (method (__mod__)
    (make-py-primitive
      '%
      (lambda 
        ; The other number
        (other) 
        (make-py-num 
          ; Obtain the value of this number modulo the value of the other number
          (modulo val (ask other 'val))
        )
      )
    )
  )
  ; Obtain the integer part of this number
  (method (__trunc__) (make-py-num (truncate val)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;
; PYTHON BOOLEAN OBJECT
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class (py-bool val)
  ; Parent: python object
  (parent (py-obj))
  ; Override boolean check
  (method (bool?) #t)
  ; Set type of object
  (method (type) 'py-bool)
  (method (true?) val)
  ; Check if two boolean objects are equal
  (method (__eq__)
    (make-py-primitive
      '==
      (lambda 
        ; The other object
        (other) 
        (make-py-bool 
          (and 
            ; Is the other object a python boolean object?
            (eq? (ask other 'type) 'py-bool)
            ; is the value of this boolean the same as the value of the 
            ; other boolean
	          (eq? val (ask other 'val))
          )
        )
      )
    )
  )
  ; Obtain string value
  (method (__str__)
    ; Return "True" of "False" based on the value of the boolean
    (make-py-string (if val "True" "False"))
  )
)

; Definition of boolean constants
(define *PY-TRUE* (instantiate py-bool #t))
(define *PY-FALSE* (instantiate py-bool #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; PYTHON LIST OBJECT
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class (py-list val)
  ; Parent: python object
  (parent (py-obj))
  ; Set the type of the object
  (method (type) 'py-list)
  ; Override the list check
  (method (list?) #t)
  ; Override the mutable check
  (method (mutable?) #t)
  (method (true?) (not (zero? (length val))))
  ; Convert to string
  (method (__str__)
    (make-py-string
     (string-append "["
		    (accumulate
		      (lambda 
            ; left: "]"
            ; right: element in the list of strings
            (left right)
            ; If the element is "]"
		        (if (equal? right "]")
              ; Then append another closing bracket (=left)
			        (string-append left right)
              ; Else keep separating by commas
			        (string-append left ", " right)
            )
          )
          ; Starting value
		     "]"
          ; This is the list of the converted values
		      (map 
            (lambda 
              ; For each element of the list
              (item)
              ; If it is a string
			        (if (eq? (ask item 'type) 'py-string)
                ; Enclose it between two special symbols
				        (string-append 
                  (string #\")
					        (ask item 'val)
					        (string #\")
                )
                ; Else obtain the string value of the object
				        (ask (ask item '__str__) 'val)
              )
            )
            ; Elements in the list
			      val
          )
        )
      )
    )
  )
  ; Obtain length of list: returns a python number object
  (method (__len__) (make-py-num (length val)))
  ; Reverse list
  (method (reverse)
    (make-py-primitive 
      ; name of operation
      'reverse
      ; procedure
		  (lambda 
        () 
        ; Use scheme's reverse to reverse the list
        (set! val (reverse val)) 
        ; Return nothing
        *NONE*
      )
    )
  )
  ; Return a copy of the list but reversed 
  (method (__reversed__)
    (make-py-primitive 
      ; name of operation
      'reversed 
      ; procedure
      (lambda 
        () 
        ; Create a new list object
        (make-py-list 
          ; Whose value is this reversed list
          (reverse val)
        )
      )
    )
  )
  ; Order the list
  (method (sort)
    (make-py-primitive
      ; Operation name
      'sort
      ; Procedure
      (lambda ()
        (set! val 
          ; Use scheme's sort procedure
          (sort 
            ; List to sort
            val 
            ; Rule to sort by
            (lambda 
              ; Given two values a and b
              (a b) 
              (ask 
                ; Get a's procedure to check if a is less than another object
                ; Will return a python boolean object
                (py-apply (ask a '__lt__) (list b)) 
                ; Check if a __lt__ is tru
                'true?
              )
            )
          )
        )
        ; Return nothing
        *NONE*
      )
    )
  )
  ; Create a copy of the list that is sorted
  (method (__sorted__)
    (make-py-primitive
      ; operation name
      'sorted
      ; procedure
      (lambda 
        () 
        ; Create python list object
        (make-py-list 
          ; Sort the list abiding by each object's __lt__ procedure 
          (sort 
            ; List to sort
            val 
            ; Predicate to sort by
            (lambda 
              (a b)
              (ask 
                (py-apply (ask a '__lt__) (list b))
               'true?
              )
            )
          )
        )
      )
    )
  )
  ; Create iterator for the list
  ; var-name: name for the variable that holds the value of the current element when we call next
  ; block: sequence of expression executed when we initialize iterator
  (method (__iter__ var-name block env)
    (define (iter seq)
      ; If the list is empty
      (if (null? seq)
        ; No iterator: just none object
	      *NONE*
        ; Else
	      (begin 
          ; Create a variable with the current name of the list
          ; in the current environment
          (define-variable! var-name (car seq) env)
		      (let 
            ; Evaluate the code block inside the __iter__ method
            ((result (eval-sequence block env)))
		        (cond 
              ; If the result is break, stop and return break object
              ((eq? result '*BREAK*) '*BREAK*)
              ; If the result is a list, and one element in the list
              ; is a return then return the result
		    	    ((and 
                 (pair? result) 
                 (eq? (car result) '*RETURN*)
                )
		    	      result
              )
              ; Else keep iterating
		    	    (else (iter (cdr seq)))
            )
          )
        )
      )
    )
    (let 
      ; Obtain result of the iterator
      ((result (iter val)))
      (cond 
        ; If the result is a pair of objects, and the first one is return, then return result
        ((and (pair? result) (eq? (car result) '*RETURN*)) result)
        ; If it is a break, return result
	      ((eq? result '*BREAK*) result)
        ; Else, return nothing
	      (else *NONE*))
    )
  )
  (method (__getitem__ index)
    ; Create a sublist
    (define (sublist seq start end)
      (cond 
        ; If the start index is bigger than 0
        ((> start 0) 
          (sublist 
            ; The we remove the first element of the list
            (cdr seq) 
            ; Update the start index
            (- start 1) 
            end
          )
        )
        ; If the end index equals 0, then the sublist to generate is empty
	      ((= end 0) '())
        ; Else 
	      (else 
          ; Create list recursively
          (cons 
            ; First element of current list
            (car seq) 
            (sublist 
              ; Rest of list
              (cdr seq) 
              start 
              ; Update end
              (- end 1)
            )
          )
        )
      )
    )
    (let 
      ; Obtain first index of slice [i, j]
      ((n (ask (car index) 'val)))
      ; If it is not an integer throw error
      (if (not (integer? n))
	      (py-error 
          "TypeError: list indices must be integers, not "
		      (objtype (car index))
        )
      )
      ; If n is not in [-len(val), len(val)], throw error
      ; note python allows for reverse indexing with negative indexes
      (if (not 
            (and 
              (>= n (- (length val))) 
              (< n (length val))
            )
          )
	      (py-error "IndexError: list index out of range: " n)
      )
      ; If n is negative, update its value to select the corresponding
      ; value but in reverse
      (if (< n 0) 
        (set! n (+ n (length val)))
      )
      ; If there is not second index
      (if (null? (cdr index))
        ; Simply return the value in index n
	      (list-ref val n)
        ; Else, apply the same logic than for the index n, onto index m
	      (let 
          ((m (ask (cadr index) 'val)))
	        (if (not (integer? m))
		        (py-error 
              "TypeError: list indices must be integers, not "
		        	(objtype (cadr index))
            )
          )
	        (if (not 
                (and 
                  (>= m (- (length val))) 
                  (< m (length val))
                )
              )
		          (py-error "IndexError: list index out of range: " m)
          )
	        (if (< m 0) 
            (set! m (+ m (length val)))
          )
          ; Three indexes not implemented
	        (if (not (null? (cddr index)))
		        (py-error "only one- and two- element slices implemented")
          )
          ; Create a list from the sublist between indexes n and m
	        (make-py-list (sublist val n m))
        )
      )
    )
  )
  (method (__setitem__ index item)
    ; Two indexes not supported
    (if (not (null? (cdr index))) 
      (py-error "Slice-assignment not implemented")
    )
    ; Obtain the index = first element of pair (index)
    (set! index (car index))

    (define (replace-item i seq)
      (cond 
        ; If the list is empty, return error, because any 
        ; integer is bigger than the max possible index 
        ((null? seq)
	        (py-error 
            "IndexError: list assignment out of range: "
		        (ask index 'val))
        )
        ; If the index equals zero
	      ((zero? i) 
          ; Replace value in current position in list to
          ; item
          (set-car! seq item) 
          ; Return none object
          *NONE*
        )
        ; Else keep going through the list until i = 0
	      (else 
          (replace-item 
            ; Update index
            (- i 1) 
            ; Rest of list
            (cdr seq)
          )
        )
      )
    )
    (let 
      ; Obtain the value of the index
      ((n (ask index 'val)))
      ; If the value of the index is not and integer -> error
      (if (not (integer? n)) 
        (py-error "TypeError: list indices must be integers")
      )
      ; If the value of the index is not in [-len(val), len(val)], then throw error
      ; note python supports negative index for reverse indexing
      (if (not 
            (and 
              (>= n (- (length val))) 
              (< n (length val))
            )
          )
	      (py-error "IndexError: list index out of range: " n)
      )
      ; If the value of the index is negative, set it to the corresponding
      ; positive value according to inverse indexing
      (if (< n 0) (set! n (+ n (length val))))
      ; Replace the item in the n index by the value given by item in the list "val"
      (replace-item n val))
  )
  ; Check if the list contains an object given by other
  (method (__contains__ other)
    (py-error "TodoError: Person A, Question 4")
  )
  ; Append two lists
  (method (append)
    (make-py-primitive 
      ; operation name
      'append
		  (lambda 
        (item)
        ; If the list is null
        (if (null? val)
          ; Simply return this item as a list
          (set! val (list item))
          ; Else use scheme's append! method to append two lists
          (append! val (list item))
        )
        ; Return none object
        *NONE*
      )
    )
  )
  ; Insert item into list
  (method (insert) ;; only setup to add to the front, for efficiency
    (make-py-primitive 
      ; operation name
      'insert
		  (lambda 
        ; given item to insert
        (item) 
        ; udpate the list to be a new list made up by 
        ; the item and the old list
        (set! val (cons item val)) 
        ; Return none object
        *NONE*
      )
    )
  )
  ; Pop an element from the left
  (method (popleft)
    (make-py-primitive
      ; operation name
      'popfront
      (lambda ()
        ; If the list is empty: error
        (if (null? val)
	        (py-error "IndexError: pop from empty list")
          ; Else 
	        (let 
            ; Get first element
            ((head (car val)))
            ; Delete first element from the list
	          (set! val (cdr val))
            ; Return the value we popped
	          head
          )
        )
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTRUCTORS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BOOLEAN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-py-bool val) (if (memq val '(|True| #t)) *PY-TRUE* *PY-FALSE*))
(define (negate-bool bool) (py-error "TodoError: Person B, Question 4"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-py-list val)
  (instantiate py-list val)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;table datatype. python dictionaries use this implementaiton of tables

;;;;;;;;;;;;;;;;;;;
;;; CONSTRUCTORS
;;;;;;;;;;;;;;;;;;;

; Create single table entry from a givenkey and value
(define (table-make-entry key val)
  (cons key val)
)

; Create a table from a list of keys and values
(define (table-make keys values)
  (define (iter keys values)
    ; If there are no keys return empty list
    (if (null? keys)
      '()
      ; Else
      (cons 
        ; Create a entry from the current key and value
        (table-make-entry (car keys) (car values))
        ; Keep generating entries from the rest of keys and values
        (iter (cdr keys) (cdr values))
      )
    )
  )
  ; If there are not the same number of keys and values: error
  (if (not (= (length keys) (length values)))
    (py-error "Dictionary error: Not same number of keys as values in dictionary")
    ; Else start creating entries
    (iter keys values)
  )
)

;;;;;;;;;;;;;;;;;;;
;;; SELECTORS
;;;;;;;;;;;;;;;;;;;

; First registry in table = first element in list=table
(define (table-first-entry table)
  (car table)
)

; First key of first registry in table
(define (table-first-entry-key table)
  ; Obtain first element (=key) of the first registry 
  ; in the table
  (car (table-first-entry table))
)

; Obtain list of keys in the table
(define (table-get-keys t)
  ; First element of every pair in the table is the key
  (map car t)
)
(define (table-get-vals t)
  ; Second element of every pair in the table is the value
  (map cdr t)
)

;;;;;;;;;;;;;;;;;;;
;;; PROCEDURES
;;;;;;;;;;;;;;;;;;;

;;return the entry (a pair) or #f if not in table
(define (table-contains-key? table key compare-proc)
  (cond
    ((null? table)
     #f)
    ((compare-proc (table-first-entry-key table) key)
     (table-first-entry table))
    (else (table-contains-key? (cdr table) key compare-proc))))
(define (table-add-key-val-pair table key val compare-proc)
  (cond
    ((null? table)
      (cons (table-make-entry key val) table))
    ((compare-proc (table-first-entry-key table) key)
      (cons (table-make-entry key val) (cdr table)))
    (else (cons (car table)
                (table-add-key-val-pair (cdr table) key val compare-proc)))))
(define (table-for-each table proc)
  (for-each (lambda (n)
              (proc (car n) (cdr n)))
            table))

(define (assert-or-error pred description)
  (if (not pred)
    (py-error description)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DICTIONARY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-py-dictionary pairs)
  (instantiate py-dictionary pairs))

(define-class (py-dictionary table)
  (parent (py-obj))
  (instance-vars (comp-proc (lambda (x y)
                             (and (eq? (ask x 'type) (ask y 'type))
                                  (eq? (py-apply (ask x '__eq__) (list y)) *PY-TRUE*)))))
  (initialize
    (for-each (lambda (x) (ask self 'isValidKey (car x)))
              table))
  (method (type) 'py-dictionary)
  (method (dictionary?) #t)
  (method (mutable?) #t)
  (method (true?) (not (null? table)))
  (method (isValidKey key)
    (assert-or-error (not (ask key 'mutable?)) "Dictionary Error: All keys to the dictionary must be IMMUTABLE objects"))
  (method (__str__)
    (make-py-string
         (string-append "{\n"
            (accumulate (lambda (left right)
                          (if (equal? right "}")
                            (string-append "  " left "\n" right)
                            (string-append "  " left ",\n" right)))
                        "}"
                        (map (lambda (item)
                               (let ((key (car item))
                                     (val (cdr item)))
                                 (if (eq? (ask key 'type) 'py-string)
                                   (set! key (string-append (string #\") (ask key 'val) (string #\")))
                                   (set! key (string-append (ask (ask key '__str__) 'val))))
                                 (if (eq? (ask val 'type) 'py-string)
                                   (set! val (string-append (string #\") (ask val 'val) (string #\")))
                                   (set! val (string-append (ask (ask val '__str__) 'val))))
                                 (string-append key " : " val )))
                             table)))))
  (method (__setitem__ py-obj-key py-obj-val)
    (ask self 'isValidKey py-obj-key)
    (set! table (table-add-key-val-pair table py-obj-key py-obj-val comp-proc))
    *NONE*)
  (method (__getitem__ key)
    (let ((v (table-contains-key? table key comp-proc)))
      (if v
        (cdr v) ;return the value
        (py-error (string-append "Dictionary doesn't contain the key: " (ask (ask key '__str__) 'val))))))
  (method (__contains__ key)
    (let ((v (table-contains-key? table key comp-proc)))
      (if v
        (make-py-bool #t)
        (make-py-bool #f))))
  (method (__keys__)
    (make-py-list (table-get-keys table)))
  (method (__vals__)
    (make-py-list (table-get-vals table)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;
;; PROCEDURE
;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-py-proc name params body env)
  (instantiate py-proc name params body env))
(define-class (py-proc name params body env)
  (parent (py-obj))
  (instance-vars (num-params #f))
  (initialize (set! num-params (length params)))
  (method (type) 'py-proc)
  (method (procedure?) #t)
  (method (__str__)
    (make-py-string
     (apply string-append (list "<function " (symbol->string name) ">"))))
  (method (__call__ args)
    (define (execute block env)
      (if (null? block)
	  *NONE*
	  (let ((line-obj (make-line-obj (car block))))
	    (if (and (not (empty? line-obj)) ;; check for tail call
		     (eq? (ask line-obj 'peek) 'return))
		(begin (ask line-obj 'next) ;; discard return token
		       (py-eval line-obj env))
		(let ((val (py-eval line-obj env)))
		  (if (and (pair? val) (eq? (car val) '*RETURN*))
		      (cdr val)
		      (execute (cdr block) env)))))))
    (let ((num-args (length args)))
      (cond ((> num-args num-params)
	     (py-error "TypeError: Too many arguments to " name))
	    ((< num-args num-params)
	     (py-error "TypeError: Too few arguments to " name))
	    (else (execute body (extend-environment params args env))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIMITIVE
;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-py-primitive name proc)
  (instantiate py-primitive name proc))
(define-class (py-primitive name proc)
  (parent (py-obj))
  (method (type) 'py-proc)
  (method (primitive?) #t)
  (method (__str__)
    (make-py-string (apply string-append
			   (list "<method " (symbol->string name) ">"))))
  (method (__call__ args) (apply proc args))
  )

(define (square x) (* x x)) ;; math helper
(define-class (math)
  (parent (py-obj))
  (method (__str__) (make-py-string "<built-in module 'math'>"))
  ;; Mathematical constants
  (class-vars (pi (make-py-num (* 4 (atan 1))))
	      (e (make-py-num (exp 1)))
	      (phi (make-py-num (/ (+ 1 (sqrt 5)) 2))))
  ;; Number-theoretic functions
  (method (ceil)
    (make-py-primitive 'ceil
		       (lambda (num) (make-py-num (ceiling (ask num 'val))))))
  (method (fabs)
    (make-py-primitive 'fabs
		       (lambda (num)
			 (let ((val (ask num 'val)))
			   (if (< val 0)
			       (make-py-num (- val))
			       num)))))
  (method (factorial)
    (make-py-primitive
     'factorial
     (lambda (num)
       (define (fact-iter n p)
	 (if (= n 0)
	     p
	     (fact-iter (- n 1) (* p n))))
       (if (or (not (ask num 'int?))
	       (< (ask num 'val) 0))
	   (py-error "ValueError: factorial() not defined for negative values")
	   (fact-iter (ask num 'val) 1)))))
  (method (floor)
    (make-py-primitive 'floor
		       (lambda (num) (make-py-num (floor (ask num 'val))))))
  (method (trunc)
    (make-py-primitive 'trunc
		       (lambda (num) (ask num '__trunc__))))
  ;; Power and logarithmic functions
  (method (exp)
    (make-py-primitive 'exp
		       (lambda (num) (make-py-num (exp (ask num 'val))))))
  (method (log)
    (make-py-primitive
     'log
     (lambda (num . base)
       (if (null? base)
	   (make-py-num (log (ask num 'val)))
	   (make-py-num (/ (log (ask num 'val))
			   (log (ask (car base) 'val))))))))
  (method (log10)
    (make-py-primitive 'log10
		       (lambda (num)
			 (make-py-num (/ (log (ask num 'val)) (log 10))))))
  (method (pow)
    (make-py-primitive 'pow
		       (lambda (x y)
			 (make-py-num (expt (ask x 'val) (ask y 'val))))))
  (method (sqrt)
    (make-py-primitive 'sqrt
		       (lambda (num) (make-py-num (sqrt (ask num 'val))))))
  ;; Trigonometric functions
  (method (acos)
    (make-py-primitive 'acos
		       (lambda (num) (make-py-num (acos (ask num 'val))))))
  (method (asin)
    (make-py-primitive 'asin
		       (lambda (num) (make-py-num (asin (ask num 'val))))))
  (method (atan)
    (make-py-primitive 'atan
		       (lambda (num) (make-py-num (atan (ask num 'val))))))
  (method (atan2)
    (make-py-primitive
     'atan2
     (lambda (x y) (make-py-num (atan (ask x 'val) (ask y 'val))))))
  (method (cos)
    (make-py-primitive
     'cos
     (lambda (num) (make-py-num (cos (ask num 'val))))))
  (method (hypot)
    (make-py-primitive
     'hypot
     (lambda (x y) (make-py-num (sqrt (+ (square (ask x 'val))
					 (square (ask y 'val))))))))
  (method (sin)
    (make-py-primitive
     'sin
     (lambda (num) (make-py-num (sin (ask num 'val))))))
  (method (tan)
    (make-py-primitive
     'tan
     (lambda (num) (make-py-num (tan (ask num 'val))))))
  ;; Angular conversion functions
  (method (degrees)
    (make-py-primitive
     'degrees
     (lambda (num) (make-py-num (* 180 (/ (ask num 'val) pi))))))
  (method (radians)
    (make-py-primitive
     'radians
     (lambda (num) (make-py-num (* pi (/ (ask num 'val) 180))))))
  ;; Hyperbolic functions:
  (method (asinh)
    (make-py-primitive
     'asinh
     (lambda (num)
       (make-py-num (log (+ (ask num 'val)
			    (sqrt (1+ (square (ask num 'val))))))))))
  (method (acosh)
    (make-py-primitive
     'acosh
     (lambda (num)
       (make-py-num (log (+ (ask num 'val)
			    (sqrt (- (square (ask num 'val)) 1))))))))
  (method (atanh)
    (make-py-primitive
     'atanh
     (lambda (num)
       (make-py-num (* .5 (log (/ (+ 1 (ask num 'val))
				  (- 1 (ask num 'val)))))))))
  (method (sinh)
    (make-py-primitive 'sinh
		       (lambda (num)
			 (make-py-num
			  (* .5 (- (exp (ask num 'val))
				   (exp (- (ask num 'val)))))))))
  (method (cosh)
    (make-py-primitive 'cosh
		       (lambda (num)
			 (make-py-num (* .5 (+ (exp (ask num 'val))
					       (exp (- (ask num 'val)))))))))
  (method (tanh)
    (make-py-primitive 'tanh
		       (lambda (num)
			 (make-py-num
			  (/ (- (exp (* 2 (ask num 'val))) 1)
			     (+ (exp (* 2 (ask num 'val))) 1))))))
  )

(define-class (py-random)
  (parent (py-obj))
  (method (__str__) (make-py-string "<built-in module 'random'>"))
  (method (randrange)
    (make-py-primitive
     'randrange
     (lambda args
       (cond ((null? args)
	      (py-error "TypeError: Too few arguments to randrange"))
	     ((null? (cdr args)) (make-py-num (random (ask (car args) 'val))))
	     ((null? (cddr args))
	      (let ((start (ask (car args) 'val))
		    (end (-1+ (ask (cdr args 'val)))))
		(make-py-num (+ start (random (- end start))))))
	     ((null? (cdddr args))
	      (let ((start (ask (car args) 'val))
		    (end (ask (cadr args) 'val))
		    (step (ask (caddr args) 'val)))
		(set! end (- end (quotient (- end start) step)))
		(make-py-num (+ start (* step (random (- end start)))))))
	     (else (py-error "TypeError: Too many arguments to randrange"))))))
  (method (randint)
    (make-py-primitive
     'randint
     (lambda (a b)
       (make-py-num (+ (ask a 'val) (random (1+ (ask b 'val))))))))
  (method (choice)
    (make-py-primitive
     'choice
     (lambda (seq)
       (let ((len (ask (ask seq '__len__) 'val)))
	 (ask seq '__getitem__ (make-py-num (random len)))))))
  (method (random)
    (make-py-primitive 'random
		       (lambda () (make-py-num (/ (random 4000000000))))))
  )


(define (define-primitives!)
  (define (add-prim name proc)
    (define-variable! name (make-py-primitive name proc) the-global-environment))
  (define-variable! 'math (instantiate math) the-global-environment)
  (define-variable! 'random (instantiate py-random) the-global-environment)
  (add-prim 'abs
	    (lambda (num)
	      (if (< (ask num 'val) 0) (make-py-num (- (ask num 'val))) val)))
  (add-prim 'bin
	    (lambda (int)
	      (let ((n (ask int 'val)))
		(let ((str (number->string n 2)))
		  (make-py-string
		   (if (< n 0)
		       (string-append "-0b"
				      (substring str 1 (string-length str)))
		       (string-append "0b" str)))))))
  (add-prim 'bool (lambda arg (if (null? arg)
				  (make-py-bool #f)
				  (make-py-bool (ask (car arg) 'true?)))))
  (add-prim 'chr (lambda (num)
		   (make-py-string (string (integer->char (ask num 'val))))))
  (add-prim 'cmp (lambda (x y)
		   (cond ((py-apply (ask x '__lt__) (list y)) -1)
			 ((py-apply (ask x '__gt__) (list y)) 1)
			 (else 0))))
  (add-prim 'divmod
	    (lambda (a b)
	      (make-py-list (list (py-apply (ask a '__div__) (list b))
				  (py-apply (ask b '__mod__) (list b))))))
  (add-prim 'float (lambda (num) (ask num '__float__)))
  (add-prim 'hex
            (lambda (int)
              (let ((n (ask int 'val)))
                (let ((str (number->string n 16)))
                  (make-py-string
                   (if (< n 0)
                       (string-append "-0x"
                                      (substring str 1 (string-length str)))
                       (string-append "0x" str)))))))
  (add-prim 'int (lambda (num) (ask num '__int__)))
  (add-prim 'len (lambda (seq) (ask seq '__len__)))
  (add-prim 'oct
	    (lambda (int)
              (let ((n (ask int 'val)))
                (let ((str (number->string n 8)))
                  (make-py-string
                   (if (< n 0)
                       (string-append "-0"
                                      (substring str 1 (string-length str)))
                       (string-append "0" str)))))))
  (add-prim
   'ord
   (lambda (char)
     (if (not (= (ask (ask char '__len__) 'val) 1))
	 (py-error "TypeError: Expected string of length 1")
	 (make-py-num (char->integer (string-ref (ask char 'val) 0))))))
  (add-prim
   'pow
   (lambda (base pow . mod)
     (define (mexpt b n m)
       (cond ((= n 0) 1)
             ((even? n) (modulo (mexpt (modulo (* b b) m) (/ n 2) m) m))
             (else (modulo (* b (modulo (mexpt (modulo (* b b) m)
                                               (quotient n 2) m)
                                        m))
                           m))))
     (if (null? mod)
         (py-apply (ask base '__pow__) (list pow))
         (make-py-num (mexpt (ask base 'val)
                             (ask pow 'val)
                             (ask (car mod) 'val))))))
  (add-prim
   'range
   (lambda (num . other-args)
     (define (make-range low cur step so-far)
       (if (< cur low)
           (make-py-list so-far)
           (make-range low (- cur step) step (cons (make-py-num cur) so-far))))
     (cond ((null? other-args) (make-range 0 (-1+ (ask num 'val)) 1 '()))
           ((null? (cdr other-args))
            (make-range (ask num 'val) (-1+ (ask (car other-args) 'val)) 1 '()))
           (else
            (let ((start (ask num 'val))
                  (end (ask (car other-args) 'val))
                  (step (ask (cadr other-args) 'val)))
              (cond ((= step 0)
                     (py-error "ValueError: range() step argument cannot be zero"))
                    ((> step 0)
		     (let ((last-value (- end (modulo (- end start) step))))
		       (make-range start last-value step '())))
                    (else
                     (let ((result (make-range (1+ end) start (- step) '())))
		       (py-apply (ask result 'reverse) '())
                       result))))))))
  (add-prim 'raw_input
	    (lambda arg
	      (define (read-line so-far)
		(let ((char (read-char)))
		  (if (or (eof-object? char)
			  (eq? char #\newline))
		      so-far
		      (read-line (string-append so-far (string char))))))
	      (if (not (null? arg))
		  (begin (display (ask (ask (car arg) '__str__) 'val)) (flush)))
	      (make-py-string (read-line ""))))
  (add-prim 'reversed
	    (lambda (obj) (ask (ask obj '__reversed__) '__call__ '())))
  (add-prim 'sorted
	    (lambda (obj) (ask (ask obj '__sorted__) '__call__ '())))
  (add-prim 'str (lambda (obj) (ask obj '__str__)))
  (add-prim 'type (lambda (obj) (make-py-type (objtype obj))))
  )

(define (make-py-type type)
  (instantiate py-type type))
(define-class (py-type val)
  (parent (py-obj))
  (method (__str__) (make-py-string val)))

(define (objtype obj)
  (cdr (assq (ask obj 'type)
	     '((*NONE* . "<type 'NoneType'>")
	       (py-int . "<type 'int'>")
	       (py-float . "<type 'float'>")
	       (py-bool . "<type 'bool'>")
	       (py-list . "<type 'list'>")
         (py-dictionary . "<type 'dictionary'>")
	       (py-string . "<type 'str'>")
	       (py-proc . "<type 'function'>")))))

(define (py-list? val) (equal? (ask val 'type) 'py-list))
(define (py-dict? val) (equal? (ask val 'type) 'py-dictionary))
