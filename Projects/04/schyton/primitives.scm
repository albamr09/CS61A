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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (define (loop lst)
      (cond
        ((null? lst) *PY-FALSE*)
        ((ask (py-apply (ask (car lst) '__eq__) (list other)) 'true?)
          *PY-TRUE*
        )
        (else
          (loop (cdr lst))
        )
      )
    )
    (loop (ask self 'val))
    ; (ask (py-apply (ask obj '__eq__) (list other)) 'true?)
    ;(py-error "TodoError: Person A, Question 4")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    ; If the table is empty, it contains no element
    ((null? table) #f)
    ; Check if the key we are searching and the current key of the table we are analysing are equal
    ((compare-proc (table-first-entry-key table) key)
      ; If so, return entry
      (table-first-entry table)
    )
    ; Else, keep searching
    (else 
      (table-contains-key? 
        ; Rest of table
        (cdr table) 
        key 
        ; Comparing procedure
        compare-proc)
    )
  )
)

(define (table-add-key-val-pair table key val compare-proc)
  (cond
    ; If the table is empty, simply add entry to table and return it
    ((null? table) 
      (cons (table-make-entry key val) table)
    )
    ; Check if the key we are inserting and the current key of the table we are analysing are equal
    ((compare-proc (table-first-entry-key table) key)
      (cons 
        ; Override the current linked to the key
        (table-make-entry key val) 
        ; Concatenate the rest of the table
        (cdr table)
      )
    )
    ; Else keep going through the table, until it is emtpy or the key is found
    (else 
      ; Re-construct table
      (cons 
        ; Current entry
        (car table)
        ; Rest of entries
        (table-add-key-val-pair (cdr table) key val compare-proc)
      )
    )
  )
)

; Apply procedure to every entry of the table
(define (table-for-each table proc)
  (for-each 
    ; n: entry of table
    (lambda (n)
      ; Apply procedure to key and value
      (proc (car n) (cdr n))
    )
    ; Go through table
    table
  )
)

(define (assert-or-error pred description)
  ; Check if predicate is satisfied
  (if (not pred)
    ; If not throw error
    (py-error description)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DICTIONARY PYTHON OBJECT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (py-dictionary table)
  ; Parent: python object
  (parent (py-obj))
  (instance-vars 
    ; Define procedure to compare two entries of a dictionary 
    (comp-proc 
      (lambda (x y)
        (and 
          ; The entries have the same type
          (eq? (ask x 'type) (ask y 'type))
          ; The entries have the same value
          (eq? (py-apply (ask x '__eq__) (list y)) *PY-TRUE*)
        )
      )
    )
  )
  ; When creating dictionary, check if every key is valid
  (initialize
    (for-each 
      ; Pair: key - value
      (lambda (x) 
        ; Check if the current key is value
        (ask self 'isValidKey (car x))
      )
      ; List of pairs: key-value
      table
    )
  )
  ; Override the type
  (method (type) 'py-dictionary)
  ; Override dictionary check
  (method (dictionary?) #t)
  ; Override mutable check
  (method (mutable?) #t)
  ; Override true check
  (method (true?) (not (null? table)))
  ; Check if a key is valid
  (method (isValidKey key)
    (assert-or-error 
      ; If key is mutable, throw error
      (not (ask key 'mutable?)) 
      "Dictionary Error: All keys to the dictionary must be IMMUTABLE objects"
    )
  )
  ; To string method
  (method (__str__)
    (make-py-string
      (string-append "{\n"
        (accumulate 
          (lambda 
              (left right)
              (if (equal? right "}")
                (string-append "  " left "\n" right)
                (string-append "  " left ",\n" right)
              )
          )
          "}"
          (map 
            (lambda (item)
              (let 
                (
                  (key (car item))
                  (val (cdr item))
                )
                (if (eq? (ask key 'type) 'py-string)
                  (set! key (string-append (string #\") (ask key 'val) (string #\")))
                  (set! key (string-append (ask (ask key '__str__) 'val)))
                )
                (if (eq? (ask val 'type) 'py-string)
                  (set! val (string-append (string #\") (ask val 'val) (string #\")))
                  (set! val (string-append (ask (ask val '__str__) 'val)))
                )
                (string-append key " : " val ))
            )
            table
          )
        )
      )
    )
  )
  ; Update/Save pair key - value
  (method (__setitem__ py-obj-key py-obj-val)
    ; If the key is valid
    (ask self 'isValidKey py-obj-key)
    ; Update the table with the pair key - value
    (set! 
      table 
      ; Add entry to dictionary
      (table-add-key-val-pair 
        table 
        py-obj-key 
        py-obj-val 
        ; Procedure to compare if two entries in the dictionary are equal
        comp-proc
      )
    )
    ; Return none object
    *NONE*
  )
  ; Obtain value in dictionary from key
  (method (__getitem__ key)
    (let 
      ; Search for the key in the dictionary (table)
      ((v (table-contains-key? table key comp-proc)))
      ; If it exists, return value
      (if v
        (cdr v) ;return the value
        ; Else throw error
        (py-error 
          (string-append 
            "Dictionary doesn't contain the key: " (ask (ask key '__str__) 'val)
          )
        )
      )
    )
  )
  ; Check if the dictionary contains a given key
  (method (__contains__ key)
    (let 
      ; Search the key in the dictionary (table)
      ((v (table-contains-key? table key comp-proc)))
      ; If it exists
      (if v
        ; Return boolean python object with true value
        (make-py-bool #t)
        ; Return boolean python object with false value
        (make-py-bool #f)
      )
    )
  )

  ; Obtain all keys in the dictionary
  (method (__keys__)
    ; Return list python object with the keys of the dictionary (table)
    (make-py-list (table-get-keys table))
  )
  ; Obtain all values in the dictionary
  (method (__vals__)
    ; Return list python object with the values of the dictionary (table)
    (make-py-list (table-get-vals table))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; PROCEDURE
;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (py-proc name params body env)
  ; Parent: python object
  (parent (py-obj))
  ; Create attributes: no parameters
  (instance-vars (num-params #f))
  ; Update hte number of parameters
  (initialize (set! num-params (length params)))
  ; Override object type
  (method (type) 'py-proc)
  ; Override procedure check
  (method (procedure?) #t)
  ; To String method
  (method (__str__)
    (make-py-string
     (apply 
      string-append 
      (list "<function " (symbol->string name) ">")
      )
    )
  )
  ; Call method to apply procedure
  (method (__call__ args)
    (define (execute block env)
      ; If the set of declarations is null
      (if (null? block)
        ; Return none object
	      *NONE*
        ; Else
	      (let 
          ; Obtain first line to execute
          ((line-obj (make-line-obj (car block))))
	        (if (and 
                (not (empty? line-obj)) ;; check for tail call
		            (eq? (ask line-obj 'peek) 'return)
            )
            ; If there is a return in the line
		        (begin 
              (ask line-obj 'next) ;; discard return token
              ; Return the result of evaluating the line
		          (py-eval line-obj env)
            )
            ; If not
		        (let 
              ; Obtain value after result of evaluating the line
              ((val (py-eval line-obj env)))
              ; If the result is a pair that contains a return
		          (if (and 
                    (pair? val) 
                    (eq? (car val) '*RETURN*)
                  )
                ; Then return value of result
		            (cdr val)
                ; Else, keep executing next lines
		            (execute (cdr block) env)
              )
            )
          )
        )
      )
    )
    (let 
      ; Obtain the number of arguments
      ((num-args (length args)))
      (cond 
        ; If the number of arguments is larger than the number of parameters: error
        ((> num-args num-params)
	        (py-error "TypeError: Too many arguments to " name)
        )
        ; If the number of arguments is lower than the number of parameters: error
	      ((< num-args num-params)
	        (py-error "TypeError: Too few arguments to " name)
        )
        ; Else apply procedure
	      (else 
          (execute
            ; Set of declarations to execute
            body 
            ; Create new frame that binds parameters and 
            ; arguments whose enclosing environment is env
            (extend-environment params args env)
          )
        )
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIMITIVE
;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (py-primitive name proc)
  ; Parent: python object
  (parent (py-obj))
  ; Override object type
  (method (type) 'py-proc)
  ; Override primitive check
  (method (primitive?) #t)
  ; To String method
  (method (__str__)
    (make-py-string 
      (apply 
        string-append
        (list "<method " (symbol->string name) ">")
      )
    )
  )
  ; Call procedure to apply primitive procedure
  (method (__call__ args) (apply proc args))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; TYPE
;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (py-type val)
  ; Parent: python object
  (parent (py-obj))
  ; To string method
  (method (__str__) (make-py-string val))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; LINE OBJECT
;;;;;;;;;;;;;;;;;;;;;;;;

;; A class to represent a sequence of tokens to be used by the evaluator.
(define-class (line-obj indentation tokens)
  (method (empty?)
	  (null? tokens)
  )
  (method (exit?)
	  (member tokens '((exit |(| |)|) (quit |(| |)|)))
  )
  (method (peek)
	  (car tokens)
  )
  (method (push token)
	  (set! tokens (cons token tokens))
  )
  (method (next)
	  (let ((token (car tokens)))
	    (set! tokens (cdr tokens))
	    token
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define python objects

(define (objtype obj)
  (cdr 
    (assq 
      (ask obj 'type)
	    '(
        (*NONE* . "<type 'NoneType'>")
	      (py-int . "<type 'int'>")
	      (py-float . "<type 'float'>")
	      (py-bool . "<type 'bool'>")
	      (py-list . "<type 'list'>")
        (py-dictionary . "<type 'dictionary'>")
	      (py-string . "<type 'str'>")
	      (py-proc . "<type 'function'>")
      )
    )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructors for objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Boolean
(define (make-py-bool val) 
  (if (memq val '(|True| #t)) *PY-TRUE* *PY-FALSE*))

; List
(define (make-py-list val)
  (instantiate py-list val)
)

; Dictionary
(define (make-py-dictionary pairs)
  (instantiate py-dictionary pairs)
)

; Procedure
(define (make-py-proc name params body env)
  (instantiate py-proc name params body env)
)

; Type
(define (make-py-type type)
  (instantiate py-type type)
)

; Primitives
(define (make-py-primitive name proc)
  (instantiate py-primitive name proc)
)

; Strings
(define (make-py-string str)
  (instantiate py-string str)
)

; Numeric object
(define (make-py-num num)
  (if (exact? num)
      (instantiate py-int num)
      (instantiate py-float num)))

; Line object
(define (make-line-obj line)
  (instantiate line-obj (indentation line) (tokens line))
)

; Definition of boolean constants
(define *PY-TRUE* (instantiate py-bool #t))
(define *PY-FALSE* (instantiate py-bool #f))

; This procedure takes in a PY-BOOL object and returns the
; PY-BOOL object with the opposite value.
(define (negate-bool bool) 
  (make-py-bool (not (ask bool 'val)))
)
