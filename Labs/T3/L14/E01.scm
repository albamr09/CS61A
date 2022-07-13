; Suppose that Peter, Paul, and Mary share a joint bank account that initially contains 
; $100. Concurrently, Peter deposits $10, Paul withdraws $20, and Mary
; withdraws half the money in the account, by executing the following commands:

; Peter: (set! balance (+ balance 10))
; Paul: (set! balance (- balance 20))
; Mary: (set! balance (- balance (/ balance 2)))

; a. List all the different possible values for balance after
; these three transactions have been completed, assuming that the 
; banking system forces the three processes to run sequentially in some order.

; What are some other values that could be produced
; if the system allows the processes to be interleaved?
; Draw timing diagrams like the one in Figure 3.29 to
; explain how these values can occur.

1. First Peter
  2. Second Paul
    3. Third Mary
  2. Second Mary
    3. Third Paul

1. First Paul
  2. Second Peter
    3. Third Mary
  2. Second Mary
    3. Third Peter

1. First Mary
  2. Second Peter
    3. Third Paul
  2. Second Paul
    3. Third Peter
