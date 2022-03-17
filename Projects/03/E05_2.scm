;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B05_2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Invent the laptop class. 
; 1. A laptop has one instantiation variable, its name.
; 2. A laptop is a thing that has two extra methods: 
;   - Connect, with a password as argument, sends a connect message to the place 
;     where the laptop is. If the password is wrong, return an error.
; 3. Surf, with a URL text string as argument, sends a surf message to the place 
;   where it is.
; 4. Whenever a laptop enters a new hotspot, the user must ask to connect to that hotspot's network; 
; 5. When the laptop leaves the hotspot, it must automatically be disconnected from the network.
; 6. If it's in a place other than a hotspot, the surf message won't be understood; 
; 7. If it's in a hotspot but not connected, return an error.

