;
; runtime functions
;

def list 
	. list

def first 
	. first

def next 
	. next

def = 
	. =

def use
	. use

;
; pair
;

def pair (f s)
	list f s

def second (l)
	first (next l)

;
; logic
;

def not (v)
	if v
		false
		true

def not= (v)
	not (= v)
