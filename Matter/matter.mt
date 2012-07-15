;
; runtime functions
;

def use (. use)

def list (. list)
def cons (. cons)

def first (. first)
def next (. next)
def empty? (. empty?)

def = (. =)

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
