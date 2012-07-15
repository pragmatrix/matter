; tokens:
; :number int
; :string string
; :boolean bool
; :keyword string

; :symbol string
; :begin
; :end
; :quote

def take-while (pred in)
	first
		take-while-core pred () in

def take-while-core (pred out in)
	if (empty? in)
		pair out in
		do
			let c (first in)
			let rest (next in)
			if (pred c)
				take-while-core pred (cons c out) rest
				pair out in
