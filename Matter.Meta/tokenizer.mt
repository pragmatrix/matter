; tokens:
; :number int
; :string string
; :boolean bool
; :keywoard string

; :symbol string
; :begin
; :end
; :quote

; matter.mt candidate
; or lang.mt?

def take-while (p in)
	first
		take-while-core p () in

def take-while-core (p out in)
	if (empty? in)
		pair out in
		do
			let c (head in)
			let rest (tail in)
			if (p c)
				take-while-core f (conj c out) rest
				pair out in
