. include "matter"

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

def pair (a b) (list a b)

def takeWhileCore (p out in)
	if (empty? in)
		pair out in
		do
			let c (head in)
			let rest (tail in)
			if (p c)
				takeWhileCore f (conj c out) rest
				pair out in


			