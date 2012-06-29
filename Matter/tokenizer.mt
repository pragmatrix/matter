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

def takeWhileCore (f out in)
	if (empty? in)
		list out in
		do
			let c (head in)
			let rest (tail in)
			let r (f c)
			if (! r)
				list out in
				takeWhileCore f (conj c out) rest


			