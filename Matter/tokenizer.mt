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
			def c (head in)
			def rest (tail in)
			def r (f c)
			if (! r)
				list out in
				takeWhileCore f (conj c out) rest


			