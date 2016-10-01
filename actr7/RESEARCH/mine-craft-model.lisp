; MINECRAFT MODEL
; Goal: Just runs randomly and do some random stuff

(clear-all)

(load "~/quicklisp/setup.lisp")
(load "C:/Users/Son Pham/Desktop/actr7/RESEARCH/server-client.lisp")

; -----
; ACT-R
; -----
(define-model minecraft-random
(sgp :esc t :lf .05 :trace-detail high)

; -----------
; CHUNK-TYPES
; -----------
(chunk-type move direction)
;(chunk-type turn amount)
;(chunk-type pitch amount)
;(chunk-type jump bool)
;(chunk-type crouch bool)
;(chunk-type attach bool)
;(chunk-type use bool)
(chunk-type minecraft-goal state dice num-steps)

; ------
; CHUNKS
; ------

(add-dm
	(begin isa chunk)
	(do-action isa chunk)
	(reduce-count)

	(g1 isa minecraft-goal state begin dice 0 num-steps 1000)
)

(goal-focus g1)

; -----------
; PRODUCTIONS
; -----------

(p start
	=goal>
		ISA		minecraft-goal
		state	begin
		!bind!  =value (random 4)

==>
	=goal>
		dice	=value
		state	do-action
)

; These are all the action productions of the model
; -------------------------------------------------

(p action-up
	=goal>
		ISA			minecraft-goal
		state		do-action
		dice		0
		- num-steps 0
	?manual>
		state	free
==>
	=goal>
		state	reduce-count
	+manual>
		cmd		press-key
		key		up
	!output!	"W"
	!bind!		=value (create-client-W 12330)
)

(p action-down
	=goal>
		ISA		minecraft-goal
		state	do-action
		dice	1
	?manual>
		state	free
==>
	=goal>
		state	reduce-count
	+manual>
		cmd		press-key
		key		down
	!output!	"S"
	!bind!		=value (create-client-S 12330)
)

(p action-left
	=goal>
		ISA		minecraft-goal
		state	do-action
		dice	2
	?manual>
		state	free
==>
	=goal>
		state	reduce-count
	+manual>
		cmd		press-key
		key		left
	!output!	"A"
	!bind!		=value (create-client-A 12330)
)

(p action-right
	=goal>
		ISA		minecraft-goal
		state	do-action
		dice	3
	?manual>
		state	free
==>
	=goal>
		state	reduce-count
	+manual>
		cmd		press-key
		key		"right"
	!output! 	"D"
	!bind!		=value (create-client-D 12330)
)

; Reduce the count
; ----------------
(p reduce-count
	=goal>
		ISA			minecraft-goal
		state		reduce-count
		num-steps 	=num1
==>
	=goal>
		num-steps	(- =num1 1)
		state		begin) 
)