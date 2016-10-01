; Copy the demo2.lisp to understand it better

; ---------
; VARIABLES
; ---------
(defvar *response* nil)
(defvar *model* nil)

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
	(setf *response* (string key))
	(clear-exp-window)
	(when *model*
		(proc-display)
	)
)


; do-demo2 excutes the thing
(defun do-demo2 (&optional who)

	(reset)

	(if (eq who 'human)
		; if
		(setf *model* nil)
		; else
		(setf *mode* t))
	
	(let* ((lis (permute-list '("B" "C" "D" "F" "G" "H" 
                              "J" "K" "L" "M" "N" "P" 
                              "Q" "R" "S" "T" "V" "W" 
                              "X" "Y" "Z")))
			(text1 (first lis))
			(window (open-exp-window "Letter Recognition")))

			(add-text-to-exp-window :text text1 :x 125 :y 150)

			(setf *response* nil)

			(if *model*
				(progn
					(install-device window)
					(proc-display)
					(run 10 :real-time t))

				(while (null *response*)
					(allow-event-manager window)))
			
			*response*)) ; Return response

; ------------
; ACT-R BEGINS
; ------------

(clear-all)

(define-model demo2)

(sgp :seed (123456 0))
(sgp :v t :needs-mouse nil :show-focus t :trace-detail high)


; CHUNK-TYPES

(chunk-type read-letters state)
(chunk-type array letter)

; CHUNKS

(add-dm
	(start isa chunk) (attend isa chunk)
	(response isa chunk) (done isa chunk)
	(goal isa read-letters state start)
)

; PRODUCTIONS

(P find-unattended-letter
	=goal>
		ISA		read-letters
		state	start
==>
	+visual-location>
		:attended nil
	=goal>
		state	find-location
)

(P attend-letter
	=goal>
		ISA		read-letters
		state	find-location
	=visual-location>
	?visual>
		state	free

==>
	+visual>
		cmd			move-attention
		screen-pos 	=visual-location
	=goal>
		state		attend
)

(P encode-letter
	=goal>
		ISA		read-letters
		state	attendd
	=visual>
		value	=letter
	?imaginal>
		state	free
==>
	=goal>
		state	respond
	+imaginal
		ISA		array
		letter	=letter
)

(P respond
	=goal>
		ISA		read-letters
		state	respond
	=imaginal>
		ISA		array
		letter	=letter
	?manual>
		state	free
==>
	=goal>
		state	done
	+manual>
		cmd		press-key
		key		=letter
)

(goal-focus goal)

)