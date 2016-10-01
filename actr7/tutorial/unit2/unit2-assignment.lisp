; Clear-existing models
(clear-all)

;-------------------------
; VARIABLES AND FUNCTIONS
;-------------------------

(defvar *response* nil)

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (setf *response* (string key))
)

(defun do-unit2 (&optional who)
  
  (reset)
  
  (let* ((letters (permute-list '("B" "C" "D" "F" "G" "H" "J" "K"  
                                  "L" "M" "N" "P" "Q" "R" "S" "T"  
                                  "V" "W" "X" "Y" "Z")))
         (target (first letters))
         (foil (second letters))
         (window (open-exp-window "Letter difference"))
		 
		 ; Set 3 texts as foil
         (text1 foil)
         (text2 foil)
         (text3 foil))       
    
    ; But one of them is the target
    (case (act-r-random 3)
      (0 (setf text1 target))
      (1 (setf text2 target))
      (2 (setf text3 target)))

	; Add these 3 texts to the window
    (add-text-to-exp-window :text text1 :x 125 :y 75)
    (add-text-to-exp-window :text text2 :x 75 :y 175)
    (add-text-to-exp-window :text text3 :x 175 :y 175)
    
    (setf *response* nil)
    
    (if (not (eq who 'human)) 
        (progn
          (install-device window)
          (proc-display)
          (run 10 :real-time t))
      (while (null *response*)
        (allow-event-manager window)))
    
    (if (string-equal *response* target)
        'correct
      nil)))

;-----------
; ACT-R CODE
;-----------

; MODEL DEFINITION
(define-model unit2
(sgp :v t :show-focus t :needs-mouse nil)

; CHUNK-TYPES
(chunk-type read-letters state)
(chunk-type array letter1 letter2 letter3)

; CHUNKS AND GOAL
(add-dm
	(start isa chunk)
	(attend isa chunk)
	(respond isa chunk)
	(done isa chunk)
	
	; The goal is to read the letter
	(goal isa read-letters state start)
)

; PRODUCTIONS
(P find-unattended-letter
	=goal>
		ISA		read-letters
		state	start
==>
	+visual-location>
		:attended	nil
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
		cmd		move-attention
		screen-pos =visual-location
	=goal>
		state	attend
)

(P encode-letter1
	=goal>
		ISA		read-letters
		state	attend
	=visual>
		value	=letter
	?imaginal>
		state	free
==>
	=goal>
		state 	respond
	+imaginal>
		ISA		array
		letter1 =letter
)

(P encode-letter2
	=goal>
		ISA		read-letters
		state	attend
	=visual>
		value	=letter
	=imaginal>
		ISA		array
		letter1 =letter1
		letter2 nil
		letter3 nil
==>
	=goal>
		state	respond
	=imaginal>
		letter2 =letter
)

(P encode-letter3
	=goal>
		ISA		read-letters
		state	attend
	=visual>
		value	=letter
	=imaginal>
		ISA		array
		letter1 =letter1
		letter2 =letter2
		letter3 nil
==>
	=goal>
		state	respond
	=imaginal>
		ISA		array
		letter3 =letter
)

(P rollback2
	=goal>
		ISA		read-letters
		state	respond
	=imaginal>
		ISA		array
		letter1 =letter1
		letter2 nil
		letter3 nil
==>
	=goal>
		state	start
	=imaginal>
	!output!	=letter1
)

(P rollback3
	=goal>
		ISA		read-letters
		state	respond
	=imaginal>
		ISA		array
		letter1 =letter1
		letter2 =letter2
		letter3 nil
==>
	=goal>
		state	start
	=imaginal>
	!output!	=letter2
)

(P respond-first-letter
	=goal>
		ISA		read-letters
		state	respond
	=imaginal>
		isa		array
		letter1 =dif-letter
		letter2 =letter
		letter3 =letter
	?manual>
		state	free
==>
	=goal>
		state	done
	+manual>
		cmd		press-key
		key		=letter
	; Print out that letter
)

(P respond-second-letter
	=goal>
		ISA		read-letters
		state	respond
	=imaginal>
		isa		array
		letter1 =letter
		letter2 =dif-letter
		letter3 =letter
	?manual>
		state	free
==>
	=goal>
		state	done
	+manual>
		cmd		press-key
		key		=dif-letter
	!output!	=dif-letter ; Print out that letter
)

(P respond-third-letter
=goal>
		ISA		read-letters
		state	respond
	=imaginal>
		isa		array
		letter1 =letter
		letter2 =letter
		letter3 =dif-letter
	?manual>
		state	free
==>
	=goal>
		state	done
	+manual>
		cmd		press-key
		key		=dif-letter
	!output!	=dif-letter ; Print out that letter
)

(goal-focus goal)
)