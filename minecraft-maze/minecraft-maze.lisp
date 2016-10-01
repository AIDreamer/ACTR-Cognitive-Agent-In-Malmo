(clear-all)

(defmethod device-handle-keypress ((device list) key)
	(model-output "Model pressed key ~c" key))

(defun add-one (x) (+ x 1))

(define-model minecraft-test

	(sgp :jni-hostname "127.0.0.1" :jni-port 5555 :jni-sync t)
  
	;;; ---------------------
	;;; CHUNK-TYPE AND CHUNKS
	;;; ---------------------
	
	(chunk-type (blockobj (:include visual-object)) value letter)
	(chunk-type minecraft-mission state look-dir up down left right decision-made decision1 decision2 count-dup)
	(chunk-type block x y type)
  
	(add-dm
		;; MINECRAFT FACTS

		;; GOAL STATES
		(get-data ISA chunk)
		(look-data ISA chunk)
		(find-loc ISA chunk)
		(attend-loc ISA chunk)
		(inspect-data ISA chunk)

		;; LOOK DIRECTIONS
		(dir-up ISA chunk)
		(dir-down ISA chunk)
		(dir-left ISA chunk)
		(dir-right ISA chunk)
		(no-dir ISA chunk)

		;; GOAL CHUNK
		(goal isa minecraft-mission state get-data look-dir dir-up decision-made no-dir decision1 no-dir decision2 no-dir count-dup 0))

	(goal-focus goal)

	(sgp :v t :trace-detail low)
	(sgp :needs-mouse nil :process-cursor nil)
	(sgp :dat 0.010 
		 :visual-attention-latency 0.010 
		 :motor-burst-time 0.01 
		 :motor-feature-prep-time 0.01 
		 :motor-initiation-time 0.01)
  
	;;; ----------
	;;; PRODUCTION
	;;; ----------
	(p get-data
		=goal>
			ISA minecraft-mission
			state get-data
		?manual>
			state free
		==>
		=goal>
			ISA minecraft-mission
			state look-data
		+manual>
			ISA press-key
			key "H"
	)

	;; Keep looping back to intro if no information
	;; about the environment has arrived yet
	(p no-loc-found
		=goal>
			ISA minecraft-mission
			state find-loc
			look-dir dir-up
		==>
		=goal>
			state get-data
	)

	;; -----------
	;; Get data UP
	;; -----------

	(p look-up
		=goal>
			ISA minecraft-mission
			state look-data
			look-dir dir-up
		==>
		+visual-location>
			= screen-x 1
			= screen-y 2
		=goal>
			ISA minecraft-mission
			state find-loc
	)

	(p found-up
		=goal>
			ISA	minecraft-mission
			state find-loc
			look-dir dir-up
		=visual-location>
		?visual>
			state free
		==>
		+visual>
			cmd	move-attention
			screen-pos =visual-location
		=goal>
			state attend-loc
	)

	(p attend-up
		=goal>
			ISA minecraft-mission
			state attend-loc
			look-dir dir-up
		=visual>
			value =value
		?manual>
			state free
		==>
		=goal>
			state look-data
			look-dir dir-down
			up =value
		-visual>
		!output! =value
	)

	;; -------------
	;; Get data DOWN
	;; -------------

	(p look-down
		=goal>
			ISA minecraft-mission
			state look-data
			look-dir dir-down
		==>
		+visual-location>
			= screen-x 1
			= screen-y 0		
		=goal>
			ISA minecraft-mission
			state find-loc
	)

	
	;; If there is information, attend the object and read it
	(p found-down
		=goal>
			ISA	minecraft-mission
			state find-loc
			look-dir dir-down
		=visual-location>
		?visual>
			state free
		==>
		+visual>
			cmd	move-attention
			screen-pos =visual-location
		=goal>
			state attend-loc
	)

	(p attend-down
		=goal>
			ISA minecraft-mission
			state attend-loc
			look-dir dir-down
		=visual>
			value =value
		?manual>
			state free
		==>
		=goal>
			state look-data
			look-dir dir-left
			down =value
		-visual>
		!output! =value
	)

	;; -------------
	;; Get data LEFT
	;; -------------

	(p look-left
		=goal>
			ISA minecraft-mission
			state look-data
			look-dir dir-left
		==>
		+visual-location>
			= screen-x 2
			= screen-y 1		
		=goal>
			ISA minecraft-mission
			state find-loc
	)

	
	;; If there is information, attend the object and read it
	(p found-left
		=goal>
			ISA	minecraft-mission
			state find-loc
			look-dir dir-left
		=visual-location>
		?visual>
			state free
		==>
		+visual>
			cmd	move-attention
			screen-pos =visual-location
		=goal>
			state attend-loc
	)

	(p attend-left
		=goal>
			ISA minecraft-mission
			state attend-loc
			look-dir dir-left
		=visual>
			value =value
		?manual>
			state free
		==>
		=goal>
			state look-data
			look-dir dir-right
			left =value
		-visual>
		!output! =value
	)

	;; --------------
	;; Get data RIGHT
	;; --------------

	(p look-right
		=goal>
			ISA minecraft-mission
			state look-data
			look-dir dir-right
		==>
		+visual-location>
			= screen-x 0
			= screen-y 1		
		=goal>
			ISA minecraft-mission
			state find-loc
	)

	
	;; If there is information, attend the object and read it
	(p found-right
		=goal>
			ISA	minecraft-mission
			state find-loc
			look-dir dir-right
		=visual-location>
		?visual>
			state free
		==>
		+visual>
			cmd	move-attention
			screen-pos =visual-location
		=goal>
			state attend-loc
	)

	(p attend-right
		=goal>
			ISA minecraft-mission
			state attend-loc
			look-dir dir-right
		=visual>
			value =value
		?manual>
			state free
		==>
		=goal>
			state inspect-data
			right =value
		-visual>
		!output! =value
	)

	;; ------------------------------------
	;; Inspect the data to get the movement
	;; ------------------------------------

	(p go-up
		=goal>
			ISA minecraft-mission
			state inspect-data
			up "sandstone"
			- decision1 dir-down
			- decision2 dir-down
		?manual>
			state free
		==>
		=goal>
			ISA minecraft-mission
			state analyze-state
			look-dir dir-up
			decision-made dir-up
			up nil
			down nil
			left nil
			right nil
		+manual>
			cmd press-key
			key "W"
	)
	
	(p go-down
		=goal>
			ISA minecraft-mission
			state inspect-data
			down "sandstone"
			- decision1 dir-up
			- decision2 dir-up
		?manual>
			state free
		==>
		=goal>
			ISA minecraft-mission
			state analyze-state
			look-dir dir-up
			decision-made dir-down
			up nil
			down nil
			left nil
			right nil
		+manual>
			cmd press-key
			key "S"
	)

	(p go-left
		=goal>
			ISA minecraft-mission
			state inspect-data
			left "sandstone"
			- decision1 dir-right
			- decision2 dir-right
		?manual>
			state free
		==>
		=goal>
			ISA minecraft-mission
			state analyze-state
			look-dir dir-up
			decision-made dir-left
			up nil
			down nil
			left nil
			right nil
		+manual>
			cmd press-key
			key "A"
	)

	(p go-right
		=goal>
			ISA minecraft-mission
			state inspect-data
			right "sandstone"
			- decision1 dir-left
			- decision2 dir-left
		?manual>
			state free
		==>
		=goal>
			ISA minecraft-mission
			state analyze-state
			look-dir dir-up
			decision-made dir-right
			up nil
			down nil
			left nil
			right nil
		+manual>
			cmd press-key
			key "D"
	)

	(p no-dir
		=goal>
			ISA minecraft-mission
			state inspect-data
			- decision1 no-dir
			- decision2 no-dir
		?manual>
			state free
		==>
		=goal>
			ISA minecraft-mission
			state analyze-state
			look-dir dir-up
			decision-made no-dir
			up nil
			down nil
			left nil
			right nil
		+manual
			cmd press-key
			key "F"
	)	

	;; ------------------
	;; Analyse the memory
	;; ------------------
	
	(p swap-dec
		=goal>
			ISA minecraft-mission
			state analyze-state
			decision-made =same-dec
			- decision1 =same-dec
			decision1 =other-dec
			decision2 =same-dec
		==>
		=goal>
			state get-data
			decision1 =same-dec
			decision2 =other-dec
	)

	(p count-dec
		=goal>
			ISA minecraft-mission
			state analyze-state
			decision-made =same-dec
			decision1 =same-dec
			- decision2 =same-dec
			decision2 =other-dec
			< count-dup 4
			count-dup =count
		==>
		!bind! =count2 (add-one =count)
		=goal>
			state get-data
			decision1 =same-dec
			decision2 =other-dec
			count-dup =count2
	)

	(p smooth-count
		=goal>
			ISA minecraft-mission
			state analyze-state
			decision-made =same-dec
			decision1 =same-dec
			- decision2 =same-dec
			decision2 =other-dec
			count-dup 4
		==>
		=goal>
			state get-data
			decision1 =same-dec
			decision2 =same-dec
			count-dup 0
	)

	(p continous-dec
		=goal>
			ISA minecraft-mission
			state analyze-state
			decision-made =same-dec
			decision1 =same-dec
			decision2 =same-dec
		==>
		=goal>
			state get-data
			decision1 =same-dec
			decision2 =same-dec
			count-dup 0
	)
		
	(p chain-dec
		=goal>
			ISA minecraft-mission
			state analyze-state
			decision-made =dec1
			- decision1 =dec1
			decision1 =dec2
			- decision2 =dec1
			decision2 =dec3
		==>
		=goal>
			state get-data
			decision1 =dec1
			decision2 =dec2
	)
)