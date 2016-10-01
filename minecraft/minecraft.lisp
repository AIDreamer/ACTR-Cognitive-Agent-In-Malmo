(clear-all)

(defmethod device-handle-keypress ((device list) key)
	(model-output "Model pressed key ~c" key))

(define-model minecraft-test

	(sgp :jni-hostname "127.0.0.1" :jni-port 5555 :jni-sync t)
  
	;;; ---------------------
	;;; CHUNK-TYPE AND CHUNKS
	;;; ---------------------
	
	(chunk-type (blockobj (:include visual-object)) value letter)
	(chunk-type minecraft-mission state inspection)
	(chunk-type block x y type)
  
	(add-dm
		;; MINECRAFT FACTS

		;; GOAL STATES
		(get-data ISA chunk)
		(look-data ISA chunk)
		(find-loc ISA chunk)
		(attend-loc ISA chunk)
		(inspect-block ISA chunk)

		;; GOAL CHUNK
		(goal isa minecraft-mission state get-data))

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

	(p look-data
		=goal>
			ISA minecraft-mission
			state look-data
		==>
		+visual-location>
			= screen-x 0
			= screen-y 2
		=goal>
			ISA minecraft-mission
			state find-loc
	)

	;; Keep looping back to intro if no information
	;; about the environment has arrived yet
	(p no-loc-found
		=goal>
			ISA minecraft-mission
			state find-loc
		==>
		=goal>
			state get-data
	)

	;; If there is information, attend the object and read it
	(p loc-found
		=goal>
			ISA	minecraft-mission
			state find-loc
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

	;; Take a look to see what it is
	(p attend-loc
		=goal>
			ISA minecraft-mission
			state attend-loc
		=visual>
			value =value
		?manual>
			state free
		==>
		=goal>
			state inspect-block
			inspection =value
		-visual>
	)

	;; Inspect the block
	(p inspect-block-jump
		=goal>
			ISA minecraft-mission
			state inspect-block
			inspection "lava"
		?manual>
			state free
		==>
		=goal>
			state intro
			inspection nil
		+manual>
			cmd	press-key
			key "f"
		!output! "lava")

	(p inspect-block-not-jump
		=goal>
			ISA minecraft-mission
			state inspect-block
			inspection =value
			- inspection "lava"
		==>
		=goal>
			state get-data
			inspection nil
		!output! =value)
)