(clear-all)

(defmethod device-handle-keypress ((device list) key)
	(model-output "Model pressed key ~c" key))

(define-model minecraft-test

	(sgp :jni-hostname "127.0.0.1" :jni-port 5555 :jni-sync t)
	(sgp :v t :trace-detail low)
	(sgp :needs-mouse nil :process-cursor nil)
	(sgp :dat 0.010 
		 :visual-attention-latency 0.010 
		 :motor-burst-time 0.01 
		 :motor-feature-prep-time 0.01 
		 :motor-initiation-time 0.01)
	(sgp :u)
	(sgp :ul t)
  
	;;; ---------------------
	;;; CHUNK-TYPE AND CHUNKS
	;;; ---------------------
	
	(chunk-type (blockobj (:include visual-object)) value health)
	(chunk-type (airobj (:include visual-object)) value health)
	(chunk-type (statsobj (:include visual-object)) value health)
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
		(goal isa minecraft-mission state get-data health 10))

	(goal-focus goal)
  
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
		==>
		=goal>
			state get-data
	)

	;; --------------
	;; Get data STATE
	;; --------------

	(p look-state
		=goal>
			ISA minecraft-mission
			state look-data
		==>
		+visual-location>
			= screen-x 2
			= screen-y 2
		=goal>
			ISA minecraft-mission
			state find-loc
		!eval! (print-visicon)
	)

	(p found-state
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
			kind blockobj
		=goal>
			state attend-loc
	)

	(p attend-state
		=goal>
			ISA minecraft-mission
			state attend-loc
		=visual>
			value =value
			health =health
		?manual>
			state free
		==>
		=goal>
			state inspect-block
			inspection =value
			health =health
		-visual>
	)

	;; ------------------------------------
	;; Inspect the data to get the movement
	;; ------------------------------------

	(p go-somewhere
		=goal>
			ISA minecraft-mission
			state inspect-block
			inspection "deciding"
		?manual>
			state free
		==>
		=goal>
			ISA minecraft-mission
			state get-data
		+manual>
			cmd press-key
			key "J"
	)

	(p keep-doing
		=goal>
			ISA minecraft-mission
			state inspect-block			
			- inspection "deciding"
		==>
		=goal>
			ISA minecraft-mission
			state get-data
	)
)