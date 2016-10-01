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
	)
)