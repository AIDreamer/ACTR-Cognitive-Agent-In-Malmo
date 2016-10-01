;;;; -----------------------------------------------------------------------------------------
;;;; This model is developed by Son Pham.
;;;;
;;;; (load "C:/cygwin/home/Son Pham/Cognitive-Agents-in-Malmo/minecraft-discrete/loader.lisp")


(clear-all)

(defmethod device-handle-keypress ((device list) key)
	(model-output "Model pressed key ~c" key))

(define-model minecraft-test

	(sgp :jni-hostname "127.0.0.1" :jni-port 5555 :jni-sync t)

	;;; ---------------------
	;;; CHUNK-TYPE AND CHUNKS
	;;; ---------------------
	
	(chunk-type (blockobj (:include visual-object)) value letter) 
	(chunk-type minecraft-mission state up down left right look-dir decision-made)
	(chunk-type block x y type)
  
	(add-dm
		;; MINECRAFT FACTS

		;; GOAL STATES
		(get-info ISA chunk)
		(find-loc ISA chunk)
		(attend-loc ISA chunk)
		(make-decision ISA chunk)
		(speak-decision ISA chunk)
		
		;; LOOK DIRECTIONS
		(dir-up ISA chunk)
		(dir-down ISA chunk)
		(dir-left ISA chunk)
		(dir-right ISA chunk)

		;; GOAL CHUNK
		(goal isa minecraft-mission state get-info look-dir dir-up))

	(goal-focus goal)

	(sgp :v t :trace-detail low)
	(sgp :needs-mouse nil :process-cursor nil)
  
	;;; ----------------------
	;;; GATHER DATA PRODUCTION
	;;; ----------------------

	;;; Get data forward
	(p look-forward
		=goal>
			ISA minecraft-mission
			state get-info
			look-dir dir-up
		==>
		+visual-location>
			= screen-x 1
			= screen-y 0
		=goal>
			state find-loc
	)

	(p get-forward
		=goal>
			ISA minecraft-mission
			state find-loc
			look-dir dir-up
		=visual-location>
		?visual>
			state free
		==>
		+visual>
			cmd move-attention
			screen-pos =visual-location
		=goal>
			state attend-loc
	)

	(p attend-forward
		=goal>
			ISA minecraft-mission
			state attend-loc
			look-dir dir-up
		=visual>
			value =value
		==>
		=goal>
			ISA minecraft-mission
			state get-info
			up =value
			look-dir dir-down
		-visual>
	)

	;;; Get data backward
	(p look-backward
		=goal>
			ISA minecraft-mission
			state get-info
			look-dir dir-down
		==>
		+visual-location>
			= screen-x 1
			= screen-y 2
		=goal>
			state find-loc
	)

	(p get-backward
		=goal>
			ISA minecraft-mission
			state find-loc
			look-dir dir-down
		=visual-location>
		?visual>
			state free
		==>
		+visual>
			cmd move-attention
			screen-pos =visual-location
		=goal>
			state attend-loc
	)

	(p attend-backward
		=goal>
			ISA minecraft-mission
			state attend-loc
			look-dir dir-down
		=visual>
			value =value
		==>
		=goal>
			ISA minecraft-mission
			state get-info
			down =value
			look-dir dir-left
	)

	;;; Get data left
	(p look-left
		=goal>
			ISA minecraft-mission
			state get-info
			look-dir dir-left
		==>
		+visual-location>
			= screen-x 0
			= screen-y 1
		=goal>
			state find-loc
	)

	(p get-left
		=goal>
			ISA minecraft-mission
			state find-loc
			look-dir dir-left
		=visual-location>
		?visual>
			state free
		==>
		+visual>
			cmd move-attention
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
		==>
		=goal>
			ISA minecraft-mission
			state get-info
			left =value
			look-dir dir-right
	)

	;;; Get data right
	(p look-right
		=goal>
			ISA minecraft-mission
			state get-info
			look-dir dir-right
		==>
		+visual-location>
			= screen-x 2
			= screen-y 1
		=goal>
			state find-loc
	)

	(p get-right
		=goal>
			ISA minecraft-mission
			state find-loc
			look-dir dir-right
		=visual-location>
		?visual>
			state free
		==>
		+visual>
			cmd move-attention
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
		==>
		=goal>
			ISA minecraft-mission
			state make-decision
			right =value
	)

	;;; ------------------------
	;;; MAKE DECISION PRODUCTION
	;;; ------------------------

	(p go-up
		=goal>
			ISA minecraft-mission
			state make-decision
			up "sandstone"
			- decision-made dir-down
		?manual>
			state free
		==>
		=goal>
			ISA minecraft-mission
			state get-info
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
			state make-decision
			down "sandstone"
			- decision-made dir-up
		?manual>
			state free
		==>
		=goal>
			ISA minecraft-mission
			state get-info
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
			state make-decision
			left "sandstone"
			- decision-made dir-right
		?manual>
			state free
		==>
		=goal>
			ISA minecraft-mission
			state get-info
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
			state make-decision
			right "sandstone"
			- decision-made dir-left
		?manual>
			state free
		==>
		=goal>
			ISA minecraft-mission
			state get-info
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
)