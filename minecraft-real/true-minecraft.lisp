(clear-all)

;; Will transfer some code from Python to Lisp for better timing.
;; So far, the code just has to sit there.
(defvar *pre-decision-time* 0)
(defvar *post-decision-time* 0)

(defmethod decision-cost (pre-time post-time)
	(- post-time pre-time))

;; This is the model part
(defmethod device-handle-keypress ((device list) key)
	(model-output "Model pressed key ~c" key))

(define-model true-minecraft

	;; ========
	;; SETTINGS
	;; ========

	(sgp :jni-hostname "127.0.0.1" :jni-sync t :jni-port 5555)
	(sgp :v t :trace-detail low :model-warnings nil)
	(sgp :needs-mouse nil :process-cursor nil)
	(sgp :dat 0.010 
		 :visual-attention-latency 0.010 
		 :motor-burst-time 0.01 
		 :motor-feature-prep-time 0.01 
		 :motor-initiation-time 0.01)
	(sgp :u)
	(sgp :ul t :esc t)
	
	;; ======================
	;; CHUNK-TYPES AND CHUNKS
	;; ======================

	;; Visual object chunk type
	(chunk-type (groundobj (:include visual-object)) value)
	(chunk-type (airobj (:include visual-object)) value)
	(chunk-type (statsobj (:include visual-object)) state health)
	(chunk-type (mapobj (:include visual-object)) up down left right)
	(chunk-type (inventoryobj (:include visual-object)) item0 item1 item2 item3 item4 item5 item6 item7 item8 item9 item10)
	
	;; Decision chunk type
	(chunk-type decision posX posY direction time-cost)
	
	;; Mission chunk-type
	(chunk-type minecraft-mission 
				state 
				status health 
				forward ground 
				up down left right posX posY 
				item0 item1 item2 item3 item4 item5 item6 item7 item8 item9)

	;; Facts
	(add-dm
		;; MINECRAFT FACTS
		
		;; AGENT STATES
		(get-data ISA chunk)

		; Stats
		(look-stats ISA chunk)
		(find-stats ISA chunk)
		(attend-stats ISA chunk)
		(moving-vs-deciding ISA chunk)

		; Forward 1
		(look-forward ISA chunk)
		(find-forward ISA chunk)
		(attend-forward ISA chunk)
		
		; Ground 2
		(look-ground ISA chunk)
		(find-ground ISA chunk)
		(attend-ground ISA chunk)	
		(jump-or-stay ISA chunk)

		; Map
		(look-map ISA chunk)
		(find-map ISA chunk)
		(attend-map ISA chunk)
		(decide-direction ISA chunk)

		; Inventory
		(look-inventory ISA chunk)
		(find-inventory ISA chunk)
		(attend-inventory ISA chunk)

		; Decision
		(inspection ISA chunk)

		;; GOAL CHUNK
		(goal isa minecraft-mission state get-data health 10)
	)

	(goal-focus goal)

	;; ===========
	;; PRODUCTIONS
	;; ===========

	; ---------------------------------
	; Initiate a get data by pressing H
	; ---------------------------------

	(p get-data
		=goal>
			ISA minecraft-mission
			state get-data
		?manual>
			state free
		==>
		=goal>
			ISA minecraft-mission
			state look-stats
		+manual>
			ISA press-key
			key "H")

	; Keep looking back to get-data if no information 
	; about the environment arrived yet

	(p no-info-found
		=goal>
			ISA minecraft-mission
			state find-stats
		==>
		=goal>
			state get-data)

	; ----------------------------------
	; Look the stats (of the agent) data
	; ----------------------------------

	(p look-stats
		=goal>
			ISA minecraft-mission
			state look-stats
		==>
		+visual-location>
			= screen-x -1
			= screen-y 0
			:attended nil
		=goal>
			ISA minecraft-mission
			state find-stats)

	(p find-stats
		=goal>
			ISA minecraft-mission
			state find-stats
		=visual-location>
		?visual>
			state free
		==>
		+visual>
			cmd move-attention
			screen-pos =visual-location
			kind statsobj
		=goal>
			state attend-stats)

	(p attend-stats
		=goal>
			ISA minecraft-mission
			state attend-stats
		=visual>
			status =status
			health =health
		==>
		=goal>
			status =status
			health =health
			state moving-vs-deciding)

	; -------------------------------------
	; If "moving", get the environment data
	; If "deciding", get the map data
	; -------------------------------------

	(p moving
		=goal>
			ISA minecraft-mission
			state moving-vs-deciding
			status "moving"
		==>
		=goal>
			state look-forward)

	(p deciding
		=goal>
			ISA minecraft-mission
			state moving-vs-deciding
			status "deciding"
		==>
		=goal>
			state look-map)

	; --------------------
	; Look the forward data
	; --------------------

	(p look-forward
		=goal>
			ISA minecraft-mission
			state look-forward
		==>
		+visual-location>
			= screen-x 1
			= screen-y 2
		=goal>
			ISA minecraft-mission
			state find-forward)

	(p find-forward
		=goal>
			ISA minecraft-mission
			state find-forward
		=visual-location>
		?visual>
			state free
		==>
		+visual>
			cmd move-attention
			screen-pos =visual-location
			kind airobj
		=goal>
			state attend-forward)

	(p attend-forward
		=goal>
			ISA minecraft-mission
			state attend-forward
		=visual>
			value =value
		==>
		=goal>
			forward =value
			state look-ground)
			
	; -------------------
	; Look the ground data
	; -------------------

	(p look-ground
		=goal>
			ISA minecraft-mission
			state look-ground
		==>
		+visual-location>
			= screen-x 0
			= screen-y 7
		=goal>
			ISA minecraft-mission
			state find-ground)

	(p find-ground
		=goal>
			ISA minecraft-mission
			state find-ground
		=visual-location>
		?visual>
			state free
		==>
		+visual>
			cmd move-attention
			screen-pos =visual-location
			kind groundobj
		=goal>
			state attend-ground)

	(p attend-ground
		=goal>
			ISA minecraft-mission
			state attend-ground
		=visual>
			value =value
		==>
		=goal>
			forward =value
			state jump-or-stay)

	(p jump-lava
		=goal>
			ISA minecraft-mission
			state jump-or-stay
			ground "lava"
		?manual>
			state free
		==>
		+manual>
			isa press-key
			key "l"
		=goal>
			state get-data)

	(p jump-obstacle
		=goal>
			ISA minecraft-mission
			state jump-or-stay
			- forward "air"
		?manual>
			state free
		==>
		+manual>
			isa press-key
			key "l"
		=goal>
			state get-data)

	(p dont-jump
		=goal>
			ISA minecraft-mission
			state jump-or-stay
			forward "air"
			- ground "lava"
		?manual>
			state free
		==>
		+manual>
			isa press-key
			key "m"
		=goal>
			state get-data)

	; ----------------
	; Look the map data
	; ----------------

	(p look-map
		=goal>
			ISA minecraft-mission
			state look-map
		==>
		+visual-location>
			= screen-x -1
			= screen-y 1
		=goal>
			state find-map)

	(p find-map
		=goal>
			ISA minecraft-mission
			state find-map
		=visual-location>
		?visual>
			state free
		==>
		+visual>
			cmd move-attention
			screen-pos =visual-location
			kind mapobj
		=goal>
			state attend-map)

	(p attend-map
		=goal>
			ISA minecraft-mission
			state attend-map
		=visual>
			up =up
			down =down
			left =left
			right =right
		==>
		=goal>
			up =up
			down =down
			left =left
			right =right
			state decide-direction)

	; --------------------------------------
	; Decide on one of the direction to take
	; --------------------------------------

	(p decide-up
		=goal>
			ISA minecraft-mission
			state decide-direction
			up =up
			< up 1000000
			>= down =up
			>= left =up
			>= right =up
		?manual>
			state free
		==>
		+manual>
			cmd press-key
			key "w"
		=goal>
			state get-data)

	(p decide-down
		=goal>
			ISA minecraft-mission
			state decide-direction
			down =down
			< down 1000000
			>= up =down
			>= left =down
			>= right =down
		?manual>
			state free
		==>
		+manual>
			cmd press-key
			key "s"
		=goal>
			state get-data)

	(p decide-left
		=goal>
			ISA minecraft-mission
			state decide-direction
			left =left
			< left 1000000
			>= right =left
			>= up =left
			>= down =left
		?manual>
			state free
		==>
		+manual>
			cmd press-key
			key "a"
		=goal>
			state get-data)

	(p decide-right
		=goal>
			ISA minecraft-mission
			state decide-direction
			right =right
			< right 1000000
			>= left =right
			>= up =right
			>= down =right
		?manual>
			state free
		==>
		+manual>
			cmd press-key
			key "d"
		=goal>
			state get-data)
)