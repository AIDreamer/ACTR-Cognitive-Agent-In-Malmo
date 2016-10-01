;; Play the simple capture game in 1 window with buttons for interaction.
;; Buttons highlight in red or blue to indicate player turn.

;; global variables for game info

(defvar *current-player*)  ;; name of model or human
(defvar *p1*)              ;; player1 name
(defvar *p2*)              ;; player2 name
(defvar *game-over*)       ;; if true stop the game
(defvar *p1-position*)     ;; current player locations, 0-5
(defvar *p2-position*)

(defvar *window*)          ;; the interaction window
(defvar *human-action*)    ;; a flag to indicate an external action needs to be processed


(defvar *spaces* (make-list 6 :initial-element nil)) ;; the actual button objects

;; Create the initial game board, install that device for all models and have them process the screen.

(defun start-game ()
  (let ((window (with-model game (open-exp-window "game" :height 100 :width 300 :visible t)))) ; created in the game model
    
    (setf *window* window)
    (setf *p1-position* 0)
    (setf *p2-position* 5)
    
    (dolist (m (mp-models))  ;; just set all models to use this device whether they're playing or not
      (with-model-eval m
        (install-device window)))
    
    (dotimes (i 6)            ;; create the buttons
      (setf (nth i *spaces*)  ;; save them for later use
        (let ((index i))      ;; need to bind the value of i to use in the action function
          (add-button-to-exp-window :window window ;; indicate which window since it's not in a model
                                    :x (+ 10 (* i 40)) 
                                    :y 10 
                                    :width 35 :height 35 
                                    :color (if (zerop i) 'red 'white) :text (if (zerop i) "1" (if (= i 5) "2" "")) 
                                    :action (lambda (b)            ;; button actions always get called with the button
                                              (declare (ignore b)) ;; don't need that here so ignore to avoid warnings
                                              (pick-button index))))))
    
    ;; Add a safety button so I can stop the game explicitly
    
    (add-button-to-exp-window :window window :x 10 :y 60 :width 100 :height 30 :color 'gray :text "stop" 
                              :action (lambda (b) 
                                        (declare (ignore b))  
                                        (setf *game-over* t))) ;; *game-over* is how the run-until-condition function knows to stop

    (dolist (m (mp-models))  ;; have all of the models look at the screen
      (with-model-eval m
        (proc-display)))))

;; The function to call when a button is pressed.
;; If the current player made the move, update the state 
;; immediately for a model and set a flag to indicate human input
;; for processing by the event.

(defun pick-button (index)
  
  ;; Make sure the right player made the action
  ;; but can't distinguish between two different humans
  
  (when (or (eq (model-generated-action) *current-player*)
            (and (or (eq *current-player* 'human2) (eq *current-player* 'human)) (not (model-generated-action))))
    
    ;; Note that for some native interface windows (like those in ACL, LispWorks, or CCL)
    ;; it is possible for a model to make an action but not have model-generated-action
    ;; be true since it might be a separate GUI thread handling the real window input.
    ;; That will never happen for virtual windows or experiment windows shown through
    ;; the ACT-R Environment (i.e. visible virtual windows).
    ;; If one is using such a system, then something like the code for the human player
    ;; might be required to make sure the processing occurs in the same thread that is
    ;; running ACT-R.
    
    (if (model-generated-action)
        ;; Handle model actions immediately
        (update-game-state index)
      ;; For human interaction set a flag so the
      ;; ACT-R event can handle the update
      (setf *human-action* index))))

;; Function which actually updates the game information
;; based on a box being selected.

(defun update-game-state (index)
  (if (eq *current-player* *p1*)
      ;; check if it was a valid move -- 1 or 2 spaces forward
      (when (and (> index *p1-position*) (<= (- index *p1-position*) 2))
        
        ;; get rid of the old p1 space and the one where it's moving to
        (remove-items-from-exp-window :window *window* (nth *p1-position* *spaces*) (nth index *spaces*))
        
        (if (>= index *p2-position*) 
            (progn  ;; p1 wins 
              ;; schedule an event to indicate the game is over after a few
              ;; seconds to allow the models a chance to see and process the
              ;; end game state if necessary
              (schedule-event-relative 3 (lambda () (setf *game-over* *p1*)))
              (add-button-to-exp-window :window *window* :x (+ 10 (* *p1-position* 40)) :y 10 :width 35 :height 35 :color 'white :text "")
              (add-button-to-exp-window :window *window* :x (+ 10 (* index 40)) :y 10 :width 35 :height 35 :color 'green :text "1"))
          
          (progn ;; update the p1 position and make p2 the current player
            
            (setf (nth *p1-position* *spaces*) (add-button-to-exp-window :window *window* 
                                                                         :x (+ 10 (* *p1-position* 40)) 
                                                                         :y 10 
                                                                         :width 35 :height 35 
                                                                         :color 'white :text "" 
                                                                         :action (lambda (b)            
                                                                                   (declare (ignore b)) 
                                                                                   (pick-button *p1-position*))))
            
            (setf (nth index *spaces*) (add-button-to-exp-window :window *window* 
                                                                 :x (+ 10 (* index 40)) 
                                                                 :y 10 
                                                                 :width 35 :height 35 
                                                                 :color 'white :text "1" 
                                                                 :action (lambda (b)            
                                                                           (declare (ignore b)) 
                                                                           (pick-button index))))
            
            (remove-items-from-exp-window :window *window* (nth *p2-position* *spaces*))
            
            (setf (nth *p2-position* *spaces*) (add-button-to-exp-window :window *window* 
                                                                         :x (+ 10 (* *p2-position* 40)) 
                                                                         :y 10 
                                                                         :width 35 :height 35 
                                                                         :color 'blue :text "2" 
                                                                         :action (lambda (b)
                                                                                   (declare (ignore b)) 
                                                                                   (pick-button *p2-position*))))
            (setf *p1-position* index)
            (setf *current-player* *p2*))))
    
    ;; if p2 makes a valid move
    (when (and (< index *p2-position*) (<= (- *p2-position* index) 2))
      
      (remove-items-from-exp-window :window *window* (nth *p2-position* *spaces*) (nth index *spaces*))
      
      (if (<= index *p1-position*) 
          (progn ;; p2 wins
            (schedule-event-relative 3 (lambda () (setf *game-over* *p2*)))
            (add-button-to-exp-window :window *window* :x (+ 10 (* *p2-position* 40)) :y 10 :width 35 :height 35 :color 'white :text "")
            (add-button-to-exp-window :window *window* :x (+ 10 (* index 40)) :y 10 :width 35 :height 35 :color 'green :text "2"))
        
        (progn ;; update the p2 poston and make p1 the current player
          
          (setf (nth *p2-position* *spaces*) (add-button-to-exp-window :window *window* 
                                                                       :x (+ 10 (* *p2-position* 40)) 
                                                                       :y 10 
                                                                       :width 35 :height 35 
                                                                       :color 'white :text "" 
                                                                       :action (lambda (b)
                                                                                 (declare (ignore b))
                                                                                 (pick-button *p2-position*))))
          
          (setf (nth index *spaces*) (add-button-to-exp-window :window *window* 
                                                               :x (+ 10 (* index 40)) 
                                                               :y 10 
                                                               :width 35 :height 35 
                                                               :color 'white :text "2" 
                                                               :action (lambda (b)
                                                                         (declare (ignore b))
                                                                         (pick-button index))))
          
          (remove-items-from-exp-window :window *window* (nth *p1-position* *spaces*))
          
          (setf (nth *p1-position* *spaces*) (add-button-to-exp-window :window *window* 
                                                                       :x (+ 10 (* *p1-position* 40)) 
                                                                       :y 10 
                                                                       :width 35 :height 35 
                                                                       :color 'red :text "1" 
                                                                       :action (lambda (b)
                                                                                 (declare (ignore b))
                                                                                 (pick-button *p1-position*))))
          (setf *p2-position* index)
          (setf *current-player* *p1*)))))
  
  ;; Have all models look at the window now
  
  (dolist (m (mp-models))
    (with-model-eval m
      (proc-display))))


;; Function to process human player actions as an event in
;; the model running since they're actually being generated
;; asynchronously (and possibly in a different Lisp thread).
      
(defun process-human-input ()
  (allow-event-manager *window*)
  (when *human-action*
    (update-game-state *human-action*)
    (setf *human-action* nil)))
          

;; play one round of the game resetting the models
;; before it starts.  Player1 and player2 are the names
;; of models or 'human.
        
(defun play (player1 player2)
  
  (reset)
  
  (unless (eq player1 'human) ;; if player1 is a model
    
    (with-model-eval player1
      
      ;; Have the visual-location buffer stuffing only pay attention to
      ;; buttons that are red or green (not any of the other colors used)
      
      (set-visloc-default-fct '(kind oval - color white - color blue - color gray))
      
      ;; Create a chunk with the player color and player label and make it the current goal
      
      (define-chunks-fct (list (list 'goal 'isa 'play 'my-color 'red 'my-number "1")))
      (goal-focus goal)))
  
  ;; do the same for player2
  
  (unless (eq player2 'human)
    (with-model-eval player2
      (set-visloc-default-fct '(kind oval - color white - color red - color gray))
      (define-chunks-fct (list (list 'goal 'isa 'play 'my-color 'blue 'my-number "2")))
      (goal-focus goal)))
  
  ;; If a human is playing schedule an event to make sure that the clock updates and
  ;; player actions get processed appropriately, but don't show it in the trace.
  
  (when (or (eq player1 'human) (eq player2 'human))
    (with-model game
      (schedule-periodic-event .5 'process-human-input :output nil)))
  
  ;; separate names for two human players
  
  (when (and (eq player1 'human) (eq player2 'human))
    (setf player2 'human2))
  
  ;; initialize some game info
  (setf *p1* player1)
  (setf *p2* player2)
  (setf *game-over* nil)
  (setf *current-player* player1)
  (setf *human-action* nil)
  
  (start-game)
  
  ;; Run ACT-R until the *game-over* variable is non-nil.
  
  (run-until-condition (lambda () *game-over*) :real-time t)
  
  ;; return the result
  *game-over*)
          
(clear-all)

(define-model game (sgp :v nil :needs-mouse nil)) ;; dummy model for creating window and human player delay events


(defparameter *model-code*
    '((sgp :v t :trace-detail high :needs-mouse nil :er t)
      
      (define-chunks (move) (read) (check) (done) (click))

      (chunk-type play my-color my-number state)
      
      (declare-buffer-usage goal play :all)
      
      (start-hand-at-mouse)
      
      (p my-turn-from-left
         =goal>
           state nil
           my-color red
         =visual-location>
           screen-x =x
           color red
       ==>
         ;; cheat and restrict location based on Lisp code
         !bind! =limit (+ =x 100)
         +visual-location>
           kind oval
           color white
          > screen-x =x
          < screen-x =limit
         =goal>
           state move)
      
      (p my-turn-from-right
         =goal>
           state nil
           my-color blue
         =visual-location>
           screen-x =x
           color blue
       ==>
         ;; cheat and restrict location based on Lisp code
         !bind! =limit (- =x 100)
         +visual-location>
           kind oval
           color white
          < screen-x =x
          >= screen-x =limit
         =goal>
           state move)
      
      (p game-over
         =goal>
           state nil
         =visual-location>
           screen-x =x
           screen-y =y
           color green
       ==>
         +visual-location>
           kind text
           screen-x =x
           screen-y =y
         =goal>
           state read)
      
      (p read
         =goal>
           state read
         =visual-location>
         ?visual>
           state free
       ==>
         =goal>
         state check
         +visual>
         isa move-attention
         screen-pos =visual-location)
      
      (p yeah
         =goal>
           state check
           my-number =num
         =visual>
           value =num
       ==>
         !output! (I won)
         =goal>
           state done)
      
      (p boo
         =goal>
           state check
           my-number =num
         =visual>
          - value =num
       ==>
         !output! (I lost)
         =goal>
           state done)
      

      (p move
         =goal>
           state move
         =visual-location>
         ?manual>
           state free
       ==>
         +manual>
           cmd move-cursor
           loc =visual-location
         =goal>
           state click)
      
      (p click
         =goal>
           state click
         ?manual>
           state free
       ==>
         +manual>
           cmd click-mouse
         =goal>
           state nil)))
      
(define-model-fct 'model1 *model-code*)
(define-model-fct 'model2 *model-code*)
