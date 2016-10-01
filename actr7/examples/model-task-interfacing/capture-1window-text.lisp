
;;; Play the capture game with a simple single interface that
;;; just shows position.  Have models say "done" to indicate 
;;; their turn is over and the game says the starting player's
;;; name at the beginning.  Show the winner's name in green
;;; when game over.


;; global variables

(defvar *current-player*) ;; model name
(defvar *p1*)             ;; player names
(defvar *p2*)
(defvar *game-over*)      ;; true means stop game
(defvar *p1-position*)    ;; player positions
(defvar *p2-position*)
(defvar *p1-text*)        ;; text items on the screen
(defvar *p2-text*)        ;; for player positions

(defvar *window*)          ;; save a window for human interaction
(defvar *human-action*)    ;; a flag to indicate an external action needs to be processed

;; Initialize the game information, create a window in the human model, display
;; the initial values and say the starting player's name to each model.

(defun start-game ()
  (setf *p1-position* 0)
  (setf *p2-position* 5)
  
  ;; create the window in the human model for simplicity
  
  (let ((window (with-model human (open-exp-window "game" :height 100 :width 200 :visible t))))
    
    ;; save that for possible use by a human player
    (setf *window* window)
    
    ;; install that as the device for all models
    (dolist (m (mp-models))
      (with-model-eval m
        (install-device window)))
    
    ;; in the human model add the text since that model owns the window
    ;; and a button to stop it as a safety measure
    
    (with-model human
      (setf *p1-text* (add-text-to-exp-window :text (princ-to-string *p1-position*) :color 'red :x 20 :y 10 :font-size 20))
      (setf *p2-text* (add-text-to-exp-window :text (princ-to-string *p2-position*) :color 'blue :x 140 :y 10 :font-size 20))

      (add-button-to-exp-window :x 10 :y 60 :width 100 :height 30 :color 'gray :text "Stop" :action (lambda (b) 
                                                                                                     (declare (ignore b))
                                                                                                     (setf *game-over* t))))
    ;; Have all models process the visual scene
    
    (dolist (m (mp-models))
      (with-model-eval m
        (proc-display)))
    
    ;; speak to all models telling them the name
    ;; of the first player.
    
    (dolist (m (mp-models))
      (with-model-eval m
        (new-word-sound (string *p1*) 0 'start)))))


;; On a valid keypress update the game assuming a 1 step move
;; for any key that's not 2.

(defmethod rpm-window-key-event-handler ((device rpm-window) key)
  (let ((model (model-generated-action))) ;; record who made the action
    
    ;; if it's the correct player
    (when (or (and model (eq model *current-player*))
              (and (not model) (eq *current-player* 'human)))
      
      ;; Note that for some native interface windows (like those in ACL, LispWorks, or CCL)
      ;; it is possible for a model to make an action but not have model-generated-action
      ;; be true since it might be a separate GUI thread handling the real window input.
      ;; That will never happen for virtual windows or experiment windows shown through
      ;; the ACT-R Environment (i.e. visible virtual windows).
      ;; If one is using such a system, then something like the code for the human player
      ;; might be required to make sure the processing occurs in the same thread that is
      ;; running ACT-R.
      
      (if model
          ;; Handle model actions immediately
          (update-game-state model key)
        ;; For human interaction set a flag so the
        ;; ACT-R event can handle the update
        (setf *human-action* key)))))

;; Function which actually updates the game information
;; based on a key being pressed.

(defun update-game-state (model key)
  
  ;; determine the distance
  (let ((move (cond ((eql key #\1) 1)
                    ((eql key #\2) 2)
                    (t 1))))
    
    ;; update the position
    (if (eq *current-player* *p1*)
        (incf *p1-position* move)
      (decf *p2-position* move))
    
    (if (<= *p2-position* *p1-position*)
        ;; if there's a winner
        (progn
          ;; save the winning player and clear the current player
          (setf *winner* *current-player*)
          (setf *current-player* nil)
          
          (with-model human ; since that's who owns the window
            ;; remove the text
            (remove-items-from-exp-window *p1-text* *p2-text*)
            ;; display the winner
            (add-text-to-exp-window :text (string *winner*) :x 60 :y 20 :font-size 20 :color 'green)
            ;; schedule the game over event
            (schedule-event-relative 3 'game-over)))
      
      ;; not a winner so update the display with the new position
      (if (eq *current-player* *p1*)
          (with-model human
            (remove-items-from-exp-window *p1-text*)
            (setf *p1-text* (add-text-to-exp-window :text (princ-to-string *p1-position*) :color 'red :x 20 :y 10 :font-size 20))
            (setf *current-player* *p2*))
        (with-model human
          (remove-items-from-exp-window *p2-text*)
          (setf *p2-text* (add-text-to-exp-window :text (princ-to-string *p2-position*) :color 'blue :x 140 :y 10 :font-size 20))
          (setf *current-player* *p1*)))))
      
  (when (eq model 'human) ;; if it was a person say done automatically
    (with-model human
      (device-speak-string 
       (current-device) ;; the currently installed device for a model
       "done")))
  
  ;; have all models process the display
  (dolist (m (mp-models))
    (with-model-eval m
      (proc-display))))


;; a function to set the game-over value instead of just
;; using a lambda in the event.

(defun game-over ()
  (setf *game-over* *winner*))

;; the method that gets called when a model speaks

(defmethod device-speak-string ((device rpm-window) string)
  (let ((originator (current-model))) ; record which model spoke
    (dolist (model (mp-models))
      (unless (eq model (current-model)) ; for everyone but the originator
        
        (with-model-eval model ; make the other model the current-model
          
          ;; Create the originating model's name as a simple chunk if
          ;; it isn't one already to avoid a warning of it being
          ;; created by default when the sound is generated.
          
          (unless (chunk-p-fct originator)
            (define-chunks-fct `((,originator isa chunk))))
          
          ;; Create the sound as a word with location indicating
          ;; the speaker.
          
          (new-word-sound string (mp-time) originator))))))

;; Function to process human player actions as an event in
;; the model running since they're actually being generated
;; asynchronously (and possibly in a different Lisp thread).
      
(defun process-human-input ()
  (allow-event-manager *window*)
  (when *human-action*
    (update-game-state 'human *human-action*)
    (setf *human-action* nil)))


;; Function to play one game resetting the models
;; before it starts.  Player1 and player2 are model
;; names or the symbol human.
      
(defun play (player1 player2)
  
  (reset)
  
  ;; only allow 1 human player
  (when (and (eq player1 'human) (eq player2 'human))
    (setf player2 'model1))

  (unless (eq player1 'human)
    (with-model-eval player1
      ;; create a goal chunk with the player's color and name  
      (define-chunks-fct (list (list 'goal 'isa 'play 'my-color 'red 'my-name (string player1))))
      (goal-focus goal)))
  
  (unless (eq player2 'human)
    (with-model-eval player2
      ;; create a goal chunk with the player's color and name
      (define-chunks-fct (list (list 'goal 'isa 'play 'my-color 'blue 'my-name (string player2))))
      (goal-focus goal)))
  
  ;; schedule an interface processing event if there's a human player
  (when (or (eq player1 'human) (eq player2 'human))
    (with-model human
      (schedule-periodic-event .5 'process-human-input :output nil)))
  
  
  ;; initialize some game info
  
  (setf *p1* player1)
  (setf *p2* player2)
  (setf *game-over* nil)
  (setf *current-player* player1)
  (setf *human-action* nil)
  
  ;; set the models' focus rings to different colors
  ;; if supported by the current display window.
  ;; Not particularly useful for this interface since
  ;; they both only look at the same spot at the
  ;; end, but in other situations could be quite
  ;; helpful.
  
  (with-model model1 (sgp :show-focus red))
  (with-model model2 (sgp :show-focus blue))
  
  ;; create the initial interface 
  (start-game)
  
  (run-until-condition (lambda () *game-over*) :real-time t)
  
  *game-over*)


(clear-all)

(define-model human (sgp :v nil :needs-mouse nil)) ;; dummy model for creating window and human player delay events


(defparameter *model-code*
    '((sgp :v t :trace-detail high :needs-mouse nil :er t)
      
      (chunk-type play my-color my-name state)
      
      (set-audloc-default - location self :attended nil)
      (set-visloc-default color green)
      
      (define-chunks (move) (say-done))
      
      (declare-buffer-usage goal play :all)
      
      (p game-over
         =visual-location>
         ?visual>
           state free
       ==>
         +visual>
           cmd move-attention
           screen-pos =visual-location
         -goal>)
      
      (p hear-something
         =aural-location>
         ?aural>
           state free
       ==>
         +aural>
           event =aural-location)
      
      (p my-turn-to-start
         =goal>
           my-name =name
         =aural>
           content =name
       ==>
         =goal>
           state move)
      
      (p other-player-done
         =goal>
         =aural>
           content "done"
       ==>
         =goal>
           state move)
      
      (p other-player-starts
         =goal>
           my-name =name
         =aural>
          - content =name
          - content "done"
       ==>
         )

      (p 1-step
         =goal>
           state move
         ?manual>
           state free
       ==>
         +manual>
           cmd press-key
           key 1
         =goal>
           state say-done)
      
      (p 2-step
         =goal>
           state move
         ?manual>
           state free
       ==>
         +manual>
           cmd press-key
           key 2
         =goal>
           state say-done)
      
      (p say-done
         =goal>
           state say-done
         ?vocal>
           state free
         ?manual>
           state free
       ==>
         +vocal>
           cmd speak 
           string "done"
         =goal>
           state nil)))
      
(define-model-fct 'model1 *model-code*)
(define-model-fct 'model2 *model-code*)
