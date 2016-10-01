
;;; Play the capture game with each model having its own interface
;;; showing only the opponent's position.  The game says the starting
;;; player, players press a key to indicate a move, and models are sensitive
;;; to scene changes (not visual-location stuffing) to know when to
;;; take a turn.


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

;; Initialize the game information, create a window in each model, display
;; the initial values and say the starting player's name to each model.

(defun start-game ()
  (setf *p1-position* 0)
  (setf *p2-position* 5)
  
  ;; create a window in player 1
  ;; with the text for player 2's postion
  ;; and save that window to use if there's a human player
  
  (with-model-eval *p1*
      
    (setf *window* (install-device (open-exp-window "game" :height 100 :width 200 :x 20 :y 50 :visible t)))
    
    (setf *p1-text* (add-text-to-exp-window :text (princ-to-string *p2-position*) :x 20 :y 10 :font-size 20))
  
    (add-button-to-exp-window :x 10 :y 60 :width 80 :height 30 :color 'gray :text "Stop" :action (lambda (b) 
                                                                                                   (declare (ignore b))
                                                                                                   (setf *game-over* t)))
    (proc-display))
  
  (with-model-eval *p2*
      
    (install-device (open-exp-window "game" :height 100 :width 200 :x 20 :y 250 :visible t))
      
    (setf *p2-text* (add-text-to-exp-window :text (princ-to-string *p1-position*) :x 20 :y 10 :font-size 20))
  
    (add-button-to-exp-window :x 10 :y 60 :width 80 :height 30 :color 'gray :text "Stop" :action (lambda (b) 
                                                                                                   (declare (ignore b))
                                                                                                   (setf *game-over* t)))
    (proc-display))    
  
    ;; speak to all models telling them the name
    ;; of the first player.
    
    (dolist (m (mp-models))
      (with-model-eval m
        (new-word-sound (string *p1*) 0 'start))))


;; To reduce the code in the update-game-state function have another
;; function that updates the display for either player.

(defun update-display (player current-item text &optional (color 'black))
  (with-model-eval player
    (prog2
      (remove-items-from-exp-window current-item)
      (add-text-to-exp-window :text text :x 20 :y 20 :font-size 20 :color color)
      (proc-display))))
      
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
          
          ;; update the displays
          
          (update-display *p1* *p1-text* (if (eq *p1* *winner*) "win" "lose") 'green)
          (update-display *p2* *p2-text* (if (eq *p2* *winner*) "win" "lose") 'green)
          
          ;; schedule the game-over event in player 1
          
          (with-model-eval *p1*
            (schedule-event-relative 3 'game-over)))
      
      ;; not a winner so update the display with the new position
      ;; only the display of the opposite player needs to change
      (if (eq *current-player* *p1*)
          (progn
            (setf *p2-text* (update-display *p2* *p2-text* (princ-to-string *p1-position*)))
            (setf *current-player* *p2*))
        (progn
          (setf *p1-text* (update-display *p1* *p1-text* (princ-to-string *p2-position*)))
          (setf *current-player* *p1*))))))


;; a function to set the game-over value instead of just
;; using a lambda in the event.

(defun game-over ()
  (setf *game-over* *winner*))

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
      ;; create a goal chunk with the player's name and starting position
      (define-chunks-fct (list (list 'goal 'isa 'play 'position 0 'my-name (string player1))))
      (goal-focus goal)))
  
  (unless (eq player2 'human)
    (with-model-eval player2
      ;; create a goal chunk with the player's name and starting position
      (define-chunks-fct (list (list 'goal 'isa 'play 'position 5 'my-name (string player2))))
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
  
  ;; create the initial interface 
  (start-game)
  
  (run-until-condition (lambda () *game-over*) :real-time t)
  
  *game-over*)


(clear-all)

(define-model human (sgp :v nil :needs-mouse nil)) ;; dummy model for creating human interface 


(defparameter *model-code*
    '((sgp :v t :trace-detail high :needs-mouse nil :er t)
      
      ;; adjust the sensitivty to make sure the model
      ;; repsonds to any change in the visual scene
      
      (sgp :scene-change-threshold 0)
      
      (chunk-type play position my-name state)
      
      (define-chunks (start) (play) (move) (attend) (read))
      
      (declare-buffer-usage goal play :all)
      
      
      (p hear-something
         =goal>
           state nil
         =aural-location>
         ?aural>
           state free
         ?visual>
           state free
       ==>
         =goal>
           state start
         +aural>
           event =aural-location
         
         ;; ignore the scene change that occurred
         +visual>
           cmd clear)
      
      
      (p my-turn-to-start
         =goal>
           my-name =name
           state   start
         =aural>
           content =name
       ==>
         =goal>
           state move)
      
      (p other-player-starts
         =goal>
           my-name =name
           state   start
         =aural>
          - content =name
       ==>
         =goal>
           state play)
      
      
      (p do-something
         ;; If the visual scene has changed
         ?visual>
           scene-change t
           state free
         =goal>
           state play
       ==>
         ;; there's only one thing to see
         +visual-location>
         
         ;; clear the scene change
         +visual>
           cmd clear
         
         =goal>
           state attend)
      
      (p attend
         =goal>
           state attend
         =visual-location>
         ?visual>
           state free
       ==>
         =goal>
           state read
         +visual>
           cmd move-attention
           screen-pos =visual-location)
      
      (p lost
         =visual>
           value "lose"
         =goal>
           state read
       ==>
         -goal>)
      
      (p won
         =visual>
           value "win"
         =goal>
           state read
       ==>   
         -goal>)
      
      (p my-turn
         =visual>
           - value "win"
           - value "lose"
         =goal>
           state read
       ==>   
         =goal>
           state move)

      
      (p 1-step
         =goal>
           state move
           position =pos
         ?manual>
           state free
       ==>
         ;; should update position internally if really playing  
         +manual>
           cmd press-key
           key 1
         =goal>
           state play)
      
      (p 2-step
         =goal>
           state move
           position =pos
         ?manual>
           state free
       ==>
         ;; should update position internally if really playing  
         +manual>
           cmd press-key
           key 2
         =goal>
           state play)))
      
(define-model-fct 'model1 *model-code*)
(define-model-fct 'model2 *model-code*)
