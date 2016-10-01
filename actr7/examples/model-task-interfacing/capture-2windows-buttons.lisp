;; Play the simple capture game with each model having its own
;; window and each player plays left->right as red in its own window.


;; global variables 

(defvar *current-player*)  ;; player name
(defvar *p1*)              ;; player 1 name
(defvar *p2*)              ;; player 2 name
(defvar *game-over*)       ;; stop game when true
(defvar *p1-position*)     ;; current player positions
(defvar *p2-position*)
(defvar *spaces-1* (make-list 6 :initial-element nil)) ;; separate buttons for each player
(defvar *spaces-2* (make-list 6 :initial-element nil))

(defvar *window*)          ;; save a window for human interaction
(defvar *human-action*)    ;; a flag to indicate an external action needs to be processed

;; initialize the game info and draw each player's starting board
;; making sure to reverse it for player 2 so both look like playing
;; from the left

(defun start-game ()
  (setf *p1-position* 0)
  (setf *p2-position* 5)

  (with-model-eval *p1*  ;; for first player
    
    ;; create a window and install it as the device and
    ;; save it for possible human interaction 
    (setf *window* (install-device (open-exp-window "game" :height 100 :width 300 :x 10 :y 50 :visible t)))
        
    ;; draw the buttons and record them in player 1's list
    (dotimes (i 6)
      (setf (nth i *spaces-1*)
        (let ((index i)) ;; necessary to capture current i value for the lambda
          (add-button-to-exp-window :x (+ 10 (* i 40)) 
                                    :y 10 
                                    :width 35 :height 35 
                                    :color (if (zerop i) 'red 'white) 
                                    :text (if (zerop i) "1" (if (= i 5) "2" "")) 
                                    :action (lambda (b) (pick-button b index))))))
    
    ;; add the safety stop
    (add-button-to-exp-window :x 10 :y 60 :width 100 :height 30 :color 'gray 
                              :text "Stop" :action (lambda (b) (declare (ignore b)) (setf *game-over* t)))
    
    ;; have the model process the window
    (proc-display))
  
  (with-model-eval *p2* ;; same for player 2 but reverse the button positions
    
    (install-device (open-exp-window "game" :height 100 :width 300 :x 10 :y 200 :visible t))
    
    (dotimes (i 6)
      (setf (nth i *spaces-2*)
        (let ((index (- 5 i)))
          (add-button-to-exp-window :x (+ 10 (* i 40)) 
                                    :y 10 
                                    :width 35 :height 35 
                                    :color 'white 
                                    :text (if (zerop i) "1" (if (= i 5) "2" "")) 
                                    :action (lambda (b) (pick-button b index))))))
    
    (add-button-to-exp-window :x 10 :y 60 :width 100 :height 30 :color 'gray 
                              :text "Stop" :action (lambda (b) (declare (ignore b)) (setf *game-over* t)))
    
    (proc-display)))

;; The function to call when a button is pressed.
;; If the current player made the move, update the state 
;; immediately for a model and set a flag to indicate human input
;; for processing by the event.

(defun pick-button (b index)
  (declare (ignore b))
  
  ;; if the current player makes the action 
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
   
    
    
;; Update both players' boards when a button is pressed.
;; Could be abstracted a little to clean things up, but 
;; keeping it fairly explicit for example purposes.
    

(defun update-game-state (index)
    
  (if (eq *current-player* *p1*) 
      
      ;; update the boards when player1 makes a valid move
      (when (and (> index *p1-position*) (<= (- index *p1-position*) 2))
        
        ;; remove the player's current button from both windows
        
        (with-model-eval *p1*
          (remove-items-from-exp-window (nth *p1-position* *spaces-1*) (nth index *spaces-1*)))
        (with-model-eval *p2*
          (remove-items-from-exp-window (nth (- 5 *p1-position*) *spaces-2*) (nth (- 5 index) *spaces-2*)))
        
        (if (>= index *p2-position*)
            
            (progn ;; player 1 wins
              
              ;; give the models some time to process things before it stops
              
              (with-model-eval *p1*
                (schedule-event-relative 3 (lambda () (setf *game-over* *p1*))))
              
              ;; update player1's board 
              ;; and have the model process it
              
              (with-model-eval *p1*
                (add-button-to-exp-window :x (+ 10 (* *p1-position* 40)) :y 10 :width 35 :height 35 :color 'white :text "")
                (add-button-to-exp-window :x (+ 10 (* index 40)) :y 10 :width 35 :height 35 :color 'green :text "1")
                (proc-display))
              
              ;; update player2's board which is opposite
              ;; and have the model process it
              (with-model-eval *p2*
                (add-button-to-exp-window  :x (+ 10 (* (- 5 *p1-position*) 40)) :y 10 :width 35 :height 35 :color 'white :text "")
                (add-button-to-exp-window  :x (+ 10 (* (- 5 index) 40)) :y 10 :width 35 :height 35 :color 'green :text "2")
                (proc-display)))
          
          (progn  ;; non-winning player 1 move
            
            ;; update player1 board
            
            (with-model-eval *p1*
              (let ((i *p1-position*))
                (setf (nth *p1-position* *spaces-1*) 
                  (add-button-to-exp-window :x (+ 10 (* *p1-position* 40)) 
                                            :y 10 
                                            :width 35 :height 35 
                                            :color 'white :text "" 
                                            :action (lambda (b) (pick-button b i)))))
              (setf (nth index *spaces-1*) 
                (add-button-to-exp-window :x (+ 10 (* index 40)) 
                                          :y 10 
                                          :width 35 :height 35 
                                          :color 'white :text "1" 
                                          :action (lambda (b) (pick-button b index))))
              (proc-display))
            
            ;; update player2 board
            
            (with-model-eval *p2*
              (let ((i *p1-position*))
                (setf (nth (- 5 *p1-position*) *spaces-2*) 
                  (add-button-to-exp-window :x (+ 10 (* (- 5 *p1-position*) 40)) 
                                            :y 10 
                                            :width 35 :height 35 
                                            :color 'white :text "" 
                                            :action (lambda (b) (pick-button b i)))))
              (setf (nth (- 5 index) *spaces-2*) 
                (add-button-to-exp-window :x (+ 10 (* (- 5 index) 40)) 
                                          :y 10 
                                          :width 35 :height 35 
                                          :color 'white :text "2" 
                                          :action (lambda (b) (pick-button b index))))
              
              ;; it's now player2's turn so make the button red
              
              (remove-items-from-exp-window (nth (- 5 *p2-position*) *spaces-2*))
              (let ((i *p2-position*))
                (setf (nth (- 5 *p2-position*) *spaces-2*) 
                  (add-button-to-exp-window :x (+ 10 (* (- 5 *p2-position*) 40)) 
                                            :y 10 
                                            :width 35 :height 35 
                                            :color 'red :text "1" 
                                            :action (lambda (b) (pick-button b i)))))
              (proc-display))
            
            (setf *p1-position* index)
            
            (setf *current-player* *p2*))))
    
    ;; player 2 makes a valid move
    (when (and (< index *p2-position*) (<= (- *p2-position* index) 2))
      
      (with-model-eval *p1*
        (remove-items-from-exp-window (nth *p2-position* *spaces-1*) (nth index *spaces-1*)))
      (with-model-eval *p2*
        (remove-items-from-exp-window (nth (- 5 *p2-position*) *spaces-2*) (nth (- 5 index) *spaces-2*)))
      
      (if (<= index *p1-position*)
          (progn 
            (with-model-eval *p2*
              (schedule-event-relative 3 (lambda () (setf *game-over* *p2*))))
            
            (with-model-eval *p1*
              (add-button-to-exp-window :x (+ 10 (* *p2-position* 40)) :y 10 :width 35 :height 35 :color 'white :text "")
              (add-button-to-exp-window :x (+ 10 (* index 40)) :y 10 :width 35 :height 35 :color 'green :text "2")
              (proc-display))
            
            (with-model-eval *p2*
              (add-button-to-exp-window  :x (+ 10 (* (- 5 *p2-position*) 40)) :y 10 :width 35 :height 35 :color 'white :text "")
              (add-button-to-exp-window  :x (+ 10 (* (- 5 index) 40)) :y 10 :width 35 :height 35 :color 'green :text "1")
              (proc-display)))
        (progn 
          (with-model-eval *p1*
            (let ((i *p2-position*))
              (setf (nth *p2-position* *spaces-1*) 
                (add-button-to-exp-window :x (+ 10 (* *p2-position* 40)) 
                                          :y 10 
                                          :width 35 :height 35 
                                          :color 'white :text "" 
                                          :action (lambda (b) (pick-button b i)))))
            (setf (nth index *spaces-1*) 
              (add-button-to-exp-window :x (+ 10 (* index 40)) 
                                        :y 10 
                                        :width 35 :height 35 
                                        :color 'white :text "2" 
                                        :action (lambda (b) (pick-button b index))))
            
            (remove-items-from-exp-window (nth *p1-position* *spaces-1*))
            (let ((i *p1-position*))
              (setf (nth *p1-position* *spaces-1*) 
                (add-button-to-exp-window :x (+ 10 (* *p1-position* 40)) 
                                          :y 10 
                                          :width 35 :height 35 
                                          :color 'red :text "1" 
                                          :action (lambda (b) (pick-button b i)))))
            (proc-display))
          
          (with-model-eval *p2*
            (let ((i *p2-position*))
              (setf (nth (- 5 *p2-position*) *spaces-2*) 
                (add-button-to-exp-window :x (+ 10 (* (- 5 *p2-position*) 40)) 
                                          :y 10 
                                          :width 35 :height 35 
                                          :color 'white :text "" 
                                          :action (lambda (b) (pick-button b i)))))
            (setf (nth (- 5 index) *spaces-2*) 
              (add-button-to-exp-window :x (+ 10 (* (- 5 index) 40)) 
                                        :y 10 
                                        :width 35 :height 35 
                                        :color 'white :text "1" 
                                        :action (lambda (b) (pick-button b index))))
            (proc-display))
          
          (setf *p2-position* index)
          
          (setf *current-player* *p1*))))))
      

;; Function to process human player actions as an event in
;; the model running since they're actually being generated
;; asynchronously (and possibly in a different Lisp thread).
      
(defun process-human-input ()
  (allow-event-manager *window*)
  (when *human-action*
    (update-game-state *human-action*)
    (setf *human-action* nil)))


;; play one round of the game resetting models
;; before it starts.  Player1 and player2 are the
;; names of models to run, or the symbol human for
;; one of them.

(defun play (player1 player2)
  
  (reset)
  
  ;; if there's a human player create a periodic event
  ;; to record input with some window -- doesn't actually
  ;; matter which one since all human actions can occur
  ;; in any window.
  
  (when (or (eq player1 'human) (eq player2 'human))
    (with-model human
      (schedule-periodic-event .5 'process-human-input :output nil)))
  
  ;; Can't have two human players in this version
  
  (when (and (eq player1 'human) (eq player2 'human))
    (setf player2 'model1))
    
  (setf *p1* player1)
  (setf *p2* player2)
  
  (setf *game-over* nil)
  (setf *current-player* player1)
  (setf *human-action* nil)
  
  (start-game)
  
  (run-until-condition (lambda () *game-over*) :real-time t)
  *game-over*)
          
(clear-all)

(define-model human (sgp :v nil :needs-mouse nil)) ;; dummy model for creating window and human player delay events


(defparameter *model-code*
    '((sgp :v t :trace-detail high :needs-mouse nil)
      (chunk-type play my-color state)
      
      (define-chunks (goal isa play my-color red)
          (move) (click))
      
      (goal-focus goal)
      (declare-buffer-usage goal play :all)
      
      (set-visloc-default-fct '(kind oval color red))
      
      (start-hand-at-mouse)
      
      (p my-turn
         =goal>
           state nil
           my-color red
         =visual-location>
           screen-x =x
       ==>
         !bind! =limit (+ =x 100)
         +visual-location>
           kind oval
           color white
          > screen-x =x
          < screen-x =limit
         =goal>
           state move)
      
      (p move
         =goal>
           state move
         =visual-location>
         ?manual>
           state free
         ==>
         +manual>
           isa move-cursor
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
           isa click-mouse
         =goal>
           state nil)))
      
(define-model-fct 'model1 *model-code*)
(define-model-fct 'model2 *model-code*)
