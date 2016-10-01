;; Play the simple capture game with 2 models using no interface.
;; Goal chunk holds player and state information and imaginal holds board positions.

;; global variables for game info

(defvar *current-player*) ; name of the model
(defvar *winner*)         ; winning model
(defvar *p1*)             ; player1 model name
(defvar *p2*)             ; player2 model name
(defvar *game-over*)      ; If true stop the game

(defvar *p1-position*)    ; current player spots 0-5
(defvar *p2-position*)


;; function called directly by a model to make a move of dist spaces
;; Just schedule a call to a function that actually handles that after
;; everything else the model needs to do at this time.

(defun make-move (dist)
  (with-model-eval *current-player*
    (schedule-event-now 'process-move :params (list dist) :priority :min)))

;; Actually update the models' goal chunks as needed

(defun process-move (dist)
  (when (eq (current-model) *current-player*) ; make sure the right player is taking the turn
    
    (if (eq *current-player* *p1*) ; update the position
        (incf *p1-position* dist)
      (decf *p2-position* dist))
        
    (cond ((<= *p2-position* *p1-position*) ; the game is over
           (setf *winner* *current-player*)
           (setf *current-player* nil)      ; clear so no more moves allowed
           
           (with-model-eval *p1*
             (schedule-mod-buffer-chunk 'goal (list 'state 'game-over) 0)
             (schedule-mod-buffer-chunk 'imaginal (list 'p1 (if (eq *p1* *winner*) 'win 'lose) 'p2 (if (eq *p1* *winner*) 'lose 'win)) 0))
           
           (with-model-eval *p2*
             (schedule-mod-buffer-chunk 'goal (list 'state 'game-over) 0)
             (schedule-mod-buffer-chunk 'imaginal (list 'p1 (if (eq *p1* *winner*) 'win 'lose) 'p2 (if (eq *p1* *winner*) 'lose 'win)) 0))
           
           ;; schedule the game over event in some model, so why not winner
           
           (with-model-eval *winner*
             (schedule-event-relative 3 'game-over)))
          
          ((eq *current-player* *p1*) ;; player 1 moved so tell player 2 to go now
           
           ;; update both players' imaginal chunk
           (dolist (m (mp-models))
             (with-model-eval m
               (schedule-mod-buffer-chunk 'imaginal (list 'p1 *p1-position* 'p2 *p2-position*) 0)))
           
           ;; update player2's goal
           (with-model-eval *p2*
             (schedule-mod-buffer-chunk 'goal (list 'state 'move) 0))
           
           (setf *current-player* *p2*))
          
          (t  ;; player 2 moved so tell player 1 to go now
           
           ;; update both players' imaginal chunk
           (dolist (m (mp-models))
             (with-model-eval m
               (schedule-mod-buffer-chunk 'imaginal (list 'p1 *p1-position* 'p2 *p2-position*) 0)))
           
           ;; update player1's goal
           (with-model-eval *p1*
             (schedule-mod-buffer-chunk 'goal (list 'state 'move) 0))
           
           (setf *current-player* *p1*)))))


;; function that gets scheduled to set the *game-over* variable
;; which signals ACT-R to stop running.

(defun game-over ()
  (setf *game-over* *winner*))

;; play one round of the game resetting the models
;; before it starts.  Player1 and player2 are the
;; names of the models to run in the indicated 
;; position.

(defun play (player1 player2)
  
  ;make sure that both names are valid model names
  
  (when (every (lambda (x) (find x (mp-models))) (list player1 player2))
    
    (reset)
    
    ; For the player1 model create the buffer chunks and set them
    ; into the buffers
    
    (with-model-eval player1
      
      (define-chunks 
          (goal isa play me p1 opp p2 state move)
          (game isa board p1 0 p2 5))
      
      (goal-focus goal)
      
      ; use schedule-set-buffer-chunk to set the
      ; imaginal chunk 
      
      (schedule-set-buffer-chunk 'imaginal 'game 0))
    
    ;; same for player2 model with opposite position slots
    
    (with-model-eval player2
      (define-chunks 
          (goal isa play me p2 opp p1)
          (game isa board p1 0 p2 5))
      (goal-focus goal)
      (schedule-set-buffer-chunk 'imaginal 'game 0))
    
    ;; initialize the game information
    (setf *p1* player1)
    (setf *p2* player2)
    (setf *p1-position* 0)
    (setf *p2-position* 5)
    (setf *game-over* nil)
    (setf *current-player* player1)
    
    ;; Run ACT-R until the *game-over* variable is set to a non-nil value,
    ;; or more than 100 seconds have passed as a safety case if models get stuck.
    (run-until-condition (lambda () (or *game-over* (> (mp-time) 100))))
    
    ;; return the winner
    *game-over*))


(clear-all)

;; Create one copy of the model code using defparameter so that
;; it gets updated if we reload -- defvar doesn't re-evaluate the
;; value after the first time it is loaded.

(defparameter *model-code*
  
  ;; A list of information just like one would put in a 
  ;; define-model call.
  
  '((sgp :v t :trace-detail high :er t)
    
    (chunk-type play me opp state)
    (chunk-type board p1 p2)
    
    (define-chunks (done) (game-over) (move) (p1) (p2) (win) (lose))
    
    ;; Avoid any warnings about slot usage in the goal
    ;; buffer since the slots are being set by Lisp code
    
    (declare-buffer-usage goal play :all)
     
    
    (p 1-step
       =goal>
         me =me
         state move
       =imaginal>
         =me =val  ; use the variable as the slot name!
     ==>
       =imaginal>
       !output! (I am at position =val)
       
       ;; call the function to take the action
       !eval! (make-move 1)
       =goal>
         state done)
    
    (p 2-step
       =goal>
         me =me
         state move
       =imaginal>
         =me =val  ; use the variable as the slot name!
     ==>
       =imaginal>
       !output! (I am at position =val)
       
       ;; call the function to take the action
       !eval! (make-move 2)
       =goal>
       state done)
    
    (p result
       =goal>
         me =me
         state game-over
       =imaginal>
         =me =outcome  ; use the variable as the slot name!
     ==>
       !output! (I =outcome)
       
       =goal>
         state done)
    ))

;; Use the define-model function to create models named
;; model1 and model2 each using the same definition code.

(define-model-fct 'model1 *model-code*)
(define-model-fct 'model2 *model-code*)
