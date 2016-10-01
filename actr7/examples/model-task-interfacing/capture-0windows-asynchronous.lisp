;; Play the simple capture game with 2 models in separate meta-processes using no interface.
;; Goal chunk holds player and state information and imaginal holds board positions.
;; Play order is fixed with the player1 model always going first.
;; Won't work with ACT-R Environment.

;; global variables for game info

(defvar *current-player*) ; name of the model
(defvar *game-over*)      ; If true stop the game

(defvar *p1-position*)    ; current player spots 0-5
(defvar *p2-position*)

(defvar *model-move*)     ; record the action that a model makes

;; function called directly by a model to make a move of dist spaces
;; Just record that press for the play function to use.

(defun make-move (dist)
  (setf *model-move* dist))


;; function that gets scheduled to set the *game-over* variable
;; which signals ACT-R to stop running.

(defun game-over ()
  (setf *game-over* *winner*))

;; play one round of the game resetting the models
;; before it starts.  Player1 and player2 are the
;; names of the models to run in the indicated 
;; position.

(defun play ()
  
  ;; reset all of the meta-processes
  
  (dolist (mp (meta-process-names))
    (with-meta-process-eval mp
      (reset)))
    
  ; For both player models create the buffer chunks and set them
  ; into the buffers
    
  (dolist (mp (list 'player1 'player2))
    (with-meta-process-eval mp
      (define-chunks-fct
          (list (list 'goal 'isa 'play 'me (if (eq mp 'player1) 'p1 'p2) 'opp (if (eq mp 'player1) 'p2 'p1))
                '(game isa board p1 0 p2 5)))
      ;; just set them here since they're
      ;; going to be modified by an event below
      
      (set-buffer-chunk 'goal 'goal)
      (set-buffer-chunk 'imaginal 'game)))
  
  ;; initialize the game information
  (setf *p1-position* 0)
  (setf *p2-position* 5)
  (setf *game-over* nil)
  (setf *current-player* 'player1)
  
  (loop
    ;; take turns until someone wins
    ;; no real safety net here, but there
    ;; could be some things added.
    
    (when *game-over* (return))
    
    (setf *model-move* nil)
    
    (with-meta-process-eval *current-player*
      ;; update the goal and imaginal chunks
      
      (schedule-mod-buffer-chunk 'goal (list 'state 'move) 0)
  
      (schedule-mod-buffer-chunk 'imaginal (list 'p1 *p1-position* 'p2 *p2-position*) 0)
      
      (run 100)) ;; assume the model makes a move and stops when it's done 
    
    (if (null *model-move*)
        (progn
          (format t "Model ~a failed to make a move so stopping~%" *current-player*)
          (return))
      (progn
        (if (eq *current-player* 'player1) ; update the position
            (incf *p1-position* *model-move*)
          (decf *p2-position* *model-move*))
        
        (cond ((<= *p2-position* *p1-position*) ; the game is over
               
               (setf *game-over* *current-player*)
               
               (with-meta-process player1
                 (schedule-mod-buffer-chunk 'goal (list 'state 'game-over) 0)
                 (schedule-mod-buffer-chunk 'imaginal (list 'p1 (if (eq *current-player* 'player1) 'win 'lose) 
                                                            'p2 (if (eq *current-player* 'player1) 'lose 'win)) 0)
                 (run-full-time 3))
           
               (with-meta-process player2
                 (schedule-mod-buffer-chunk 'goal (list 'state 'game-over) 0)
                 (schedule-mod-buffer-chunk 'imaginal (list 'p1 (if (eq *current-player* 'player1) 'win 'lose) 
                                                            'p2 (if (eq *current-player* 'player1) 'lose 'win)) 0)
                 (run-full-time 3)))
          
          ((eq *current-player* 'player1) ;; player 1 moved so switch to player 2
           (setf *current-player* 'player2))
          
          (t  ;; player 2 moved so switch to player1
           (setf *current-player* 'player1))))))

    ;; return the winner
    *game-over*)


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

;; Create the separate meta-processes

(define-meta-process player1)
(define-meta-process player2)

;; Create the models within those meta-processes, and
;; each can have the same name since they're separate.

(with-meta-process player1
  (define-model-fct 'model *model-code*))

(with-meta-process player2
  (define-model-fct 'model *model-code*))
