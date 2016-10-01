(defvar *response* nil)
(defvar *response-time* nil)
(defvar *model-doing-task* nil)

(defvar *pairs* '(("bank" "0") ("card" "1") ("dart" "2") ("face" "3") ("game" "4")
                  ("hand" "5") ("jack" "6") ("king" "7") ("lamb" "8") ("mask" "9")
                  ("neck" "0") ("pipe" "1") ("quip" "2") ("rope" "3") ("sock" "4")
                  ("tent" "5") ("vent" "6") ("wall" "7") ("xray" "8") ("zinc" "9")))

;; Human results?
(defvar *paired-latencies* '(0.0 2.158 1.967 1.762 1.680 1.552 1.467 1.402))
(defvar *paired-probability* '(0.000 .526 .667 .798 .887 .924 .958 .954))

(defun paired-task (size trials &optional who)
  (if (not (eq who 'human))
      (do-experiment-model size trials)
    (do-experiment-person size trials)))

(defun do-experiment-model (size trials)
  (let ((result nil)
        (window (open-exp-window "Paired-Associate Experiment" :visible nil)))
    
    (setf *model-doing-task* t)
    (reset) 
    
    (install-device window)
    
    (dotimes (i trials) 
      (let ((score 0.0)
            (time 0.0)
            (start-time))
        (dolist (x (permute-list (subseq *pairs* (- 20 size)))) 
          
          (clear-exp-window)
          (add-text-to-exp-window :text (car x) :x 150 :y 150)
        
          (setf *response* nil)                   
          (setf *response-time* nil)
          (setf start-time (get-time))
          
          (proc-display)
          (run-full-time 5)
          
          (when (equal (second x) *response*)      
            (incf score 1.0)    
            (incf time (- *response-time* start-time))) 
        
          (clear-exp-window)
          (add-text-to-exp-window :text (second x) :x 150 :y 150)
          
          (proc-display)
          (run-full-time 5))
        
        (push (list (/ score size) (and (> score 0) (/ time (* score 1000.0)))) result)))
    
    (reverse result)))
  
(defun do-experiment-person (size trials)
  (let ((result nil)
        (window (open-exp-window "Paired-Associate Experiment" :visible t)))
    
    (setf *model-doing-task* nil)
    (dotimes (i trials) 
      (let ((score 0.0)
            (time 0.0)
            (start-time))
        (dolist (x (permute-list (subseq *pairs* (- 20 size)))) 
          
          (clear-exp-window)
          (add-text-to-exp-window :text (car x) :x 150 :y 150 :width 50)
          (setf *response* nil)                   
          (setf *response-time* nil)
          
          (setf start-time (get-time nil)) 
          (while (< (- (get-time nil) start-time) 5000)
                 (allow-event-manager window)) 
          
          (when (equal (second x) *response*)
            (incf score 1.0) 
            (incf time (/ (- *response-time* start-time) 1000.0))) 
        
          (clear-exp-window)
          (add-text-to-exp-window :text (second x) :x 150 :y 150)
          (sleep 5.0))
        
        (push (list (/ score size) (and (> score 0) (/ time score))) result)))
    
    ;; return the list of scores 
    (reverse result)))

(defun paired-experiment (n)
  (do ((count 1 (1+ count))
       (results (paired-task 20 8)
                (mapcar (lambda (lis1 lis2)
                          (list (+ (first lis1) (first lis2))
                                (+ (or (second lis1) 0) (or (second lis2) 0))))
                  results (paired-task 20 8))))
      ((equal count n) 
       (output-data results n))))

(defun output-data (data n)
  (let ((probability (mapcar (lambda (x) (/ (first x) n)) data))
        (latency (mapcar (lambda (x) (/ (or (second x) 0) n)) data)))
    (print-results latency *paired-latencies* "Latency")
    (print-results probability *paired-probability* "Accuracy")))

(defun print-results (predicted data label)
 (format t "~%~%~A:~%" label)
  (correlation predicted data)
  (mean-deviation predicted data)
  (format t "Trial    1       2       3       4       5       6       7       8~%")
  (format t "     ~{~8,3f~}~%" predicted))


(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (setf *response* (string-upcase (string key)))
  (setf *response-time* (get-time *model-doing-task*)))

(clear-all)

(define-model paired-copy

;;; MODEL SETTINGS
;	 :v 	- Trace
; 	 :esc - Subsymbolic compotent (should be turned on)
;	 :rt  - Minimum retrievable chunk activation
;	 :lf  - Latency factor
;   :ans - Instantaneous noise of the activation
;   :bll - Decay parameter
;	 :act - activation trace parameter
;   :ncnar - Normalize chunk names after run???s
(sgp :v nil :esc t :rt -2 :lf 0.4 :ans 0.5 :bll 0.5 :act t :ncnar nil)
(sgp :seed (200 4))

;;; CHUNKS AND CHUNK-TYPES
(chunk-type goal state)
(chunk-type pair probe answer)

(add-dm
	(start isa chunk) (attending-target isa chunk)
	(attending-probe isa chunk)
	(testing isa chunk) (read-study-item isa chunk)

	(goal isa goal state start))

;;; PRODUCTIONS

(p attend-probe
	=goal>
		isa 	goal
		state start
	=visual-location>
	?visual>
		state free
==>
	+visual>
		cmd			move-attention
		screen-pos  =visual-location
	=goal>
		state			attending-probe
)

(p read-probe
	=goal>
		isa	goal
		state attending-probe
	=visual>
		isa	visual-object
		value =val
	?imaginal>
		state free
==>
	+imaginal>
		isa	pair
		probe =val
	+retrieval>
		isa	pair
		probe =val
	=goal>
		state testing
)

(p recall
	=goal>
		isa	goal
		state testing
	=retrieval>
		isa   pair
		answer =ans
	?manual>
		state free
	?visual>
		state free
==>
	+manual>
		cmd   press-key
		key	=ans
	=goal>
		state	read-study-item
	+visual>
		cmd   clear
)

(p cannot-recall
	=goal>
		isa	goal
		state testing
	?retrieval>
		buffer failure
	?visual>
		state	 free
==>
	=goal>
		state	read-study-item
	+visual>
		cmd	clear
)

(p detect-study-item
	=goal>
		isa	goal
		state	read-study-item
	=visual-location>
	?visual>
		state	free
==>
	+visual>
		cmd	move-attention
		screen-pos =visual-location
	=goal>
		state	attending-target
)

(p associate
	=goal>
		isa   goal
		state	attending-target
	=visual>
		isa	visual-object
		value =val
	=imaginal>
		isa	pair
		probe =probe
	?visual>
		state	free
==>
	=imaginal>
		answer =val
	-imaginal>
	=goal>
		state start
	+visual>
		cmd	clear
)

(goal-focus goal)
)