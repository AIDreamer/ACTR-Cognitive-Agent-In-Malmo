;;; This model is a simple demonstration of creating and using 
;;; a new virtual dialog item.  It assumes that the image-vdi
;;; class implemented in the creating-a-new-virtual-item.lisp
;;; file is available, and that the images directory and 
;;; 999-creating-a-new-virtual-item.tcl file have been added
;;; to the Environment if a visible window is used.
;;;
;;; To run the model call the run-test function.  It has one
;;; optional parameter which if provided as any non-nil value 
;;; indicates the window should be visible.  The default is
;;; to use a virtual window.  A trace of the virtual run is
;;; included as a comment at the end of the file.

;;; The model attends to the items it sees in the window from
;;; top to bottom and for each one moves the mouse to a position
;;; slightly offset from the true location of the item and then
;;; clicks the mouse.


(clear-all)

(defun run-test (&optional (visible nil))
  ;; create an experiment window as normal
  (install-device (open-exp-window "image test" :visible visible :width 310 :height 320))
  
  ;; explicitly add instances of the new item to that window
  (add-items-to-exp-window 
   
   ;; here we create our new items assuming that the corresponding files
   ;; are available if we intend to have them shown in a visible window.
   ;; the dialog-item-text is the value which the model will see when it
   ;; attends to the image.
   
   ;; The first one does not provide an action function and thus the default
   ;; action of printing the info when :vwt is true will be used.
   
   (make-instance 'image-vdi :file "smalllogo.gif" :dialog-item-text "logo" :x-pos 10 :y-pos 10 :width 288 :height 142)
   
   ;; The second one includes a custom action function that just outputs the
   ;; click information.
   
   (make-instance 'image-vdi :file "ref-brain.gif" :dialog-item-text "brain" :x-pos 10 :y-pos 160 :width 128 :height 128
     :action (lambda (image pos)
               (declare (ignore image))
               (model-output "~s clicked image at ~D ~D" (aif (model-generated-action) it 'human) (px pos) (py pos)))))
  
  ;; process the display as normal
  (proc-display)
  
  ;; print the visicon to see how the new items are represented
  (print-visicon)
  
  ;; run for up to 5 seconds using real-time if the window is visible
  (run 5 :real-time visible))



(define-model image-testing-model

  (sgp :v t :vwt t :show-focus t)
  
  (start-hand-at-mouse)

  (set-visloc-default screen-y lowest)
  
  (p attend
     "attend to anything placed into the visual-location buffer"
     =visual-location>
     ?visual>
       state free
   ==>
     +visual>
       cmd move-attention
       screen-pos =visual-location)

  (p attend-and-instan-loc
     "Create a new chunk in the imaginal buffer with the location information of the attended item"
     =visual>
       screen-pos =loc
     ?imaginal>
       state free
   ==>
     +imaginal> =loc
     !eval! (pprint-chunks-fct (list =visual)))

  (p adjust
     "adjust the x,y coordinates in the imaginal buffer"
     =imaginal>
       screen-x =x
       screen-y =y
       color =color
   ==>
     !bind! =nx (- =x 5)
     !bind! =ny (+ =y 2)
     =imaginal>
       screen-x =nx
       screen-y =ny
       color nil)

  (p move
     "Move the mouse to the location in the imaginal buffer"
     =imaginal>
       color nil
     ?manual>
       state free
   ==>
     +manual>
       isa move-cursor
       loc =imaginal
     +goal>)
  
  (p click
     "Once the mouse is there click it and try to find another feature in the window"
     =goal>
     ?manual>
       state free
   ==>
     +manual>
       isa click-mouse
     +visual-location>
       > screen-y current
       :nearest current-y
     -goal>
     -imaginal>))

#|
CG-USER(42): (run-test)
Loc        Att   Kind           Value             Color           ID
---------  ---   -------------  ----------------  --------------  -------------
( 74 224)  NEW   IMAGE          "brain"           BLACK           VISUAL-LOCATION1
(154  81)  NEW   IMAGE          "logo"            BLACK           VISUAL-LOCATION0
     0.000   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION0-0 REQUESTED NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.050   PROCEDURAL             PRODUCTION-FIRED ATTEND
     0.050   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.050   PROCEDURAL             CLEAR-BUFFER VISUAL
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.135   VISION                 Encoding-complete VISUAL-LOCATION0-0-0 NIL
     0.135   VISION                 SET-BUFFER-CHUNK VISUAL IMAGE0
     0.135   PROCEDURAL             CONFLICT-RESOLUTION
     0.185   PROCEDURAL             PRODUCTION-FIRED ATTEND-AND-INSTAN-LOC
IMAGE0-0
   SCREEN-POS  VISUAL-LOCATION0-0-0
   VALUE  "logo"
   COLOR  BLACK
   HEIGHT  142
   WIDTH  288
   IMAGE  T

     0.185   PROCEDURAL             CLEAR-BUFFER VISUAL
     0.185   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     0.185   PROCEDURAL             CONFLICT-RESOLUTION
     0.385   IMAGINAL               SET-BUFFER-CHUNK IMAGINAL CHUNK0
     0.385   PROCEDURAL             CONFLICT-RESOLUTION
     0.435   PROCEDURAL             PRODUCTION-FIRED ADJUST
     0.435   PROCEDURAL             CONFLICT-RESOLUTION
     0.485   PROCEDURAL             PRODUCTION-FIRED MOVE
     0.485   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     0.485   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.485   PROCEDURAL             CLEAR-BUFFER GOAL
     0.485   MOTOR                  MOVE-CURSOR LOC CHUNK0-0
     0.485   GOAL                   SET-BUFFER-CHUNK GOAL CHUNK1
     0.485   PROCEDURAL             CONFLICT-RESOLUTION
     0.685   PROCEDURAL             CONFLICT-RESOLUTION
     0.735   PROCEDURAL             CONFLICT-RESOLUTION
     0.835   PROCEDURAL             CONFLICT-RESOLUTION
     0.885   PROCEDURAL             CONFLICT-RESOLUTION
     0.935   PROCEDURAL             PRODUCTION-FIRED CLICK
     0.935   PROCEDURAL             CLEAR-BUFFER GOAL
     0.935   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     0.935   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.935   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.935   MOTOR                  CLICK-MOUSE
     0.935   VISION                 Find-location
     0.935   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION1-0
     0.935   PROCEDURAL             CONFLICT-RESOLUTION
     0.985   PROCEDURAL             PRODUCTION-FIRED ATTEND
     0.985   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.985   PROCEDURAL             CLEAR-BUFFER VISUAL
     0.985   PROCEDURAL             CONFLICT-RESOLUTION
     1.070   VISION                 Encoding-complete VISUAL-LOCATION1-0-0 NIL
     1.070   VISION                 SET-BUFFER-CHUNK VISUAL IMAGE1
     1.070   PROCEDURAL             CONFLICT-RESOLUTION
     1.120   PROCEDURAL             PRODUCTION-FIRED ATTEND-AND-INSTAN-LOC
IMAGE1-0
   SCREEN-POS  VISUAL-LOCATION1-0-0
   VALUE  "brain"
   COLOR  BLACK
   HEIGHT  128
   WIDTH  128
   IMAGE  T

     1.120   PROCEDURAL             CLEAR-BUFFER VISUAL
     1.120   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     1.120   PROCEDURAL             CONFLICT-RESOLUTION
     1.135   PROCEDURAL             CONFLICT-RESOLUTION
     1.145   MOTOR                  OUTPUT-KEY #(28 2)

<< Window "image test (IMAGE-TESTING-MODEL)" image with file "smalllogo.gif" and text "logo" clicked at relative position 139 73 >>

     1.145   PROCEDURAL             CONFLICT-RESOLUTION
     1.235   PROCEDURAL             CONFLICT-RESOLUTION
     1.320   IMAGINAL               SET-BUFFER-CHUNK IMAGINAL CHUNK2
     1.320   PROCEDURAL             CONFLICT-RESOLUTION
     1.370   PROCEDURAL             PRODUCTION-FIRED ADJUST
     1.370   PROCEDURAL             CONFLICT-RESOLUTION
     1.420   PROCEDURAL             PRODUCTION-FIRED MOVE
     1.420   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     1.420   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.420   PROCEDURAL             CLEAR-BUFFER GOAL
     1.420   MOTOR                  MOVE-CURSOR LOC CHUNK2-0
     1.420   GOAL                   SET-BUFFER-CHUNK GOAL CHUNK3
     1.420   PROCEDURAL             CONFLICT-RESOLUTION
     1.620   PROCEDURAL             CONFLICT-RESOLUTION
     1.670   PROCEDURAL             CONFLICT-RESOLUTION
     1.770   PROCEDURAL             CONFLICT-RESOLUTION
     1.820   PROCEDURAL             CONFLICT-RESOLUTION
     1.870   PROCEDURAL             PRODUCTION-FIRED CLICK
     1.870   PROCEDURAL             CLEAR-BUFFER GOAL
     1.870   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     1.870   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.870   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     1.870   MOTOR                  CLICK-MOUSE
     1.870   VISION                 Find-location
     1.870   VISION                 FIND-LOC-FAILURE
     1.870   PROCEDURAL             CONFLICT-RESOLUTION
     2.020   PROCEDURAL             CONFLICT-RESOLUTION
     2.070   PROCEDURAL             CONFLICT-RESOLUTION
     2.080   MOTOR                  OUTPUT-KEY #(28 2)
IMAGE-TESTING-MODEL clicked image at 59 66
     2.080   PROCEDURAL             CONFLICT-RESOLUTION
     2.170   PROCEDURAL             CONFLICT-RESOLUTION
     2.170   ------                 Stopped because no events left to process
2.17
169
NIL
|#