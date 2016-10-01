;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2016 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : perceptual-history.lisp
;;; Version     : 1.0a2
;;; 
;;; Description : Record visicon and audicon data when requested.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2016.04.06 Dan [1.0a1] 
;;;             : * Initial creation.
;;; 2016.04.11 Dan [1.0a2]
;;;             : * Doesn't work right in a multi-model situation since each
;;;             :   model wants to remove the event hook as soon as it no longer
;;;             :   needs it.  Instead keep a counter with the hook so that all
;;;             :   models in a meta-process know if anyone else is using it.
;;; 2016.04.12 Dan
;;;             : * Fix the text box response functions for the environment so
;;;             :   that they don't display nil when there's nothing to show.
;;; 2016.04.15 Dan
;;;             : * Fixed a bug in setting :save-audicon-history -- turning it
;;;             :   off turned off the visicon history.
;;; 2016.04.22 Dan
;;;             : * Added the "get" functions for the data to support the saving
;;;             :   of data from the environment. 
;;; 2016.04.27 Dan
;;;             : * Changed the environment interface functions to also accept
;;;             :   a key for the data and then call get-history-information 
;;;             :   instead of using the module info directly.
;;; 2016.05.18 Dan
;;;             : * Fixed a bug with tracking the data for wheteher or not to
;;;             :   keep the event hook in place.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; To support visicon history and audicon history tools in the Environment there
;;; needs to be something which saves the information.
;;; Since it's being saved might as well also provide commands for a text output
;;; as well.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; Two new parameters :save-visicon-history and :save-audicon-history which
;;; if set to t will record the changes to the corresponding data stream along
;;; with all the times they are updated.
;;;
;;; show-visicon-history 
;;; 
;;; Takes no parameters.  If there is any saved history for the visicon then for
;;; each time that there was a visicon change recorded this command will print
;;; the time of the change followed by the output from print-visicon at that
;;; time.  It's possible that there could be more than one change at a given
;;; time, in which case the different print-visicon outputs will be shown in 
;;; the order that they occurred (the older ones before the new ones).  It will
;;; always return nil.
;;;
;;; show-audicon-history 
;;; 
;;; Takes no parameters.  If there is any saved history for the audicon then for
;;; each time that there was an audicon change recorded this command will print
;;; the time of the change followed by the output from print-audicon at that
;;; time.  It's possible that there could be more than one change at a given
;;; time, in which case the different print-audicon outputs will be shown in 
;;; the order that they occurred (the older ones before the new ones).  It will
;;; always return nil.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; To record the information for vision creating an after method for process-
;;; display to record when there's an explict change and similarly for audio 
;;; add an after method to new-sound-event to catch when new sounds are created.
;;; In addition, it needs to use a post-event-hook to detect when there is an
;;; audio or vision action and record any new info.  Ideally, scheduling an
;;; after module event would be better, but in the case where there are multiple
;;; models that would need to schedule such an event for each possible model
;;; which gets pretty ugly.  To keep things simple (though less efficient) for
;;; now just using the post-event-hook since that will have the model info in
;;; the event that can be used.
;;;
;;; Those new after methods could interfere with similar methods that somebody
;;; may have written to do something else, but since that wasn't part of the API
;;; it was a risky thing to do.  Of course, it could go the other way too --
;;; if someone loads such a change after this it's going to break the history 
;;; tool.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defclass perceptual-history ()
  ((visicon-history :accessor visicon-history :initform nil)
   (audicon-history :accessor audicon-history :initform nil)
   (last-visicon :accessor last-visicon :initform "")
   (last-audicon :accessor last-audicon :initform "")
   (save-visicon :accessor save-visicon :initform nil)
   (save-audicon :accessor save-audicon :initform nil)
   (perceptual-event-hook :accessor perceptual-event-hook :initform (make-hash-table :test 'eq) :allocation :class)))


(defmethod process-display :after ((devin device-interface) (vis-mod vision-module) &optional (clear nil))
  (declare (ignore clear))
  (let ((instance (get-module :perceptual-history)))
    (if instance
        (let ((current (capture-model-output (print-visicon))))
          (unless (string-equal (last-visicon instance) current)
            (setf (last-visicon instance) current)
            (let ((last-time (caar (last (visicon-history instance)))))
              (if (and last-time (= last-time (mp-time-ms)))
                  (push-last current (car (last (visicon-history instance))))
                (push-last (list (mp-time-ms) current) (visicon-history instance))))))
      (print-warning "No perceptual history module available cannot record visicon information"))))


(defmethod new-sound-event :after ((evt sound-event))
  (let ((instance (get-module :perceptual-history)))
    (if instance
        (let ((current (capture-model-output (print-audicon))))
          (unless (string-equal (last-audicon instance) current)
            (setf (last-audicon instance) current)
            (let ((last-time (caar (last (audicon-history instance)))))
              (if (and last-time (= last-time (mp-time-ms)))
                  (push-last current (car (last (audicon-history instance))))
                (push-last (list (mp-time-ms) current) (audicon-history instance))))))
      (print-warning "No perceptual history module available cannot record audicon information"))))


(defun perceptual-history-recorder (event)
  (when (current-model)
    (let ((instance (get-module :perceptual-history)))
      (when (and (save-visicon instance) (eq (evt-module event) :vision))
        (let ((current (capture-model-output (print-visicon))))
          (unless (string-equal (last-visicon instance) current)
            (setf (last-visicon instance) current)
            (let ((last-time (caar (last (visicon-history instance)))))
              (if (and last-time (= last-time (mp-time-ms)))
                  (push-last current (car (last (visicon-history instance))))
                (push-last (list (mp-time-ms) current) (visicon-history instance)))))))
      
      (when (and (save-audicon instance) (eq (evt-module event) :audio))
        (let ((current (capture-model-output (print-audicon))))
          (unless (string-equal (last-audicon instance) current)
            (setf (last-audicon instance) current)
            (let ((last-time (caar (last (audicon-history instance)))))
              (if (and last-time (= last-time (mp-time-ms)))
                  (push-last current (car (last (audicon-history instance))))
                (push-last (list (mp-time-ms) current) (audicon-history instance))))))))))


(defun reset-perceptual-history (instance)
  (setf (visicon-history instance) nil)
  (setf (audicon-history instance) nil)
  (setf (last-visicon instance) "")
  (setf (last-audicon instance) ""))  


(defun delete-perceptual-history (instance)
  (when (save-visicon instance)
    (let* ((state (gethash (current-meta-process) (perceptual-event-hook instance)))
           (vis-count (1- (third state)))
           (aud-count (second state)))
      (if (and (zerop vis-count) (zerop aud-count))
          (progn
            (delete-event-hook (first state))
            (remhash (current-meta-process) (perceptual-event-hook instance)))
        (setf (gethash (current-meta-process) (perceptual-event-hook instance)) (list (first state) aud-count vis-count)))))
   (when (save-audicon instance)
    (let* ((state (gethash (current-meta-process) (perceptual-event-hook instance)))
           (aud-count (1- (second state)))
           (vis-count (third state)))
      (if (and (zerop vis-count) (zerop aud-count))
          (progn
            (delete-event-hook (first state))
            (remhash (current-meta-process) (perceptual-event-hook instance)))
        (setf (gethash (current-meta-process) (perceptual-event-hook instance)) (list (first state) aud-count vis-count))))))

    
(defun perceptual-history-params (instance param)
  (if (consp param)
      (case (car param)
        (:save-visicon-history 
         (let ((current (cdr param))
               (previous (save-visicon instance)))
           (cond ((eq current previous) ;; hasn't changed
                  
                  ;; don't do anything but return current value
                  current)
                 ((null previous) ;; it was off now it's on
                  
                  ;; if there's not an event hook add one now and
                  ;; if there is increment the count of who is using it for vision
                  
                  (let ((state (gethash (current-meta-process) (perceptual-event-hook instance))))
                    (if (null state)
                        (setf (gethash (current-meta-process) (perceptual-event-hook instance))
                          (list 
                           (add-post-event-hook 'perceptual-history-recorder)
                           0 1))
                      (incf (third (gethash (current-meta-process) (perceptual-event-hook instance))))))
                      
                    
                  (setf (save-visicon instance) t))
                 
                 (t ;; it was on now it's off
                  (let* ((state (gethash (current-meta-process) (perceptual-event-hook instance)))
                         (vis-count (1- (third state)))
                         (aud-count (second state)))
                    (if (and (zerop vis-count) (zerop aud-count))
                        (progn
                          (delete-event-hook (first state))
                          (remhash (current-meta-process) (perceptual-event-hook instance)))
                      (setf (gethash (current-meta-process) (perceptual-event-hook instance)) (list (first state) aud-count vis-count))))
                  
                  (setf (save-visicon instance) nil)))))
        (:save-audicon-history 
         (let ((current (cdr param))
               (previous (save-audicon instance)))
           (cond ((eq current previous) ;; hasn't changed
                  
                  ;; don't do anything but return current value
                  current)
                 ((null previous) ;; it was off now it's on
                  
                  ;; if there's not an event hook add one now and
                  ;; if there is increment the count of who is using it for audio
                  
                  (let ((state (gethash (current-meta-process) (perceptual-event-hook instance))))
                    (if (null state)
                        (setf (gethash (current-meta-process) (perceptual-event-hook instance))
                          (list 
                           (add-post-event-hook 'perceptual-history-recorder)
                           1 0))
                      (incf (second (gethash (current-meta-process) (perceptual-event-hook instance))))))
                      
                    
                  (setf (save-audicon instance) t))
                 
                 (t ;; it was on now it's off
                  (let* ((state (gethash (current-meta-process) (perceptual-event-hook instance)))
                         (vis-count (third state))
                         (aud-count (1- (second state))))
                    (if (and (zerop vis-count) (zerop aud-count))
                        (progn
                          (delete-event-hook (first state))
                          (remhash (current-meta-process) (perceptual-event-hook instance)))
                      (setf (gethash (current-meta-process) (perceptual-event-hook instance)) (list (first state) aud-count vis-count))))
                  
                  (setf (save-audicon instance) nil))))))
    (case param
      (:save-visicon-history (save-visicon instance))
      (:save-audicon-history (save-audicon instance)))))


(define-module-fct :perceptual-history nil 
  (list (define-parameter :save-visicon-history :valid-test 'tornil :default-value nil
          :warning "T or nil"
          :documentation "Whether or not to record a history of all visicon changes.")
        (define-parameter :save-audicon-history :valid-test 'tornil :default-value nil
          :warning "T or nil"
          :documentation "Whether or not to record a history of all audicon changes."))
  :creation (lambda (x) (declare (ignore x)) (make-instance 'perceptual-history))
  :reset 'reset-perceptual-history
  :delete 'delete-perceptual-history
  :params 'perceptual-history-params
  :version "1.0a2"
  :documentation "Module to record visicon and audicon changes.")
        

(defun get-audicon-history ()
  (let ((m (get-module :perceptual-history)))
    (when m
      (audicon-history m))))

(defun get-visicon-history ()
  (let ((m (get-module :perceptual-history)))
    (when m
      (visicon-history m))))


(defun show-visicon-history (&optional start end)
  (let ((instance (get-module :perceptual-history)))
    (if instance
        (cond ((not (or (numberp start) (null start)))
               (print-warning "Start value for show-visicon-history must be a number or nil, but given ~s" start))
              ((not (or (numberp end) (null end)))
               (print-warning "End value for show-visicon-history must be a number or nil, but given ~s" end))
              ((and (numberp start) (numberp end) (> start end))
               (print-warning "End value for show-visicon-history must be greater-than or equal to the start value, but given start=~f and end=~f" start end))
              (t
               (let* ((history (visicon-history instance))
                      (si (if (numberp start) 
                              (aif (position-if (lambda (x) (>= (first x) (seconds->ms start))) history)
                                   it
                                   (length history))
                            0))
                      (ei (if (numberp end)
                              (position-if (lambda (x) (> (first x) (seconds->ms end))) history)
                            (length history))))
                 (dolist (x (subseq history si ei) (copy-seq (subseq history si ei)))
                   (command-output "Time: ~f~%~{~a~}" (ms->seconds (first x)) (rest x))))))
      (print-warning "No perceptual history module available cannot show visicon history"))))

(defun show-audicon-history (&optional start end)
  (let ((instance (get-module :perceptual-history)))
    (if instance
        (cond ((not (or (numberp start) (null start)))
               (print-warning "Start value for show-audicon-history must be a number or nil, but given ~s" start))
              ((not (or (numberp end) (null end)))
               (print-warning "End value for show-audicon-history must be a number or nil, but given ~s" end))
              ((and (numberp start) (numberp end) (> start end))
               (print-warning "End value for show-audicon-history must be greater-than or equal to the start value, but given start=~f and end=~f" start end))
              (t
               (let* ((history (audicon-history instance))
                      (si (if (numberp start) 
                              (aif (position-if (lambda (x) (>= (first x) (seconds->ms start))) history)
                                   it
                                   (length history))
                            0))
                      (ei (if (numberp end)
                              (position-if (lambda (x) (> (first x) (seconds->ms end))) history)
                            (length history))))
                 (dolist (x (subseq history si ei) (copy-seq (subseq history si ei)))
                   (command-output "Time: ~f~%~{~a~}" (ms->seconds (first x)) (rest x))))))
      (print-warning "No perceptual history module available cannot show audicon history"))))



(defun audicon-history-time-list (key)
  (mapcar (lambda (x) (format nil "~/print-time-in-seconds/" (first x))) (get-history-information :save-audicon-history key)))


(defun audicon-history-text (time-string key)
  (if (> (length time-string) 1)
      (let* ((time (read-from-string (remove #\. time-string)))
                   (data (assoc time (get-history-information :save-audicon-history key) :test #'=)))
              (if data 
                  (format nil "~{~a~}" (rest data))
                " "))
    " "))


(defun visicon-history-time-list (key)
  (mapcar (lambda (x) (format nil "~/print-time-in-seconds/" (first x))) (get-history-information :save-visicon-history key)))

(defun visicon-history-text (time-string key)
  (if (> (length time-string) 1)
      (let* ((time (read-from-string (remove #\. time-string)))
             (data (assoc time (get-history-information :save-visicon-history key) :test #'=)))
              (if data 
                  (format nil "~{~a~}" (rest data))
                " "))
          " "))

#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
