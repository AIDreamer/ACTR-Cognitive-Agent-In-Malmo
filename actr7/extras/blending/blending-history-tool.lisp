;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2011 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : blending-history-tool.lisp
;;; Version     : 1.1
;;; 
;;; Description : Code to support a blending history tool in the environment.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2011.06.23 Dan
;;;             : * Initial creation.
;;; 2011.06.24 Dan
;;;             : * Removed the dummy print-blending-activation-trace function.
;;;             : * Not printing the chunk parameters in the chunk view since
;;;             :   they may not correspond to the values actually used (in
;;;             :   particular activation) so that could be confusing.
;;;             : * Saving the result chunk printout as a string since it 
;;;             :   could be changed in the buffer before going to dm.
;;; 2012.02.09 Dan
;;;             : * Explicitly close streams made with make-string-output-stream 
;;;             :   to be safe.
;;; 2016.05.03 Dan [1.1]
;;;             : * Update the code to work with the new history tool interface
;;;             :   that can use saved data.  Requires getting a key for every
;;;             :   interface function, saving the text of items instead of just
;;;             :   printing them when asked, and adding a function to get the
;;;             :   history.
;;;             : * Also turn on :sact and :sblt with :save-blending-history.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Put this file into the other-files directory and the corresponding .tcl file
;;; into the environment/GUI/dialogs directory to use the new tool.
;;;
;;; Enable the Blending trace in the recorder tool or set the :save-blending-history
;;; parameter to t in the model to enable the recording.
;;; 
;;; Once the model has run click the "Get history" button in the blending history 
;;; window.  
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; :save-blending-history parameter
;;;  Enables the recording of blending history for display (default is nil).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defstruct blend-history-module
  history
  enabled)
  

(defstruct blend-history
  time
  request
  trace
  chunks
  chunk-texts
  activation-traces
  result)

(defun blend-request-recorder (request)
  (let ((history (get-module blending-history))
        (block (make-blend-history :time (mp-time-ms) :request (capture-model-output (pprint-chunk-spec request)))))
    (push block (blend-history-module-history history))
    nil))

(defun blend-set-recorder (set)
  (let* ((history (get-module blending-history))
         (record (car (blend-history-module-history history))))
    (setf (blend-history-chunks record) set)
    (dolist (x set)
      (push (cons x (capture-model-output (pprint-chunks-fct (list x))))
            (blend-history-chunk-texts record))
      (push (cons x (capture-model-output (print-chunk-activation-trace-fct x (mp-time-ms) t)))
            (blend-history-activation-traces record)))
    nil))

(defun blend-result-recorder (result)
  (let* ((history (get-module blending-history))
         (record (car (blend-history-module-history history)))
         (cmdt (car (no-output (sgp :cmdt))))
         (s (make-string-output-stream)))
    (setf (blend-history-trace record)
      (capture-model-output (print-blending-trace (blend-history-time record) t)))
    (if result
        (progn
          (sgp-fct (list :cmdt s))
          (pprint-chunks-fct (list result))
          (setf (blend-history-result record) (get-output-stream-string s))
          (sgp-fct (list :cmdt cmdt)))
      (setf (blend-history-result record) nil))
    (close s))
  nil)


(defun blend-history-chunk-display (time-string chunk key)
  (let* ((time (read-from-string (remove #\. time-string)))
         (history (get-history-information :save-blending-history key))
         (record (find time history :key 'blend-history-time)))
    (aif (assoc chunk (blend-history-chunk-texts record))
         (format t "~a" (cdr it))
         "")))

(defun blend-history-chunk-trace-display (time-string chunk key)
  (let* ((time (read-from-string (remove #\. time-string)))
         (history (get-history-information :save-blending-history key))
         (record (find time history :key 'blend-history-time)))
         
    (aif (assoc chunk (blend-history-activation-traces record))
         (format t "~a" (cdr it))
         "")))

               
(defun blend-history-chunk-list (time-string key)
  (let* ((time (read-from-string (remove #\. time-string)))
         (history (get-history-information :save-blending-history key))
         (record (find time history :key 'blend-history-time)))
    (blend-history-chunks record)))

(defun blend-history-request-text (time-string key)
  (let* ((time (read-from-string (remove #\. time-string)))
         (history (get-history-information :save-blending-history key))
         (record (find time history :key 'blend-history-time)))
    (format t "~a" (blend-history-request record))))


(defun blend-history-get-time-list (key)
  (let ((history (get-history-information :save-blending-history key)))
    (nreverse (mapcar (lambda (x) 
                        (format nil "~/print-time-in-seconds/" (blend-history-time x))) 
                history))))


(defun blend-history-result-display (time-string key)
  (let* ((time (read-from-string (remove #\. time-string)))
         (history (get-history-information :save-blending-history key))
         (record (find time history :key 'blend-history-time)))
         
    (aif (blend-history-result record)
         (format t "~a" it)
      (format t "Blending failure"))))

(defun blend-history-trace-display (time-string key)
  (let* ((time (read-from-string (remove #\. time-string)))
         (history (get-history-information :save-blending-history key))
         (record (find time history :key 'blend-history-time)))
  
    (format t "~a" (blend-history-trace record))))


(defun reset-blend-history-module (module)
  (setf (blend-history-module-history module) nil))
  
(defun params-blend-history-module (instance param)
  (if (consp param)
      (case (car param)
        (:save-blending-history 
          (no-output
           (progn
             (if (cdr param)  
                 (progn
                   (unless (find 'blend-request-recorder (car (sgp :blending-request-hook)))
                     (sgp :blending-request-hook blend-request-recorder))
                   (unless (find 'blend-set-recorder (car (sgp :blending-set-hook)))
                     (sgp :blending-set-hook blend-set-recorder))
                   (unless (find 'blend-result-recorder (car (sgp :blending-result-hook)))
                     (sgp :blending-result-hook blend-result-recorder))
                   (unless (car (sgp :sact))
                     (sgp :sact t))
                   (unless (car (sgp :sblt))
                     (sgp :sblt t)))
               
               (progn
                 (when (find 'blend-request-recorder (car (sgp :blending-request-hook)))
                   (let ((old-hooks (car (sgp :blending-request-hook))))
                     (sgp :blending-request-hook nil)
                     (dolist (x old-hooks)
                       (unless (eq x 'blend-request-recorder)
                         (sgp-fct (list :blending-request-hook x))))))
                 (when (find 'blend-set-recorder (car (sgp :blending-set-hook)))
                   (let ((old-hooks (car (sgp :blending-set-hook))))
                     (sgp :blending-set-hook nil)
                     (dolist (x old-hooks)
                       (unless (eq x 'blend-set-recorder)
                         (sgp-fct (list :blending-set-hook x))))))
                 (when (find 'blend-result-recorder (car (sgp :blending-result-hook)))
                   (let ((old-hooks (car (sgp :blending-result-hook))))
                     (sgp :blending-result-hook nil)
                     (dolist (x old-hooks)
                       (unless (eq x 'blend-result-recorder)
                         (sgp-fct (list :blending-result-hook x))))))))
          
             (setf (blend-history-module-enabled instance) (cdr param))))))
    (case param
      (:save-blending-history (blend-history-module-enabled instance)))))

(define-module-fct 'blending-history nil 
  (list (define-parameter :save-blending-history :valid-test 'tornil :default-value nil  
          :warning "T or nil" 
          :documentation "Whether or not to record the history of all blending events."))
  :creation (lambda (x) (declare (ignore x)) (make-blend-history-module))
  :reset 'reset-blend-history-module
  :params 'params-blend-history-module
  :version "1.1"
  :documentation "Module to record blending history for display in the environment.")
  

(defun get-blending-history ()
  (let ((m (get-module blending-history)))
    (when m
      (blend-history-module-history m))))
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
