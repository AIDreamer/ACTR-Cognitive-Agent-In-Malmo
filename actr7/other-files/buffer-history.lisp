;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2008-2011 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : buffer-history.lisp
;;; Version     : 1.1
;;; 
;;; Description : Code to support the buffer history tool in the environment.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [x] Consider converting the internal time over to milliseconds
;;;             :     by using buffer-record-ms-time instead of -time-stamp.
;;;             :     The down side of that is the display for the environment
;;;             :     should probably still be in seconds.
;;; 
;;; ----- History -----
;;; 2008.08.20 Dan
;;;             : * Initial creation.
;;; 2011.04.25 Dan
;;;             : * Added the "to do" to consider.
;;; 2011.06.20 Dan [1.1]
;;;             : * Changed the tool to record all the details everytime, not 
;;;             :   just when there is a "change" to the buffer because some
;;;             :   changes are below what the buffer trace detects i.e. custom
;;;             :   queries for modules.
;;; 2011.06.24 Dan
;;;             : * Changed the output so that the query info is first so that
;;;             :   it's easier to watch that for a change in a buffer since it
;;;             :   will be formatted consistently.
;;; 2013.11.15 Dan
;;;             : * Chunk contents are now displayed at time stamps when there
;;;             :   isn't an "action" on that buffer which was a bug because of
;;;             :   how the events were recorded and compared.
;;;             : * Removed the special :cleared marker for the chunk details
;;;             :   since it wasn't really being used -- now it's either the
;;;             :   string of the chunk details or nil if the buffer is empty or
;;;             :   being cleared at that time.
;;; 2015.06.09 Dan
;;;             : * Record time in ms internally, but still show seconds to the
;;;             :   user in the list.
;;; 2016.04.22 Dan
;;;             : * Start of the upgrade to allow saving history info so that it
;;;             :   can be reloaded for the environment tools.
;;; 2016.04.27 Dan
;;;             : * Modify the environment interface functions so that they use
;;;             :   the indicated underlying data.
;;; 2016.05.04 Dan
;;;             : * Just some minor code clean-up in the param function.
;;; 2016.05.06 Dan
;;;             : * Fixed an issue with buffers that were cleared and then set
;;;             :   at the same time -- before the end state showed as empty.
;;;             : * Also marked the to do as complete since that happened with
;;;             :   the 2015.06.09 update.
;;; 2016.05.16 Dan
;;;             : * Previous fix actually broke the chunk recording so this now
;;;             :   gets it right.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Set the :save-buffer-history parameter to t in the model or select the "Buffer
;;; History" option in the data to record using the Environment.
;;; 
;;; Once the model has run click the "Get history" button in the buffer history 
;;; window.  Only those buffers specified with the :traced-buffers parameter
;;; of the buffer-trace module (or those selected in the Environment recorder) 
;;; will have thier history recorded.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; :save-buffer-history parameter
;;;  Enables the recording of buffer history for display (default is nil).
;;;  This will also set the :save-buffer-trace parameter to t if it is not
;;;  already set.
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

(defstruct buffer-history-module
  (table (make-hash-table))
  enabled)
  

(defstruct buffer-history
  time
  chunk  ; a string or nil
  status ; a string
  )


(defun equal-history-samples (h1 h2)
  (and h1 h2
       (or (eq (buffer-history-chunk h1) (buffer-history-chunk h2))
           (and (stringp (buffer-history-chunk h1)) (stringp (buffer-history-chunk h2)) (string-equal (buffer-history-chunk h1) (buffer-history-chunk h2))))
       (string-equal (buffer-history-status h1) (buffer-history-status h2))))


(defun buffer-history-recorder (summaries)
  (let ((history (get-module buffer-history))
        (time (buffer-record-ms-time summaries)))
      
    (when (and history (buffer-history-module-enabled history))
      (dolist (summary (buffer-record-buffers summaries))
        (let* ((name (buffer-summary-name summary))
               (record (make-buffer-history :time time
                                            :chunk (when (buffer-read name)
                                                     (capture-model-output (buffer-chunk-fct (list name))))
                                            :status (capture-model-output (buffer-status-fct (list name))))))
          (unless (equal-history-samples record (car (gethash name (buffer-history-module-table history))))
            (push record (gethash name (buffer-history-module-table history)))))))))

               
(defun buffer-history-buffer-list (key)
  (let ((data (get-history-information :save-buffer-history key)))
    (sort (mapcar 'first data) #'string< :key 'symbol-name)))

(defun buffer-history-text (time-string buffer key)
  (if (and (> (length time-string) 1) buffer)
      (let* ((data (get-history-information :save-buffer-history key))
             (time (read-from-string (remove #\. time-string)))
             (record (find-if (lambda (x) 
                                (<= (buffer-history-time x) time))
                              (second (assoc buffer data))))
             (chunk (when record (buffer-history-chunk record)))
             (status (when record (buffer-history-status record))))
            
        (concatenate 'string 
          (cond ((and status (stringp status))
                 status)
                (t "No buffer status information available"))
          (string #\newline)
          (cond ((stringp chunk) 
                 chunk)
                (t (format nil "buffer empty~%")))))
    ""))

(defun buffer-history-time-list (key)
  (let ((data (get-history-information :save-buffer-history key)))
    (let ((times nil))
      (dolist (buffer data)
        (dolist (history (second buffer))
          (pushnew (buffer-history-time history) times :test '=)))
      (mapcar (lambda (x) (format nil "~/print-time-in-seconds/" x)) (sort times #'<)))))


(defun reset-buffer-history-module (module)
  (clrhash (buffer-history-module-table module)))

  
(defun params-buffer-history-module (instance param)
  (if (consp param)
      (case (car param)
        (:save-buffer-history 
          (no-output 
           (if (cdr param)
               (progn
                 (sgp :save-buffer-trace t)
                 (unless (find 'buffer-history-recorder (car (sgp :buffer-trace-hook)))
                   (sgp :buffer-trace-hook buffer-history-recorder)))
             (progn
               (when (find 'buffer-history-recorder (car (sgp :buffer-trace-hook)))
                 (let ((old-hooks (car (sgp :buffer-trace-hook))))
                   (sgp :buffer-trace-hook nil)
                   (dolist (x old-hooks)
                     (unless (eq x 'buffer-history-recorder)
                       (sgp-fct (list :buffer-trace-hook x)))))))))
           
           (setf (buffer-history-module-enabled instance) (cdr param))))
    (case param
      (:save-buffer-history (buffer-history-module-enabled instance)))))

(define-module-fct 'buffer-history nil 
  (list (define-parameter :save-buffer-history :valid-test 'tornil :default-value nil  
          :warning "T or nil" 
          :documentation "Whether or not to record the history of buffer changes."))
  :creation (lambda (x) (declare (ignore x)) (make-buffer-history-module))
  :reset 'reset-buffer-history-module
  :params 'params-buffer-history-module
  :version "1.0"
  :documentation "Module to record buffer change history for display in the environment.")
  

(defun get-buffer-history-data ()
  (let ((m (get-module buffer-history)))
    (when m
      (let ((r nil))
        (maphash (lambda (buffer data)
                   (push (list buffer data) r))
                 (buffer-history-module-table m))
        r))))

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
