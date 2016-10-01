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
;;; Filename    : history-recorder.lisp
;;; Version     : 1.0a1
;;; 
;;; Description : Keep track of traces and buffers picked from the history recorder
;;;               environment tool, support the history playback tool, and provide 
;;;               the functions for saving and reading the underlying trace info.
;;;
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2016.04.15 Dan [1.0a1] 
;;;             : * Initial creation.
;;; 2016.04.21 Dan
;;;             : * Updates to make this actually usable...
;;; 2016.04.22 Dan
;;;             : * Adding the support for saving/loading data for the history
;;;             :   tools.  The approach is that a tool needs to specify a function
;;;             :   to "get" the data and should always call the get-history-information
;;;             :   function to access the data for display because that will
;;;             :   return the current or saved data as needed.
;;;             : * Adding code to support the text trace here since it doesn't
;;;             :   have its own file.
;;; 2016.05.03 Dan [1.0a2]
;;;             : * Adding support for removing the saved traces from the global
;;;             :   hash table to conserve memory.  The table now holds a count
;;;             :   of windows using it and it will decrement that when the window
;;;             :   closes (needs the signals from the environment) and can 
;;;             :   remhash it at 0.
;;; 2016.05.05 Dan
;;;             : * Allow get-history-information to work without a current model
;;;             :   when it's tabled data being accessed.
;;; 2016.05.12 Dan
;;;             : * Updated the save-history-information to better deal with
;;;             :   errors and report them back to the Environment.
;;; 2016.05.13 Dan
;;;             : * Similar update to load-history-information.
;;;             : * The text history output function can't use command-output
;;;             :   since there may not be a model when looking at saved data.
;;; 2016.05.17 Dan
;;;             : * Fixed a bug with load-history-information that didn't catch
;;;             :   bad data at the start of the loaded file.
;;; 2016.05.18 Dan
;;;             : * Fixed some issues with unused variables.
;;; 2016.05.19 Dan
;;;             : * Fixed an issue with loading a model while the previous model
;;;             :   had a select data window open already -- the new model ended
;;;             :   up with invalid settings sometimes because of the asynchrony
;;;             :   in closing the old window and defining the new model.  Now
;;;             :   the module's handler code checks to make sure it's the right  
;;;             :   model before initializing the data from the window's handler.
;;;             : * Better fix for that now since reload also had an issue if it
;;;             :   was the same model...
;;; 2016.06.01 Dan
;;;             : * When saving the history information also save the current
;;;             :   ACT-R version and then check that when loading.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;;
;;; Support for the recording and playback of trace info as well as the tool
;;; for easy enabling of traces.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; Nothing.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Keeping it simple for now.  Because of that the save files are huge at this
;;; point, but until there's feedback/demand for improvement don't worry about it.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defvar *history-recorder-data-cache* (make-hash-table :test 'equalp))

(defstruct history-recorder traces buffers enabled live (data-function (make-hash-table :test 'eq)) (save-function (make-hash-table :test 'eq)))
  
(defun history-recorder-set-initial-params (x) 
  (declare (ignore x))
  (let ((module (get-module :history-recorder)))
    (when module
      (when (null (history-recorder-enabled module)) ;; just opened
        (progn ;; clear any existing settings and mark as enabled
          (clear-history-recorder module)
          (setf (history-recorder-enabled module) t))))))

(defun history-recorder-reset-params (x) 
  (declare (ignore x))
    (let ((module (get-module :history-recorder)))
      (when (and module (history-recorder-enabled module))
        (dolist (y (history-recorder-traces module))
          (no-output (sgp-fct (list y t))))
        (no-output (sgp-fct (list :traced-buffers (sort (copy-list (history-recorder-buffers module)) 'string< :key 'symbol-name)))))))




(defun disable-history-recorder (x)
  (declare (ignore x))
  (let ((module (get-module :history-recorder)))
    (when module
      (clear-history-recorder module)
      (setf (history-recorder-enabled module) nil))))
      
      
(defun add-history-trace-param (param)
  (let ((module (get-module :history-recorder)))
    (when module
      (pushnew param (history-recorder-traces module))
      (no-output (sgp-fct (list param t)))
      1)))

(defun remove-history-trace-param (param)
  (let ((module (get-module :history-recorder)))
    (when module
      (setf (history-recorder-traces module) (remove param (history-recorder-traces module)))
      (no-output (sgp-fct (list param nil)))
      1)))


(defun add-history-traced-buffer (buffer)
  (let ((module (get-module :history-recorder)))
    (when module
      (pushnew buffer (history-recorder-buffers module))
      (no-output (sgp-fct (list :traced-buffers (sort (copy-list (history-recorder-buffers module)) 'string< :key 'symbol-name))))
      1)))

(defun remove-history-traced-buffer (buffer)
  (let ((module (get-module :history-recorder)))
    (when module
      (setf (history-recorder-buffers module) (remove buffer (history-recorder-buffers module)))
      (no-output (sgp-fct (list :traced-buffers (sort (copy-list (history-recorder-buffers module)) 'string< :key 'symbol-name))))
      1)))


(defun history-recorder-initial-flags (params data save)
  (let ((module (get-module :history-recorder)))
    (when module
      (mapcar (lambda (x y z) 
                (setf (gethash x (history-recorder-data-function module)) y)
                (setf (gethash x (history-recorder-save-function module)) z)
                
                (if (car (no-output (sgp-fct (list x))))
                    (add-history-trace-param x) 
                  0))
        params
        data 
        save))))

(defun history-recorder-initial-buffers (x) 
  (declare (ignore x)) 
  (let ((module (get-module :history-recorder)))
    (when module
      (let ((buffers (no-output (car (sgp :traced-buffers)))))
        (when (eq buffers t)
          (setf buffers
            (aif (suppress-warnings (get-module procedural))
                 (cons 'production (mapcar 'car (procedural-buffer-indices it)))
                 (buffers))))
        (mapcar (lambda (y) 
                  (format nil "~a ~d" y 
                    (if (find y buffers)
                        (add-history-traced-buffer y)
                      0)))
          (sort (buffers) 'string< :key 'symbol-name))))))


(defun default-save-history-info (data stream)
  (write data :stream stream))

(defun get-history-information (parameter key)
  (cond ((symbolp key)
         (let ((module (get-module :history-recorder)))
           (when module
             (let ((fun (gethash parameter (history-recorder-data-function module))))
               (when fun
                 (funcall fun))))))
        (t
         (gethash parameter (cdr (gethash (first key) *history-recorder-data-cache*))))))


(defun select-live-data (model-name params-list)
  (let ((module (get-module :history-recorder)))
    (if module
        (if (eql (current-model) model-name)
            (cons 1 (remove-if-not (lambda (x) (find x (history-recorder-traces module))) params-list))
          (list 0))
      (list 0))))


(defun add-saved-file-user (key)
  (unless (symbolp key)
    (let ((entry (gethash (first key) *history-recorder-data-cache*)))
      (when (numberp (car entry))
        (incf (car entry)))))
  t)

(defun remove-saved-file-user (key)
  (unless (symbolp key)
    (let ((entry (gethash (first key) *history-recorder-data-cache*)))
      (when (numberp (car entry))
        (decf (car entry))
        (when (zerop (car entry))
          (remhash (first key) *history-recorder-data-cache*)))))
  t)



(defun load-history-information (file-name params-list)
  (let ((exists (probe-file file-name)))
    (if exists
        (with-open-file (f file-name :direction :input)
          (multiple-value-bind (check condition1)
              (ignore-errors (read f))
            (if (and (not (subtypep (type-of condition1) 'condition)) (listp check) 
                     (= (length check) 3) (eq (first check) :saved-act-r-history-data) (symbolp (second check)) (stringp (third check)))
                (if (car (ssp-fct (list :check-act-r-version (third check))))
                    (multiple-value-bind (result condition)
                        (ignore-errors
                         (let ((model (second check))
                               (keys nil)
                               (table (make-hash-table :test 'eq)))
                           (loop
                             (let ((val (ignore-errors (read f nil :reading-data-done))))
                               (when (eq val :reading-data-done)
                                 (return))
                               (if (and val (listp val) (= (length val) 2) (keywordp (first val)))
                                   (progn
                                     (push (first val) keys)
                                     (setf (gethash (first val) table) (second val)))
                                 (return-from load-history-information
                                   (list 0 (format nil "Invalid data found in history file ~s" file-name))))))
                           (setf (gethash file-name *history-recorder-data-cache*) (cons 1 table))
                           (append (list 1 model) (remove-if-not (lambda (x) (find x keys)) params-list))))
                  
                      (if (subtypep (type-of condition) 'condition)
                          (list 0 (format nil "Error encountered loading saved data file ~s~%" file-name))
                        result))
                  (list 0 (format nil "Saved file was from ACT-R version ~a which is not compatible with current version ~a" (third check) (car (ssp :act-r-version)))))
              (list 0 (format nil "Error encountered loading saved data file ~s~%~a" file-name "File does not appear to contain saved history information")))))
      (list 0 "File could not be found by ACT-R"))))


(defun save-history-information (file)
  (let ((module (get-module :history-recorder)))
    (if module
        (multiple-value-bind (result condition)
            (ignore-errors 
             (with-open-file (f file :direction :output :if-does-not-exist :create :if-exists :supersede)
               (format f "(:saved-act-r-history-data ~s ~s)~%" (current-model) (car (ssp :act-r-version)))
               (dolist (x (history-recorder-traces module))
                 (format f "(~s " x)
                 (funcall (gethash x (history-recorder-save-function module)) 
                          (funcall (gethash x (history-recorder-data-function module)))
                          f)
                 (format f ")~%"))))
          (declare (ignore result))
          (if (subtypep (type-of condition) 'condition)
              (list 0 (format nil "Error encountered saving file ~s~%~a" file condition))
            (list 1 (format nil "Data saved in file ~s" file))))
      (list 0 (format nil "No current model could not save data in  ~S" file)))))

(defun clear-history-recorder (instance)
  (setf (history-recorder-traces instance) nil)
  (setf (history-recorder-buffers instance) nil))


;;; support for the text traces since they don't have a separate
;;; file at this point

(defun get-saved-text-trace ()
  (get-saved-trace :detail t))

(defun text-history-trace (&key detail start end key)
  (let ((full-trace (get-history-information :save-trace key))
        (output (make-string-output-stream)))
    (dolist (x full-trace (get-output-stream-string output))
      (when (and (or (null start) (>= (first x) (seconds->ms start)))
                 (or (null end) (<= (first x) (seconds->ms end)))
                 (or (eq detail t)
                     (case detail
                       (low (eq (second x) 'low))
                       (medium (or (eq (second x) 'low)
                                   (eq (second x) 'medium)))
                       (high (second x)))))
        (format output "~a~%" (third x))))))
  
  
(define-module-fct :history-recorder nil nil
  :creation (lambda (x) (declare (ignore x)) (make-history-recorder))
  :version "1.0a1"
  :documentation "Module to support Environment saving and replaying trace information.")
        
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

