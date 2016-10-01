;;;; LOADER FOR MINECRAFT
;;;; Load this file after running environment.py
;;;; (load "C:/cygwin/home/Son Pham/Cognitive-Agents-in-Malmo/minecraft-real/loader.lisp")

(load "~/quicklisp/setup.lisp")
(load "../actr7/load-act-r.lisp")
(load "./true-minecraft.lisp")
(sgp)
(run 120 :real-time t)