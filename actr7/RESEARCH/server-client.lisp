; A SAMPLE LISP SERVER

(ql:quickload "usocket")

; Function for creating server
(defun create-server (port)
	(let* ((socket (usocket:socket-listen "127.0.0.1" port))
			(connection (usocket:socket-accept socket :element-type 'character)))
	(unwind-protect
		(progn
			(format (usocket:socket-stream connection) "Hello World %")
			(finish-output (usocket:socket-stream connection)))
		(progn
			(format t "Closing sockets~%")
			(usocket:socket-close connection)
			(usocket:socket-close socket))
	)
	)
)

(defun create-client-A (port)
	(let ((socket (usocket:socket-connect "127.0.0.1" port :element-type 'character)))
		(unwind-protect
			(progn
				;(usocket:wait-for-input socket)
				;(format t "~A~%" (read-line (usocket:socket-stream socket)))
				;(format t "Read the thing %")
				(format (usocket:socket-stream socket) "A")
				(finish-output (usocket:socket-stream socket))
				(format t "Sent it"))
			(usocket:socket-close socket))))

(defun create-client-S (port)
	(let ((socket (usocket:socket-connect "127.0.0.1" port :element-type 'character)))
		(unwind-protect
			(progn
				;(usocket:wait-for-input socket)
				;(format t "~A~%" (read-line (usocket:socket-stream socket)))
				;(format t "Read the thing %")
				(format (usocket:socket-stream socket) "S")
				(finish-output (usocket:socket-stream socket))
				(format t "Sent it"))
			(usocket:socket-close socket))))

(defun create-client-W (port)
	(let ((socket (usocket:socket-connect "127.0.0.1" port :element-type 'character)))
		(unwind-protect
			(progn
				;(usocket:wait-for-input socket)
				;(format t "~A~%" (read-line (usocket:socket-stream socket)))
				;(format t "Read the thing %")
				(format (usocket:socket-stream socket) "W")
				(finish-output (usocket:socket-stream socket))
				(format t "Sent it"))
			(usocket:socket-close socket))))

(defun create-client-D (port)
	(let ((socket (usocket:socket-connect "127.0.0.1" port :element-type 'character)))
		(unwind-protect
			(progn
				;(usocket:wait-for-input socket)
				;(format t "~A~%" (read-line (usocket:socket-stream socket)))
				;(format t "Read the thing %")
				(format (usocket:socket-stream socket) "D")
				(finish-output (usocket:socket-stream socket))
				(format t "Sent it"))
			(usocket:socket-close socket))))