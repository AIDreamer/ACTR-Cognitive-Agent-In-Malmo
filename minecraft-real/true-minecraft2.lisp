(clear-all)
(defmethod device-handle-keypress ((device list) key)
	(model-output "Model pressed key " key))