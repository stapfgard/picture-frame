(require 'posframe)
(require 'request)

(defconst picture-frame-buffer " *picture-frame-buffer*")
(defvar picture-frame-active nil)
(defvar picture-frame-frame-active nil)
(defvar picture-frame-frame-height (lambda () 450))
(defvar picture-frame-frame-width (lambda () 590))
(defvar picture-frame-poshandler 'posframe-poshandler-frame-bottom-right-corner)
(defvar picture-frame-timer nil)
(defvar picture-frame-timer-interval 1)
(defvar picture-frame-url (lambda () (message "No picture-frame-url.")))
(defvar picture-frame-url-got nil)

(defun picture-frame-load ()
	(when (posframe-workable-p)
		(if picture-frame-frame-active
			(posframe-refresh picture-frame-buffer)
			(posframe-show picture-frame-buffer
				:background-color "white"
				:keep-ratio t
				:poshandler picture-frame-poshandler
			)
		)
	)
)

(defun picture-frame-start ()
	(interactive)
	(message "picture-frame: started.")
	(setq picture-frame-timer (run-with-timer picture-frame-timer-interval t (lambda ()
		(let ((url (funcall picture-frame-url (current-time))))
			(when (not (eq url picture-frame-url-got)) (picture-frame-update url))
		)
	)))
	(setq picture-frame-active t)
)

(defun picture-frame-stop ()
	(interactive)
	(when picture-frame-timer (cancel-timer picture-frame-timer))
	(when (posframe-workable-p) (posframe-delete picture-frame-buffer))
	(setq picture-frame-frame-active nil)
	(setq picture-frame-active nil)
	(message "picture-frame stopped.")
)

(defun picture-frame-toggle ()
	(interactive)
	(if picture-frame-active
		(picture-frame-stop)
		(picture-frame-start)
	)
)

(defun picture-frame-update (url)
	(request url
		:type
			"GET"
		:parser
			'buffer-string
		:success
			(cl-function (lambda (&key data &allow-other-keys) (when data
				(with-current-buffer (get-buffer picture-frame-buffer)
					(erase-buffer)
					(insert-image (create-image
						(encode-coding-string data 'utf-8) 'jpeg t
						:height (funcall picture-frame-frame-height)
						:pointer 'arrow
						:width (funcall picture-frame-frame-width)
					))
				)
				(picture-frame-load)
				(setq picture-frame-url-got url)
				(setq picture-frame-frame-active t)
			)))
		:error
			(cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
				(message "picture-frame error: %S" error-thrown)
			))
	)
)
;;TODO: Resize event.
;; (add-to-list 'window-size-change-functions (lambda (frame)
;; 	(posframe-refresh picture-frame-buffer)
;; ))

(provide 'picture-frame)
