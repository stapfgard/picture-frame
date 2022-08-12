(require 'posframe)
(require 'request)

(defconst picture-frame-buffer " *picture-frame-buffer*")
(defcustom picture-frame-image-conversion 'enabled "image conversion")
(defcustom picture-frame-image-pointer 'arrow "image pointer")
(defcustom picture-frame-image-relief 0 "image relief")
(defcustom picture-frame-image-rotation 0 "image rotation")
(defcustom picture-frame-image-size (lambda (frame) '(960 540)) "image size")
(defcustom picture-frame-timer-interval 1 "timer interval")
(defcustom picture-frame-url (lambda () (message "No picture-frame-url.")) "url")
(defvar picture-frame-data nil)
(defvar picture-frame-poshandler 'posframe-poshandler-frame-bottom-right-corner)
(defvar picture-frame-timer nil)
(defvar picture-frame-url-got nil)

(defun picture-frame-load ()
	(when (posframe-workable-p)
		(let*
			(
				(size (funcall picture-frame-image-size (frame-root-window)))
				(width (nth 0 size))
				(height (nth 1 size))
			)
			(with-current-buffer (get-buffer-create picture-frame-buffer)
				(erase-buffer)
				(insert-image
					(create-image (encode-coding-string picture-frame-data 'utf-8) nil t
						:conversion picture-frame-image-conversion
						:height height
						:pointer 'picture-frame-image-pointer
						:relief picture-frame-image-relief
						:rotation picture-frame-image-rotation
						:width width
					)
				)
			)
			(posframe-show picture-frame-buffer
				:keep-ratio t
				:poshandler picture-frame-poshandler
			)
		)
	)
)

(defun picture-frame-start ()
	(interactive)
	(message "picture-frame: started.")
	(setq picture-frame-timer (run-with-timer picture-frame-timer-interval 1 (lambda ()
		(let ((url (funcall picture-frame-url (current-time))))
			(when (not (string= url picture-frame-url-got)) (picture-frame-update url))
			(setq picture-frame-url-got url)
		)
	)))
)

(defun picture-frame-stop ()
	(interactive)
	(setq picture-frame-url-got nil)
	(when picture-frame-timer (cancel-timer picture-frame-timer))
	(when (posframe-workable-p) (posframe-delete picture-frame-buffer))
	(message "picture-frame: stopped.")
)

(defun picture-frame-update (url)
	(request url
		:type
			"GET"
		:parser
			'buffer-string
		:success
			(cl-function (lambda (&key data &allow-other-keys)
				(when data
					(setq picture-frame-data data)
					(picture-frame-load)
				)
			))
		:error
			(cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
				(message "picture-frame: error occured. (%s)" error-thrown)
			))
	)
)

(add-hook 'window-size-change-functions (lambda (frame) (when picture-frame-mode (picture-frame-load))))

;;;###autoload
(define-minor-mode picture-frame-mode
	"Toggle picture frame mode."
	:lighter
	  " picture frame"
	(if picture-frame-mode (picture-frame-start) (picture-frame-stop))
)

(provide 'picture-frame)
