(defpackage :mplayer
  (:use :cl :sb-ext)
  (:export #:play
	   #:*mplayer-defaults*
	   #:send-command
	   #:video-play-problem
	   #:process-status-change
	   #:signal-status-changes
	   #:print-status))

(in-package :mplayer)

;;; defaults --really should be replaced by defaults file and handle by the defaults package.

(defparameter *mplayer-defaults* '("-panscan" "1"  "-volume" "5" "-loop" "0" "-quiet" "-slave" "-vo" "gl3")
  "a list of strings to be fed to the mplayer executable, formatted as per man page")
(defparameter *fail* 0)


;;; conditions and errors

(define-condition video-play-problem (condition)
  ((file :initarg :file :reader file)
   (status :initarg :status :reader status)))

(define-condition process-status-change (condition)
  ((status :initarg :status :reader status)
   (code :initarg :code :reader code)
   (pid :initarg :pid :reader pid)))

(defun print-status (condition)
  "Prints out pid, status, and exit code from a condition"
  (incf *fail*)
  (format nil "Status: ~a Pid: ~a Exit-code: ~a" (status condition) (pid condition) (code condition)))


(defun signal-status-changes (process)
  "signals a condition based on how a process exited"
  (let ((status (sb-ext:process-status process))
	(code (sb-ext:process-exit-code process))
	(pid (sb-ext:process-pid process)))
    (format t "shits going down!")
    (signal  'process-status-change :status status :code code :pid pid)))


(defun playv1 (file &key window (options *mplayer-defaults*) (status-hook #'print-status))
  "Plays file with mplayer. You can specify an X11 window to be parented to, or options in addition to a status hook besides the basic default one."
  (let ((arglist options))
    (when window (setf arglist (concatenate 'list arglist `("-wid" ,(princ-to-string (xlib:window-id window))))))
    (let* ((mp-process (sb-ext:run-program "/usr/bin/mplayer"
					   `(,file ,@arglist)
					   :wait nil :input :stream :output :stream :status-hook status-hook))
	   (status (process-status mp-process)))    
      (if (eq status :running)
	  mp-process
	  (signal :video-play-problem :file file :status status)))))

(defun play (file &key window (options *mplayer-defaults*) (status-hook #'signal-status-changes))
  "Plays file with mplayer. You can specify an X11 window to be parented to, or options in addition to a status hook besides the basic default one."
  (let ((arglist options))
    (when window (setf arglist (concatenate 'list arglist `("-wid" ,(princ-to-string (xlib:window-id window))))))
    (sb-ext:run-program "/usr/bin/mplayer"
			  `(,file ,@arglist)
			  :wait nil :input :stream :output :stream :status-hook status-hook)))

(defun send-command (mplayer-process command)
  "Send the text string command, as per slave.txt, to mplayers input stream. Automatically adds a newline."
  (let ((input-stream (sb-ext:process-input mplayer-process)) )
    (format input-stream "~a~%" command )
    (finish-output input-stream)))
