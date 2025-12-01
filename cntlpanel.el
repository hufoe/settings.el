;;; cntlpanel.el --- A lsp-bridge Flymake backend -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'wid-edit)
(require 'widget)
(require 'subr-x)
(require 'cntlpanel-widgets)

(cl-defstruct cntlpanel--monitor
  name
  id
  primary?
  mirrored
  mirror-src
  disabled?
  offsetx
  offsety)

(defvar xrandr-command "xrandr")
(defvar pactl-command "pactl")

(defgroup cntlpanel nil "Cntlpanel group.")

(defcustom cntlpanel-refresh-rate 2
  "Refresh rate of cntlpanel, in seconds."
  :group 'cntlpanel)

(defcustom cntlpanel-volume-step 0.02
  "Single step value for volume change, in decimal percentage."
  :group 'cntlpanel)

(defcustom cntlpanel-unmirror-default-action 'standalone
  "Default action after a monitor been unmirrored.

'STANDALONE: unmirror the monitor, keep it enable and work indepedently
'DISABLE: disable the monitor"
  :type '(symbol)
  :options '(disable standalone))

(defvar-local cntlpanel-mode nil)

(defun cntlpanel--async-process (commands &optional callback)
  (let ((proc
         (make-process :name "xrandr" :command commands
                       :buffer (generate-new-buffer (make-temp-name " cntl--async-process")))))
    (set-process-sentinel proc (lambda (_ _)
                                 (unless (process-live-p proc)
                                   (with-current-buffer (process-buffer proc)
                                     (when callback (funcall callback (buffer-string)))
                                     (kill-buffer (current-buffer))))))
    (set-process-filter proc (lambda (_ string)
                               (with-current-buffer (process-buffer proc)
                                 (goto-char (point-max))
                                 (insert string))))))

(defun cntlpanel--parse-xrandr-rx-gen ()
  (rx
   line-start
   (group (* (or punct alnum)))
   " connected"
   (group (? " primary"))
   (? " " (*? digit)
      "x"
      (*? digit)
      "+"
      (group (*? digit))
      "+"
      (group (*? digit)))
   (* anychar)))

(defvar cntlpanel--parse-xrandr-rx (cntlpanel--parse-xrandr-rx-gen))


(defun cntlpanel--edid-get-name (edid-str)
  (let ((_ (string-match (rx (*? anychar) "000000FC00" (group (= 24 alnum)) (* anychar)) edid-str)))
    (thread-last (seq-partition (match-string 1 edid-str) 2)
                 (cl-mapcar (lambda (s)
                              (string-to-number s 16)))
                 (cl-mapcar #'char-to-string)
                 (string-join)
                 (string-clean-whitespace))))

(defun cntlpanel--parse-xrandr (output)
  ""
  (let* ((lines (split-string output "\n")))
    (cl-labels ((parse-monitors (monitors lines)
                  (if (null lines)
                      (reverse monitors)
                    (let ((line (cl-first lines)))
                      (cond ((string-match-p "^[[:blank:]]*EDID:[[:blank:]]*" line)
                             (let ((monitor-readable-name
                                    (cntlpanel--edid-get-name
                                     (thread-last (cdr (take 17 lines))
                                                  (cl-mapcar (lambda (l)
                                                               (string-clean-whitespace l)))
                                                  (string-join)))))
                               (setf (cntlpanel--monitor-name (cl-first monitors))
                                     monitor-readable-name)
                               (parse-monitors monitors (drop 17 lines))))
                            ((string-match cntlpanel--parse-xrandr-rx
                                           line)
                             (let* ((name (match-string 1 line))
                                    (primary (match-string 2 line))
                                    (offsetx-str (match-string 3 line))
                                    (offsety-str (match-string 4 line))
                                    (monitor (make-cntlpanel--monitor
                                              :name name :id name :primary? (and (not (null primary))
                                                                                 (not (string-empty-p primary)))
                                              :offsetx (if offsetx-str
                                                           (string-to-number offsetx-str) nil)
                                              :offsety (if offsety-str
                                                           (string-to-number offsety-str) nil)
                                              :disabled? (null offsetx-str))))
                               (parse-monitors (cons monitor monitors)
                                               (cdr lines))))
                            (t
                             (parse-monitors monitors
                                             (cdr lines))))))))
      (parse-monitors nil lines))))

(defun cntlpanel--get-primary-monitor (monitors)
  ""
  (car (cl-remove-if-not #'cntlpanel--monitor-primary? monitors)))

(defun cntlpanel--set-if-mirrored (monitors)
  ""
  (let ((primary-monitor (cntlpanel--get-primary-monitor monitors)))
    (dolist (monitor monitors)
      (when (and
             (not (cntlpanel--monitor-primary? monitor))
             (equal (cntlpanel--monitor-offsetx primary-monitor)
                    (cntlpanel--monitor-offsetx monitor))
             (equal (cntlpanel--monitor-offsety primary-monitor)
                    (cntlpanel--monitor-offsety monitor)))
        (setf (cntlpanel--monitor-mirrored monitor)
              t)
        (setf (cntlpanel--monitor-mirror-src monitor)
              primary-monitor)))))

(defun cntlpanel--list-monitors (callback)
  ""
  (cntlpanel--async-process
   (list "xrandr" "--prop")
   (lambda (output)
     (let* ((monitors (cntlpanel--parse-xrandr output)))
       (cntlpanel--set-if-mirrored monitors)
       (funcall callback
                monitors)))))

(defun cntlpanel--mirror-monitor (dest-monitor src-monitor)
  ""
  (cl-assert (cntlpanel--monitor-p dest-monitor))
  (cl-assert (cntlpanel--monitor-p src-monitor))
  (cntlpanel--async-process
   (list "xrandr" "--output" (cntlpanel--monitor-id dest-monitor)
         "--same-as" (cntlpanel--monitor-id src-monitor)))
  (setf (cntlpanel--monitor-mirrored dest-monitor) t)
  (setf (cntlpanel--monitor-mirror-src dest-monitor) src-monitor))

(defun cntlpanel--unmirror-monitor (dest-monitor src-monitor)
  ""
  (cl-assert (cntlpanel--monitor-p dest-monitor))
  (cl-assert (cntlpanel--monitor-p src-monitor))
  (cntlpanel--async-process
   (list "xrandr" "--output" (cntlpanel--monitor-id dest-monitor)
         "--left-of" (cntlpanel--monitor-id src-monitor)))
  (setf (cntlpanel--monitor-mirrored dest-monitor) nil)
  (setf (cntlpanel--monitor-mirror-src dest-monitor) nil))

(defun cntlpanel--disable-monitor (monitor)
  ""
  (cl-assert (cntlpanel--monitor-p monitor))
  (if (cntlpanel--monitor-primary? monitor)
      (error "Can't disable a primary monitor"))
  (cntlpanel--async-process (list xrandr-command
                                  "--output"
                                  (cntlpanel--monitor-id monitor)
                                  "--off")))

(defun cntlpanel--enable-monitor (monitor)
  ""
  (cl-assert (cntlpanel--monitor-p monitor))
  (cntlpanel--async-process (list xrandr-command
                                  "--output"
                                  (cntlpanel--monitor-id monitor)
                                  "--auto")))

(defun cntlpanel--enable-and-mirror (dest-monitor src-monitor)
  ""
  (cl-assert (cntlpanel--monitor-p dest-monitor))
  (cl-assert (cntlpanel--monitor-p src-monitor))
  (cntlpanel--async-process (list xrandr-command
                                  "--output"
                                  (cntlpanel--monitor-id dest-monitor)
                                  "--auto"
                                  "--same-as" (cntlpanel--monitor-id src-monitor))))

(defun cntlpanel--monitors-control (monitors)
  ""
  (let ((primary-monitor (cntlpanel--get-primary-monitor monitors))
        (monitor-name-len (thread-last monitors
                                       (cl-mapcar #'cntlpanel--monitor-name)
                                       (cl-mapcar #'length)
                                       (apply #'max)
                                       (+ 2))))
    (widget-insert (string-pad (cntlpanel--monitor-name primary-monitor)
                               monitor-name-len))
    (widget-insert (propertize "[Primary]"
                               'face
                               'bold))
    (widget-insert "\n")
    (dolist (monitor monitors)
      (when (not (cntlpanel--monitor-primary? monitor))
        (widget-insert (string-pad (cntlpanel--monitor-name monitor) monitor-name-len))
        (letrec ((text-field-mirrored "Unmirror")
                 (text-field-not-mirrored "Mirror")
                 (widget-on-click (lambda (&rest _)
                                    (cond ((cntlpanel--monitor-mirrored monitor)
                                           (if (equal cntlpanel-unmirror-default-action
                                                      'disable)
                                               (cntlpanel--disable-monitor monitor)
                                             (cntlpanel--unmirror-monitor monitor
                                                                          (cntlpanel--monitor-mirror-src monitor)))
                                           (widget-value-set widget text-field-not-mirrored))
                                          (t
                                           (if (equal cntlpanel-unmirror-default-action
                                                      'disable)
                                               (cntlpanel--enable-and-mirror monitor primary-monitor)
                                             (cntlpanel--mirror-monitor monitor
                                                                        primary-monitor))
                                           (widget-value-set widget text-field-mirrored)))))
                 (widget (widget-create
                          'push-button
                          :notify widget-on-click
                          (propertize (if (cntlpanel--monitor-mirrored monitor)
                                          text-field-mirrored
                                        text-field-not-mirrored)
                                      'face 'link)))))
        (widget-insert "\n")))))

(cl-defstruct cntlpanel--sink
  name
  id
  volume)

(defun cntlpanel--parse-sinks-data (json-str)
  ""
  (let ((sinks-data (json-parse-string json-str))
        (parse-sink-data (lambda (sink-data)
                           (let ((base-volume (gethash "value" (gethash "base_volume" sink-data))))
                             (make-cntlpanel--sink
                              :id (gethash "name" sink-data)
                              :name (gethash "description" sink-data)
                              :volume (/ (float (thread-last (gethash "volume" sink-data)
                                                             (gethash "front-left")
                                                             (gethash "value")))
                                         base-volume))))))
    (cl-map 'list
			parse-sink-data
            sinks-data)))

(defun cntlpanel--fetch-sinks-data (callback)
  ""
  (cntlpanel--async-process '("pactl" "--format=json" "list" "sinks")
                            (lambda (result)
                              (funcall callback (cntlpanel--parse-sinks-data result)))))

(defun cntlpanel--volume-control (sinks)
  ""
  (widget-insert "Volume Control")
  (dolist (sink sinks)
    (widget-insert (cntlpanel--sink-name sink))
    (widget-insert "\n")
    (widget-create 'cntlpanel-widget-slider
                   :percentage (cntlpanel--sink-volume sink)
                   :length 20)))

(defun cntlpanel--set-volume (sink percentage)
  (cl-assert (cntlpanel--sink-p sink))
  (cntlpanel--async-process (list pactl-command
                                  "set-sink-volume"
                                  (cntlpanel--sink-id sink)
                                  (concat (format "%.2f" (* 100 percentage)) "%"))))

(defvar-local cntlpanel--refresh-timer nil)

(defvar-keymap cntlpanel-mode-map :parent widget-keymap)

(define-derived-mode cntlpanel-mode special-mode "cntlpanel" ()
  (read-only-mode)
  (use-local-map widget-keymap)
  (letrec ((cntlpanel-buffer (current-buffer))
           (refresh-timer
            (run-with-timer 0 cntlpanel-refresh-rate
                            (lambda ()
                              (if (buffer-live-p cntlpanel-buffer)
                                  (cntlpanel--refresh cntlpanel-buffer)
                                (cancel-timer refresh-timer))))))
    (setq-local cntlpanel--refresh-timer
                refresh-timer)))

(defun cntlpanel--refresh (cntlpanel-buffer)
  ""
  (with-current-buffer cntlpanel-buffer
    (cntlpanel--list-monitors
     (lambda (monitors)
       (with-current-buffer cntlpanel-buffer
         (cntlpanel--fetch-sinks-data
          (lambda (sinks)
            (with-current-buffer cntlpanel-buffer
              (let ((current-point (point)))
                (let ((inhibit-read-only t))
                  (erase-buffer))
                (widget-insert (propertize "Control Panel\n\n" 'face 'bold))
                (widget-insert (propertize "Monitors:\n" 'face 'bold))
                (cntlpanel--monitors-control monitors)
                (widget-insert "\n")
                (cntlpanel--volume-control sinks)
                (goto-char current-point))))))))))

(defun cntlpanel ()
  ""
  (interactive)
  (let ((cntlpanel-buffer (get-buffer-create "*cntlpanel*")))
    (with-current-buffer cntlpanel-buffer
      (unless cntlpanel-mode
        (setq cntlpanel-mode 1)
        (cntlpanel-mode))
      (cntlpanel--refresh cntlpanel-buffer)
      (pop-to-buffer cntlpanel-buffer))))

(provide 'cntlpanel)
