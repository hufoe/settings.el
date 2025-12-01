;;; cntlpanel.el --- A simple Emacs GUI for managing system settings.  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'wid-edit)
(require 'widget)
(require 'subr-x)
(require 'magit-section)
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
(defvar-local cntlpanel--updates nil)

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

(defun cntlpanel--widget-monitor-value-create (monitor-widget)
  ""
  (let ((monitors (widget-get monitor-widget :value)))
    (if (not monitors)
        (widget-create-child-and-convert
         monitor-widget
         'item
         :value
         "")
      (let ((primary-monitor (cntlpanel--get-primary-monitor monitors))
            (monitor-name-len (thread-last monitors
                                           (cl-mapcar #'cntlpanel--monitor-name)
                                           (cl-mapcar #'length)
                                           (apply #'max)
                                           (+ 2))))
        (widget-create-child-and-convert
         monitor-widget
         'item
         :format "%v"
         :value
         (string-pad (cntlpanel--monitor-name primary-monitor)
                     monitor-name-len))
        (widget-create-child-and-convert
         monitor-widget
         'item
         :format "%v\n"
         :value
         (propertize "[Primary]"
                     'face
                     'bold))

        (dolist (monitor monitors)
          (when (not (cntlpanel--monitor-primary? monitor))
            (widget-create-child-and-convert
             monitor-widget
             'item
             :format "%v"
             :value
             (string-pad (cntlpanel--monitor-name monitor) monitor-name-len))
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
                     (widget (widget-create-child-and-convert
                              monitor-widget
                              'push-button
                              :notify widget-on-click
                              (propertize
                               (if (cntlpanel--monitor-mirrored monitor)
                                   text-field-mirrored
                                 text-field-not-mirrored)
                               'face 'link)))))
            (widget-create-child-and-convert
             monitor-widget
	         'item
             :value
             "")))))))

(define-widget 'cntlpanel--widget-monitor 'item
  ""
  :convert-widget #'widget-types-convert-widget
  :copy #'widget-types-copy
  :format "%v"
  :value-create #'cntlpanel--widget-monitor-value-create)

(defun cntlpanel--monitor-widget-available-check ()
  (cond ((not (executable-find xrandr-command))
         (list :cause (concat xrandr-command " isn't available")))
        ((or
          (not (null (getenv "WAYLAND_DISPLAY")))
          (null (getenv "DISPLAY"))
          (equal (getenv "XDG_SESSION_TYPE")
                 "wayland"))
         (list :cause "xrandr only works for w11"))
        (t
         t)))

(cl-defstruct cntlpanel--sink
  name
  id
  volume
  muted?)

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
                                         base-volume)
                              :muted? (not (equal (gethash "mute" sink-data)
                                                  :false)))))))
    (cl-map 'list
			parse-sink-data
            sinks-data)))

(defun cntlpanel--fetch-sinks-data (callback)
  ""
  (cntlpanel--async-process '("pactl" "--format=json" "list" "sinks")
                            (lambda (result)
                              (funcall callback (cntlpanel--parse-sinks-data result)))))

(defun cntlpanel--widget-volume-create (volume-widget)
  ""
  (let ((sinks (widget-get volume-widget :value)))
    (if (not sinks)
        (widget-create-child-and-convert
         volume-widget
         'item
         "")
      (dolist (sink sinks)
        (widget-create-child-and-convert
         volume-widget
         'item
         (cntlpanel--sink-name sink))

        (letrec ((volume-slider
                  (widget-create-child-and-convert
                   volume-widget
                   'cntlpanel-widget-volume-slider
                   :muted? (cntlpanel--sink-muted? sink)
                   :percentage (cntlpanel--sink-volume sink)
                   :length 20
                   :toggle (lambda (&rest _)
                             (cntlpanel--toggle-volume sink)

                             ;; sinks data will be refetched after next refresh
                             ;; and the widgets will be re-rendered
                             ;; the following code is only for ui responsiveness
                             (setf (cntlpanel--sink-muted? sink)
                                   (not (cntlpanel--sink-muted? sink)))
                             (widget-put volume-slider
                                         :muted? (cntlpanel--sink-muted? sink))
                             (widget-default-value-set volume-slider
                                                       nil))
                   :incr-volume (lambda (&rest _)
                                  (setf (cntlpanel--sink-volume sink)
                                        (+ (cntlpanel--sink-volume sink)
                                           cntlpanel-volume-step))
                                  (cntlpanel--set-volume sink (cntlpanel--sink-volume sink))

                                  (widget-put volume-slider
                                              :percentage (cntlpanel--sink-volume sink))
                                  (widget-default-value-set volume-slider
                                                            nil))
                   :dec-volume (lambda (&rest _)
                                 (setf (cntlpanel--sink-volume sink)
                                       (- (cntlpanel--sink-volume sink)
                                          cntlpanel-volume-step))
                                 (cntlpanel--set-volume sink (cntlpanel--sink-volume sink))

                                 (widget-put volume-slider
                                             :percentage (cntlpanel--sink-volume sink))
                                 (widget-default-value-set volume-slider
                                                           nil))))))
        (widget-create-child-and-convert
         volume-widget
         'item
         "")))))

(define-widget 'cntlpanel--widget-volume 'item
  ""
  :convert-widget #'widget-types-convert-widget
  :copy #'widget-types-copy
  :format "%v"
  :value-create #'cntlpanel--widget-volume-create)

(defun cntlpanel--volume-widget-available-check ()
  ""
  (if (executable-find pactl-command)
      t
    (list :cause (concat pactl-command
                         " isn't available"))))

(defun cntlpanel--toggle-volume (sink)
  ""
  (cl-assert (cntlpanel--sink-p sink))
  (cntlpanel--async-process (list pactl-command
                                  "set-sink-mute"
                                  (cntlpanel--sink-id sink)
                                  "toggle")))

(defun cntlpanel--set-volume (sink percentage)
  ""
  (cl-assert (cntlpanel--sink-p sink))
  (cntlpanel--async-process (list pactl-command
                                  "set-sink-volume"
                                  (cntlpanel--sink-id sink)
                                  (concat (format "%.2f" (* 100 percentage)) "%"))))

(defvar-local cntlpanel--refresh-timer nil)

(defvar cntlpanel-mode-map (make-composed-keymap
                            (define-keymap :parent widget-keymap)
                            magit-section-mode-map))

(define-derived-mode cntlpanel-mode magit-section-mode "cntlpanel" ()
  (setq-local cntlpanel-mode 1)
  (read-only-mode)
  (use-local-map cntlpanel-mode-map)
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
    ;; We need to wait until all the data fetching is finished, then call all the callbacks at once
    ;; to prevent flicking
    (let* ((updates cntlpanel--updates)
           (data-vector (make-vector (length updates) nil))
           (cnt 0)
           (call-updates (lambda ()
                           (with-current-buffer cntlpanel-buffer
                             (cl-loop for update in updates
                                      for data across data-vector
                                      do
                                      (funcall (plist-get update :callback)
                                               data))))))
      (cl-loop for index from 0 below (length updates)
               for update in updates
               do
               ;; cl-loop seems to work weridly with async callback
               ;; local binding all the variable
               (let* ((index index)
                      (update update)
                      (data-fetch (plist-get update :data-fetch)))
                 (funcall data-fetch
                          (lambda (data)
                            (setf (aref data-vector index)
                                  data)
                            (setq cnt (+ cnt 1))
                            (when (= cnt (length updates))
                              (funcall call-updates)))))))))

(cl-defstruct cntlpanel--section
  widget
  widget-header
  value-fetch
  avaiable-check)

(defun cntlpanel--register-update (data-fetch callback)
  (add-to-list 'cntlpanel--updates (list :data-fetch data-fetch
                                         :callback callback)))

(defun cntlpanel--init (cntlpanel-buffer)
  ""
  (with-current-buffer cntlpanel-buffer
    (let ((sections (list (make-cntlpanel--section :widget 'cntlpanel--widget-monitor
                                                   :widget-header "Monitors:\n"
                                                   :value-fetch #'cntlpanel--list-monitors
                                                   :avaiable-check #'cntlpanel--monitor-widget-available-check)
                          (make-cntlpanel--section :widget 'cntlpanel--widget-volume
                                                   :widget-header "Audio Volulme: \n"
                                                   :value-fetch #'cntlpanel--fetch-sinks-data
                                                   :avaiable-check #'cntlpanel--volume-widget-available-check)))
          (current-point (point)))
      (let ((inhibit-read-only t))
        (widget-insert (propertize "Control Panel\n\n" 'face 'bold))

        (magit-insert-section (magit-section "Root Section")
          (dolist (section sections)
            (let ((check (funcall (cntlpanel--section-avaiable-check section))))
              (if (equal check t)
                  (magit-insert-section (magit-section (cntlpanel--section-widget-header section))

                    (magit-insert-heading (insert (propertize (cntlpanel--section-widget-header section)
                                                              'face 'bold)))
                    (magit-insert-section-body
                      (insert "")
                      (let ((section-widget (widget-create (cntlpanel--section-widget section)
                                                           :value
                                                           nil)))
                        (cntlpanel--register-update (cntlpanel--section-value-fetch section)
                                                    (lambda (data)
                                                      (widget-default-value-set section-widget
                                                                                data)))
                        section-widget)
                      ;; an extra "\n" seems to make magit-section work with async value set
                      (insert "\n")))
                (message (concat "Warning: " (cntlpanel--section-widget-header section)
                                 " isn't available, cause: "
                                 " " (plist-get check :cause)))))))
        (goto-char current-point)))))

(defun cntlpanel ()
  ""
  (interactive)
  (let ((cntlpanel-buffer (get-buffer-create "*cntlpanel*")))
    (pop-to-buffer cntlpanel-buffer)
    (with-current-buffer cntlpanel-buffer
      (unless cntlpanel-mode
        (setq-local cntlpanel-mode 1)
        (cntlpanel-mode)
        (cntlpanel--init cntlpanel-buffer)))))

(provide 'cntlpanel)
;;; cntlpanel.el ends here
