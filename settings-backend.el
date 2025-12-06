;;; settings-backend.el --- Interface for system executables.  -*- lexical-binding: t -*-

;; SDPX-License-Identifier:  GPL-3.0-only
;; SPDX-FileCopyrightText: (c) 2025 Hufoe <foss@hufoe.com>
;; SPDX-FileCopyrightText: (c) 2025 Antero Mejr <mail@antr.me>

;;; Commentary:

;;; Code:

;; Custom variables

(defvar settings--xrandr-command "xrandr")
(defvar settings--pactl-command "pactl")
(defvar settings--osascript-command "osascript")

;; Interface code

(defun settings--async-process (commands &optional callback)
  (let ((proc
         (make-process :name "settings--async-process" :command commands
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

;; Audio backend(s)

(defun settings--volume-widget-available-check ()
  (cond ((eq system-type 'darwin)
         (if (executable-find settings--osascript-command)
             t
           (list :cause (concat settings--osascript-command " isn't available"))))
        ((eq system-type 'gnu/linux)
         (if (executable-find settings--pactl-command)
             t
           (list :cause (concat settings--pactl-command " isn't available"))))
        (t (list :cause "unsupported platform"))))

(cl-defstruct settings--sink
  name
  id
  volume
  muted?)

(defun settings--parse-sinks-data (json-str)
  (let ((sinks-data (json-parse-string json-str))
        (parse-sink-data (lambda (sink-data)
                           (let ((base-volume (gethash "value" (gethash "base_volume" sink-data))))
                             (make-settings--sink
                              :id (gethash "name" sink-data)
                              :name (gethash "description" sink-data)
                              :volume (/ (float (thread-last (gethash "volume" sink-data)
                                                             (gethash "front-left")
                                                             (gethash "value")))
                                         base-volume)
                              :muted? (not (equal (gethash "mute" sink-data)
                                                  :false)))))))
    (cl-map 'list parse-sink-data sinks-data)))

(defvar settings--osascript-rx
  "output volume:\\([[:digit:]]+\\), .*, alert volume:\\([[:digit:]]+\\), output muted:\\(.*\\)$")

(defun settings--parse-sinks-data-darwin (str)
  (let* ((matches (string-match settings--osascript-rx str))
         (outvol (/ (string-to-number (match-string 1 str)) 100.0))
         (muted (match-string 3 str))
         (muted (string= muted "true")))
    (list (make-settings--sink :name "default" :id "default" :volume outvol :muted? muted))))

(defun settings--fetch-sinks-data (callback)
  (cond ((eq system-type 'darwin)
         (settings--async-process (list settings--osascript-command
                                        "-e" "get volume settings")
                                  (lambda (result)
                                    (funcall callback (settings--parse-sinks-data-darwin result)))))
        ((eq system-type 'gnu/linux)
         (settings--async-process (list settings--pactl-command "--format=json" "list" "sinks")
                                  (lambda (result)
                                    (funcall callback (settings--parse-sinks-data result)))))))

(defun settings--toggle-volume (sink)
  (cl-assert (settings--sink-p sink))
  (cond ((eq system-type 'darwin)
         (if (settings--sink-muted? sink)
             (settings--async-process (list settings--osascript-command
                                            "-e" "set volume output muted no"))
           (settings--async-process (list settings--osascript-command
                                          "-e" "set volume output muted yes"))))
        ((eq system-type 'gnu/linux)
         (settings--async-process (list settings--pactl-command
                                        "set-sink-mute"
                                        (settings--sink-id sink)
                                        "toggle")))))

(defun settings--set-volume (sink percentage)
  (cl-assert (settings--sink-p sink))
  (cond ((eq system-type 'darwin)
         ;; NOTE: MacOS disables mute after a volume adjustment. That behavior
         ;; does not seem to be controllable by re-muting the output. Maybe
         ;; MacOS disables muting for some amount of time after settings volume?
         (settings--async-process (list settings--osascript-command
                                        "-e" (format "set volume output volume %i"
                                                     (* 100 percentage)))))
        ((eq system-type 'gnu/linux)
         (settings--async-process (list settings-pactl-command
                                        "set-sink-volume"
                                        (settings--sink-id sink)
                                        (concat (format "%.2f" (* 100 percentage)) "%"))))))

;; Monitor backend(s)

(cl-defstruct settings--monitor
  name
  id
  primary?
  mirrored
  mirror-src
  disabled?
  offsetx
  offsety)

(defvar settings--parse-xrandr-rx (rx line-start
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

(defun settings--edid-get-name (edid-str)
  (let ((_ (string-match (rx (*? anychar) "000000FC00" (group (= 24 alnum)) (* anychar)) edid-str)))
    (thread-last (seq-partition (match-string 1 edid-str) 2)
                 (cl-mapcar (lambda (s)
                              (string-to-number s 16)))
                 (cl-mapcar #'char-to-string)
                 (string-join)
                 (string-clean-whitespace))))

(defun settings--parse-xrandr (output)
  (let* ((lines (split-string output "\n")))
    (cl-labels ((parse-monitors (monitors lines)
                  (if (null lines)
                      (reverse monitors)
                    (let ((line (cl-first lines)))
                      (cond ((string-match-p "^[[:blank:]]*EDID:[[:blank:]]*" line)
                             (let ((monitor-readable-name
                                    (settings--edid-get-name
                                     (thread-last (cdr (take 17 lines))
                                                  (cl-mapcar (lambda (l)
                                                               (string-clean-whitespace l)))
                                                  (string-join)))))
                               (setf (settings--monitor-name (cl-first monitors))
                                     monitor-readable-name)
                               (parse-monitors monitors (drop 17 lines))))
                            ((string-match settings--parse-xrandr-rx
                                           line)
                             (let* ((name (match-string 1 line))
                                    (primary (match-string 2 line))
                                    (offsetx-str (match-string 3 line))
                                    (offsety-str (match-string 4 line))
                                    (monitor (make-settings--monitor
                                              :name name
                                              :id name
                                              :primary? (and (not (null primary))
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

(defun settings--get-primary-monitor (monitors)
  (car (cl-remove-if-not #'settings--monitor-primary? monitors)))

(defun settings--monitor-widget-available-check ()
  (cond ((not (executable-find settings-xrandr-command))
         (list :cause (concat settings--xrandr-command " isn't available")))
        ((or
          (not (null (getenv "WAYLAND_DISPLAY")))
          (null (getenv "DISPLAY"))
          (equal (getenv "XDG_SESSION_TYPE")
                 "wayland"))
         (list :cause "xrandr only works for X11"))
        (t t)))

(defun settings--set-if-mirrored (monitors)
  (let ((primary-monitor (settings--get-primary-monitor monitors)))
    (dolist (monitor monitors)
      (when (and
             (not (settings--monitor-primary? monitor))
             (equal (settings--monitor-offsetx primary-monitor)
                    (settings--monitor-offsetx monitor))
             (equal (settings--monitor-offsety primary-monitor)
                    (settings--monitor-offsety monitor)))
        (setf (settings--monitor-mirrored monitor)
              t)
        (setf (settings--monitor-mirror-src monitor)
              primary-monitor)))))

(defun settings--list-monitors (callback)
  (settings--async-process
   (list settings-xrandr-command "--prop")
   (lambda (output)
     (let* ((monitors (settings--parse-xrandr output)))
       (settings--set-if-mirrored monitors)
       (funcall callback monitors)))))

(defun settings--mirror-monitor (dest-monitor src-monitor)
  (cl-assert (settings--monitor-p dest-monitor))
  (cl-assert (settings--monitor-p src-monitor))
  (settings--async-process
   (list settings-xrandr-command "--output" (settings--monitor-id dest-monitor)
         "--same-as" (settings--monitor-id src-monitor)))
  (setf (settings--monitor-mirrored dest-monitor) t)
  (setf (settings--monitor-mirror-src dest-monitor) src-monitor))

(defun settings--unmirror-monitor (dest-monitor src-monitor)
  (cl-assert (settings--monitor-p dest-monitor))
  (cl-assert (settings--monitor-p src-monitor))
  (settings--async-process
   (list settings-xrandr-command "--output" (settings--monitor-id dest-monitor)
         "--left-of" (settings--monitor-id src-monitor)))
  (setf (settings--monitor-mirrored dest-monitor) nil)
  (setf (settings--monitor-mirror-src dest-monitor) nil))

(defun settings--disable-monitor (monitor)
  (cl-assert (settings--monitor-p monitor))
  (if (settings--monitor-primary? monitor)
      (error "Can't disable a primary monitor"))
  (settings--async-process (list settings-xrandr-command
                                 "--output"
                                 (settings--monitor-id monitor)
                                 "--off")))

(defun settings--enable-monitor (monitor)
  (cl-assert (settings--monitor-p monitor))
  (settings--async-process (list settings-xrandr-command
                                 "--output"
                                 (settings--monitor-id monitor)
                                 "--auto")))

(defun settings--enable-and-mirror (dest-monitor src-monitor)
  (cl-assert (settings--monitor-p dest-monitor))
  (cl-assert (settings--monitor-p src-monitor))
  (settings--async-process (list settings-xrandr-command
                                 "--output"
                                 (settings--monitor-id dest-monitor)
                                 "--auto"
                                 "--same-as" (settings--monitor-id src-monitor))))

(provide 'settings-backend)

;;; settings-backend.el ends here
