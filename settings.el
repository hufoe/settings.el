;;; settings.el --- A simple Emacs GUI for managing system settings.  -*- lexical-binding: t -*-

;; SDPX-License-Identifier:  GPL-3.0-only
;; SPDX-FileCopyrightText: (c) 2025 Hufoe <foss@hufoe.com>
;; SPDX-FileCopyrightText: (c) 2025 Antero Mejr <mail@antr.me>
;; SPDX-FileContributor: 2025 Zephyr Yang <me@coruscation.net>

;;; Commentary:

;;; Code:
(require 'wid-edit)
(require 'widget)
(require 'subr-x)
(require 'magit-section)
(require 'settings-widgets)
(require 'settings-backend)

;; Custom variables

(defgroup settings nil "Settings group.")

(defvaralias 'settings-xrandr-command 'settings--xrandr-command)
(defvaralias 'settings-pactl-command 'settings--pactl-command)

(defcustom settings-refresh-rate 2
  "Refresh rate of the settings UI, in seconds."
  :group 'settings)

(defcustom settings-volume-step 0.02
  "Single step value for volume change, in decimal percentage."
  :group 'settings)

(defcustom settings-set-volume-warning-threshold 1.0
  "Warning when you want to set volume above this level when clicking on the volume slider bar."
  :group 'settings)

(defcustom settings-unmirror-default-action 'standalone
  "Default action after a monitor been unmirrored.

'STANDALONE: unmirror the monitor, keep it enabled and working independently
'DISABLE: disable the monitor"
  :type '(symbol)
  :options '(disable standalone)
  :group 'settings)

;; UI code

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

(defun settings--widget-monitor-value-create (monitor-widget)
  (let ((monitors (widget-get monitor-widget :value)))
    (if (not monitors)
        (widget-create-child-and-convert
         monitor-widget
         'item
         :value
         "")
      (let ((primary-monitor (settings--get-primary-monitor monitors))
            (monitor-name-len (thread-last monitors
                                           (cl-mapcar #'settings--monitor-name)
                                           (cl-mapcar #'length)
                                           (apply #'max)
                                           (+ 2))))
        (widget-create-child-and-convert
         monitor-widget
         'item
         :format "%v"
         :value
         (string-pad (settings--monitor-name primary-monitor)
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
          (when (not (settings--monitor-primary? monitor))
            (widget-create-child-and-convert
             monitor-widget
             'item
             :format "%v"
             :value
             (string-pad (settings--monitor-name monitor) monitor-name-len))
            (letrec ((text-field-mirrored (propertize "Unmirror"
                                                      'font-lock-face 'link))
                     (text-field-not-mirrored (propertize "Mirror"
                                                          'font-lock-face 'link))
                     (widget-on-click (lambda (&rest _)
                                        (cond ((settings--monitor-mirrored monitor)
                                               (if (equal settings-unmirror-default-action
                                                          'disable)
                                                   (settings--disable-monitor monitor)
                                                 (settings--unmirror-monitor monitor
                                                                              (settings--monitor-mirror-src monitor)))
                                               (widget-value-set widget text-field-not-mirrored))
                                              (t
                                               (if (equal settings-unmirror-default-action
                                                          'disable)
                                                   (settings--enable-and-mirror monitor primary-monitor)
                                                 (settings--mirror-monitor monitor
                                                                            primary-monitor))
                                               (widget-value-set widget text-field-mirrored)))))
                     (widget (widget-create-child-and-convert
                              monitor-widget
                              'push-button
                              :notify widget-on-click
                              (propertize
                               (if (settings--monitor-mirrored monitor)
                                   text-field-mirrored
                                 text-field-not-mirrored)
                               'face 'link)))))
            (widget-create-child-and-convert
             monitor-widget
	         'item
             :value
             "")))))))

(defun settings--widget-volume-create (volume-widget)
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
         (settings--sink-name sink))

        (letrec ((set-volume (lambda (sink volume inhibit-volume-set-warning)
                               (let ((settings-buffer (current-buffer)))
                                 (when (or
                                        (< volume (max settings-set-volume-warning-threshold
                                                       (settings--sink-volume sink)))
                                        inhibit-volume-set-warning
                                        (yes-or-no-p (format "Do you want to set volume at %d%%" (* volume 100))))

                                   (settings--set-volume sink volume)

                                   ;; sinks data will be refetched after next refresh
                                   ;; and the widgets will be re-rendered
                                   ;; the following code is only for ui responsiveness
                                   (setf (settings--sink-volume sink)
                                         volume)
                                   (when (and (> settings-refresh-rate
                                                 0.2)
                                              (get-buffer-window settings-buffer))

                                     ;; Due to the introduce of `yes-or-no-p', there is an interval
                                     ;;    between this `set-volume' lambda got called and volume
                                     ;;    setting logic executed.
                                     ;; Thus, the widget we originally clicked on may already be gone
                                     ;;    due to refreshing.
                                     ;; Therefore, we redraw the whole volume widget instead of
                                     ;;    the individual slider widget.
                                     (widget-default-value-set volume-widget sinks))))))
                 (volume-slider
                  (widget-create-child-and-convert
                   volume-widget
                   'settings-widget-volume-slider
                   :muted? (settings--sink-muted? sink)
                   :percentage (settings--sink-volume sink)
                   :length 20
                   :toggle (lambda (&rest _)
                             (settings--toggle-volume sink)
                             (when (> settings-refresh-rate
                                      0.2)
                               (setf (settings--sink-muted? sink)
                                     (not (settings--sink-muted? sink)))
                               (widget-put volume-slider
                                           :muted? (settings--sink-muted? sink))
                               (widget-default-value-set volume-slider nil)))
                   :incr-volume (lambda (&rest _)
                                  (funcall set-volume
                                           sink
                                           (+ (settings--sink-volume sink)
                                              settings-volume-step)
                                           t))
                   :dec-volume (lambda (&rest _)
                                 (funcall set-volume
                                          sink
                                          (- (settings--sink-volume sink)
                                             settings-volume-step)
                                          t))
                   :volume-bar-onclick (lambda (_ volume)
                                         (funcall set-volume
                                                  sink
                                                  volume
                                                  nil))))))
        (widget-create-child-and-convert
         volume-widget
         'item
         "")))))

(defvar-local settings--refresh-timer nil)
(defvar-local settings--refresh-tick 0)
(defvar-local settings--refresh-succeed-tick -1)
(defvar-local settings--updates nil)

(defun settings--refresh (settings-buffer refresh-tick)
  (with-current-buffer settings-buffer
    ;; We need to wait until all the data fetching is finished, then call all the callbacks at once
    ;; to prevent flicking
    (let* ((updates settings--updates)
           (data-vector (make-vector (length updates) nil))
           (cnt 0)
           (call-updates (lambda ()
                           (cl-flet ((do-update ()
                                       (with-current-buffer settings-buffer
                                         (cl-loop for update in updates
                                                  for data across data-vector
                                                  do
                                                  (progn
                                                    (setq-local settings--refresh-succeed-tick refresh-tick)
                                                    (funcall (plist-get update :callback)
                                                             data))))))
                             (if (and (get-buffer-window settings-buffer)
                                      (window-frame (get-buffer-window settings-buffer)))
                                 (with-selected-frame (window-frame (get-buffer-window settings-buffer))
                                   (with-selected-window (get-buffer-window settings-buffer)
                                     (do-update)))
                               (do-update))))))
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
                            (when (> refresh-tick
                                     settings--refresh-succeed-tick)
                              (setf (aref data-vector index)
                                    data)
                              (setq cnt (+ cnt 1))
                              (when (= cnt (length updates))
                                (funcall call-updates))))))))))

(cl-defstruct settings--section
  widget
  widget-header
  value-fetch
  avaiable-check)

(defun settings--register-update (data-fetch callback)
  (add-to-list 'settings--updates (list :data-fetch data-fetch
                                         :callback callback)))

(defun settings--init (settings-buffer)
  (with-current-buffer settings-buffer
    (let ((sections (list (make-settings--section :widget 'settings--widget-monitor
                                                   :widget-header "Monitors:\n"
                                                   :value-fetch #'settings--list-monitors
                                                   :avaiable-check #'settings--monitor-widget-available-check)
                          (make-settings--section :widget 'settings--widget-volume
                                                   :widget-header "Audio Volume: \n"
                                                   :value-fetch #'settings--fetch-sinks-data
                                                   :avaiable-check #'settings--volume-widget-available-check)))
          (current-point (point)))
      (let ((inhibit-read-only t))
        (widget-insert (propertize "Control Panel\n\n" 'font-lock-face 'bold))

        (magit-insert-section (magit-section "Root Section")
          (dolist (section sections)
            (let ((check (funcall (settings--section-avaiable-check section))))
              (if (equal check t)
                  (magit-insert-section (magit-section (settings--section-widget-header section))

                    (magit-insert-heading (insert (propertize (settings--section-widget-header section)
                                                              'font-lock-face 'bold)))
                    (magit-insert-section-body
                      (insert "")
                      (let ((section-widget (widget-create (settings--section-widget section)
                                                           :value
                                                           nil)))
                        (settings--register-update (settings--section-value-fetch section)
                                                    (lambda (data)
                                                      (save-excursion
                                                        (goto-char (widget-get section-widget :from))
                                                        (magit-section-maybe-update-visibility-indicator
                                                         (magit-current-section)))
                                                      (widget-default-value-set section-widget
                                                                                data)))
                        section-widget)
                      ;; an extra "\n" seems to make magit-section work with async value set
                      (insert "\n")))
                (message (concat "Warning: "
                                 (string-trim-right (settings--section-widget-header section))
                                 " section is hidden, because "
                                 (plist-get check :cause)))))))
        (goto-char current-point)))))

;; settings-mode code and M-x entry point

(defvar-local settings-mode nil)

(defvar settings-mode-map (make-composed-keymap
                            (define-keymap :parent widget-keymap)
                            magit-section-mode-map))

(define-keymap :keymap settings-mode-map
  "TAB" #'magit-section-toggle
  "<tab>" #'magit-section-toggle
  "C-t" #'magit-section-toggle)

(define-derived-mode settings-mode magit-section-mode "settings" ()
  (setq-local settings-mode 1)
  (read-only-mode)
  (use-local-map settings-mode-map)
  (letrec ((settings-buffer (current-buffer))
           (refresh-timer
            (run-with-timer 0 settings-refresh-rate
                            (lambda ()
                              (if (buffer-live-p settings-buffer)
                                  (progn (settings--refresh settings-buffer settings--refresh-tick)
                                         (setq-local settings--refresh-tick
                                                     (+ 1 settings--refresh-tick)))
                                (cancel-timer refresh-timer))))))
    (setq-local settings--refresh-timer
                refresh-timer)))

(defun settings ()
  "Launch the system settings management UI."
  (interactive)
  (let ((settings-buffer (get-buffer-create "*Settings*")))
    (pop-to-buffer settings-buffer)
    (with-current-buffer settings-buffer
      (unless settings-mode
        (setq-local settings-mode 1)
        (settings-mode)
        (settings--init settings-buffer)))))

(provide 'settings)

;;; settings.el ends here
