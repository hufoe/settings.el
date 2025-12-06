;;; settings.el --- A simple Emacs GUI for managing system settings.  -*- lexical-binding: t -*-

;; Author: Hufoe <foss@hufoe.com>
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (magit-section "4.4.2"))
;; Keywords: hardware convenience
;; Homepage: https://hufoe.com/foss/settings-el

;; SDPX-License-Identifier:  GPL-3.0-only
;; SPDX-FileCopyrightText: (c) 2025 Hufoe <foss@hufoe.com>
;; SPDX-FileCopyrightText: (c) 2025 Antero Mejr <mail@antr.me>
;; SPDX-FileContributor: 2025 Zephyr Yang <me@coruscation.net>

;;; Commentary:

;; This package provides a simple Emacs UI for managing system settings.

;;; Code:
(require 'wid-edit)
(require 'widget)
(require 'subr-x)
(require 'magit-section)
(require 'settings-widgets)
(require 'settings-backend)

;; TODO: Remove callback code and async hacks, make synchronous
;; TODO: Add an easier-to-use interface on top of widgets

;; Custom variables

(defgroup settings nil "Settings group.")

(defvaralias 'settings-xrandr-command 'settings--xrandr-command)
(defvaralias 'settings-pactl-command 'settings--pactl-command)
(defvaralias 'settings-osascript-command 'settings--osascript-command)

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
           (call-updates
            (lambda ()
              (cl-flet ((do-update ()
                          (with-current-buffer settings-buffer
                            (cl-loop for update in updates
                                     for data across data-vector
                                     do
                                     (progn
                                       (setq-local settings--refresh-succeed-tick refresh-tick)
                                       (funcall (plist-get update :callback) data))))))
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
  available-check)

(defun settings--register-update (data-fetch callback)
  (add-to-list 'settings--updates (list :data-fetch data-fetch :callback callback)))

(defun settings--init (settings-buffer)
  (with-current-buffer settings-buffer
    (let ((sections (list (make-settings--section :widget 'settings--widget-monitor
                                                  :widget-header "Monitors\n"
                                                  :value-fetch #'settings--list-monitors
                                                  :available-check
                                                  #'settings--monitor-widget-available-check)
                          (make-settings--section :widget 'settings--widget-volume
                                                  :widget-header "Audio\n"
                                                  :value-fetch #'settings--fetch-sinks-data
                                                  :available-check
                                                  #'settings--volume-widget-available-check)
                          (make-settings--section :widget 'settings--widget-about
                                                  :widget-header "About\n"
                                                  :value-fetch
                                                  (lambda (cb)
                                                    (funcall cb t))
                                                  :available-check (lambda () t))))
          (current-point (point)))
      (let ((inhibit-read-only t))
        (widget-insert (propertize "Settings\n\n" 'font-lock-face 'bold))

        (magit-insert-section (magit-section "Root Section")
          (dolist (section sections)
            (let ((check (funcall (settings--section-available-check section))))
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
                      ;; an extra "\n" seems to make magit-section work with
                      ;; async value set
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
