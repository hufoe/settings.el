;;; settings-widgets.el --- Widgets for settings.el -*- lexical-binding: t -*-

;; SDPX-License-Identifier:  GPL-3.0-only
;; SPDX-FileCopyrightText: (c) 2025 Hufoe <foss@hufoe.com>
;; SPDX-FileCopyrightText: (c) 2025 Antero Mejr <mail@antr.me>
;; SPDX-FileContributor: 2025 Zephyr Yang <me@coruscation.net>

;;; Commentary:

;;; Code:

(require 'wid-edit)

;; Customizable variables

(defvar settings-audio-muted-icon "🔇")
(defvar settings-audio-icon "📢")
(defvar settings--progress-bar-block '(" " "▏" "▎" "▍" "▌" "▋" "▊" "▉" "█"))

;; Generic widgets

(defun settings--widget-slider-get-block (decimal)
  (let ((step (/ 1 (float (length settings--progress-bar-block)))))
    (cl-loop
     for block in settings--progress-bar-block
     for base = 0 then (+ base step)
     if (< (abs (- base decimal)) step) return block)))

(defun settings--widget-slider-build-bar (percentage bar-length)
  (let ((slider-on-char ?█)
        (slider-off-char ? )
        (chars-left (floor (* bar-length percentage)))
        (chars-right (floor (* bar-length (- 1 percentage)))))

    (concat
     (make-string chars-left slider-on-char)
     (if (= bar-length (+ chars-left chars-right))
         ""
       (settings--widget-slider-get-block (* (- percentage
                                                 (/ (float chars-left)
                                                    bar-length))
                                              bar-length)))
     (make-string chars-right
                  slider-off-char))))

(defun settings--widget-slider-value-create (widget)
  (let* ((percentage (widget-get widget :percentage))
         (bar-length (widget-get widget :length))
         (onclick (widget-get widget :onclick)))
    (letrec ((widget-push-button-prefix "")
             (widget-push-button-suffix "")
             (bar-widget (widget-create-child-and-convert
                          widget
                          'push-button
                          :notify
                          (lambda (&rest _)
                            (when onclick
                              (funcall onclick
                                       widget
                                       (/ (float (+ 1
                                                    (- (point)
                                                       (widget-get bar-widget :from))))
                                          bar-length))))
                          :value
                          (propertize (settings--widget-slider-build-bar percentage bar-length)
                                      'font-lock-face
                                      'widget-field)))))))

(define-widget 'settings-widget-slider 'default
  "A custom slider widget.

Parameters:
:percentage percentage of slider bar filled, in decimal.
:length length in characters.
:onclick a callback lambda takes two arguments:
    1. widget itself  2. percentage at the clicking point in decimal"
  :convert-widget #'widget-types-convert-widget
  :copy #'widget-types-copy
  :format "%v"
  :value-create #'settings--widget-slider-value-create)

;; Audio widgets.

(defun settings-widget--volume-widget-value-create (widget)
  (let ((percentage (widget-get widget :percentage))
        (bar-length (widget-get widget :length))
        (muted? (widget-get widget :muted?))
        (toggle-callback (widget-get widget :toggle))
        (incr-volume-callback (widget-get widget :incr-volume))
        (dec-volume-callback (widget-get widget :dec-volume))
        (volume-bar-onclick (widget-get widget :volume-bar-onclick)))

    (let ((widget-push-button-prefix "")
          (widget-push-button-suffix ""))
      (widget-create-child-and-convert widget
                                       'push-button
                                       :notify toggle-callback
                                       (if muted?
                                           settings-audio-muted-icon
                                         settings-audio-icon)))
    (widget-create-child-and-convert widget
                                     'item
                                     :format " "
                                     :value " ")
    (widget-create-child-and-convert widget
                                     'push-button
                                     :notify dec-volume-callback
                                     "-")
    (widget-create-child-and-convert widget
                                     'settings-widget-slider
                                     :percentage percentage
                                     :length bar-length
                                     :onclick volume-bar-onclick)
    (widget-create-child-and-convert widget
                                     'push-button
                                     :notify incr-volume-callback
                                     "+")))

(define-widget 'settings-widget-volume-slider 'default
  "A custom slider widget.

Parameters:
:percentage percentage of slider bar filled, in decimal.
:length length in characters.
:muted? muted?
:toggle callback for toggle muted icon
:incr-volume callback for volume increasing
:dec-volume callback for volume decreasing
:volume-bar-onclick a callback lambda takes two arguments:
    1. widget itself  2. percentage at the clicking point in decimal"
  :convert-widget #'widget-types-convert-widget
  :copy #'widget-types-copy
  :format "%v"
  :value-create #'settings-widget--volume-widget-value-create)

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
                                   (setf (settings--sink-volume sink) volume)
                                   (when (and (> settings-refresh-rate 0.2)
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

(define-widget 'settings--widget-volume 'item
  ""
  :convert-widget #'widget-types-convert-widget
  :copy #'widget-types-copy
  :format "%v"
  :value-create #'settings--widget-volume-create)

;; Monitor-related widgets.

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
             :value "")))))))

(define-widget 'settings--widget-monitor 'item
  ""
  :convert-widget #'widget-types-convert-widget
  :copy #'widget-types-copy
  :format "%v"
  :value-create #'settings--widget-monitor-value-create)

;; About widget.

(defun settings--widget-about-value-create (about-widget)
  (widget-create-child-and-convert
         about-widget
         'item
         :format "%v"
         :value
         (format
          (concat "Emacs version: %s\n"
                  ;; TODO: Update each release
                  "settings.el version: 0.1.0\n"
                  "made by ")
          emacs-version))
  (insert-text-button
   "Hufoe"
   'action (lambda (_) (browse-url "https://hufoe.com"))
   'follow-link t
   'face 'link))

(define-widget 'settings--widget-about 'item
  ""
  :convert-widget #'widget-types-convert-widget
  :copy #'widget-types-copy
  :format "%v"
  :value-create #'settings--widget-about-value-create)

(provide 'settings-widgets)

;;; settings-widgets.el ends here
