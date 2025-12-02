;;; cntlpanel-widgets.el --- Widgets for cntlpanel.el -*- lexical-binding: t -*-

;; SDPX-License-Identifier:  GPL-3.0-only
;; SPDX-FileCopyrightText: (c) 2025 Hufoe <foss@hufoe.com>
;; SPDX-FileCopyrightText: (c) 2025 Antero Mejr <mail@antr.me>
;; SPDX-FileContributor: 2025 Zephyr Yang <me@coruscation.net>

;;; Commentary:

;;; Code:

(require 'wid-edit)

(defvar cntlpanel-audio-muted-icon "🔇")
(defvar cntlpanel-audio-icon "📢")

(defvar cntlpanel--progress-bar-block '(" " "▏" "▎" "▍" "▌" "▋" "▊" "▉" "█"))

(defun cntlpanel--widget-slider-get-block (decimal)
  (let ((step (/ 1 (float (length cntlpanel--progress-bar-block)))))
    (cl-loop
     for block in cntlpanel--progress-bar-block
     for base = 0 then (+ base step)
     if (< (abs (- base decimal)) step) return block)))

(defun cntlpanel--widget-slider-build-bar (percentage bar-length)
  (let ((slider-on-char ?█)
        (slider-off-char ? )
        (chars-left (floor (* bar-length percentage)))
        (chars-right (floor (* bar-length (- 1 percentage)))))

    (concat
     (make-string chars-left slider-on-char)
     (if (= bar-length (+ chars-left chars-right))
         ""
       (cntlpanel--widget-slider-get-block (* (- percentage
                                                 (/ (float chars-left)
                                                    bar-length))
                                              bar-length)))
     (make-string chars-right
                  slider-off-char))))

(defun cntlpanel--widget-slider-value-create (widget)
  ""
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
                          (propertize (cntlpanel--widget-slider-build-bar percentage bar-length)
                                      'font-lock-face
                                      'widget-field)))))))

(define-widget 'cntlpanel-widget-slider 'default
  "A custom slider widget.

Parameters:
:percentage percentage of slider bar filled, in decimal.
:length length in characters.
:onclick a callback lambda takes two arguments:
    1. widget itself  2. percentage at the clicking point in decimal"
  :convert-widget #'widget-types-convert-widget
  :copy #'widget-types-copy
  :format "%v"
  :value-create #'cntlpanel--widget-slider-value-create)

(defun cntlpanel-widget--volumne-widget-value-create (widget)
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
                                           cntlpanel-audio-muted-icon
                                         cntlpanel-audio-icon)))
    (widget-create-child-and-convert widget
                                     'item
                                     :format " "
                                     :value " ")
    (widget-create-child-and-convert widget
                                     'push-button
                                     :notify dec-volume-callback
                                     "-")
    (widget-create-child-and-convert widget
                                     'cntlpanel-widget-slider
                                     :percentage percentage
                                     :length bar-length
                                     :onclick volume-bar-onclick)
    (widget-create-child-and-convert widget
                                     'push-button
                                     :notify incr-volume-callback
                                     "+")))

(define-widget 'cntlpanel-widget-volume-slider 'default
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
  :value-create #'cntlpanel-widget--volumne-widget-value-create)

(provide 'cntlpanel-widgets)
;;; cntlpanel-widgets.el ends here
