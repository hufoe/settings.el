;;; cntlpanel-widgets.el --- Widgets for cntlpanel.el -*- lexical-binding: t -*-
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
         (bar-length (widget-get widget :length)))
    (widget-create-child-and-convert
     widget 'item
     :format "%v"
     :value (cntlpanel--widget-slider-build-bar percentage bar-length))))

(define-widget 'cntlpanel-widget-slider 'default
  "A custom slider widget.

Parameters:
:PERCENTAGE: percentage of slider bar filled, in decimal.
:LENGTH: length in characters."
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
        (dec-volume-callback (widget-get widget :dec-volume)))

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
                                     :length bar-length)
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
:dec-volume callback for volume decreasing"
  :convert-widget #'widget-types-convert-widget
  :copy #'widget-types-copy
  :format "%v"
  :value-create #'cntlpanel-widget--volumne-widget-value-create)

(provide 'cntlpanel-widgets)
;;; cntlpanel-widgets.el ends here
