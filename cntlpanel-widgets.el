;;; cntlpanel-widgets.el --- Widgets for cntlpanel.el -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require 'wid-edit)

(defvar cntlpanel-audio-muted-icon "🔇")
(defvar cntlpanel-audio-icon "📢")

(defun cntlpanel--widget-slider-build-bar (percentage bar-length)
  (let ((slider-on-char ?█)
        (slider-of-char ?_)
        (on-chars (ceiling (* bar-length percentage))))
    (concat (make-string on-chars slider-on-char)
            (make-string (- bar-length on-chars)
                         slider-of-char))))

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
PERCENTAGE: percentage of slider bar filled, in decimal.
LENGTH: length in characters."
  :convert-widget #'widget-types-convert-widget
  :copy #'widget-types-copy
  :format "%v"
  :value-create #'cntlpanel--widget-slider-value-create)

(defun cntlpanel-widget--volumne-widget-value-create (widget)
  (let ((percentage (widget-get widget :percentage))
        (bar-length (widget-get widget :length))
        (muted? (widget-get widget :muted?)))

    (let ((widget-push-button-prefix "")
          (widget-push-button-suffix ""))
      (widget-create-child-and-convert widget
                                       'push-button
                                       :notify (lambda (&rest _))
                                       (if muted?
                                           cntlpanel-audio-muted-icon
                                         cntlpanel-audio-icon)))
    (widget-create-child-and-convert widget
                                     'item
                                     :format " "
                                     :value " ")
    (widget-create-child-and-convert widget
                                     'push-button
                                     :notify (lambda (&rest _))
                                     "-")
    (widget-create-child-and-convert widget
                                     'cntlpanel-widget-slider
                                     :percentage percentage
                                     :length bar-length)
    (widget-create-child-and-convert widget
                                     'push-button
                                     :notify (lambda (&rest _))
                                     "+")))

(define-widget 'cntlpanel-widget-volume-slider 'default
  "A custom slider widget.

Parameters:
PERCENTAGE: percentage of slider bar filled, in decimal.
LENGTH: length in characters.
MUTED: muted?"
  :convert-widget #'widget-types-convert-widget
  :copy #'widget-types-copy
  :format "%v"
  :value-create #'cntlpanel-widget--volumne-widget-value-create)

(provide 'cntlpanel-widgets)
;;; cntlpanel-widgets.el ends here
