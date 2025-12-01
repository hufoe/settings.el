;;; cntlpanel-widgets.el --- A lsp-bridge Flymake backend -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require 'wid-edit)

(defun cntlpanel--widget-slider-build-bar (percentage length)
  (let ((slider-on-char ?█)
        (slider-of-char ?_)
        (on-chars (ceiling (* length percentage))))
    (concat (make-string on-chars slider-on-char)
            (make-string (- length on-chars)
                         slider-of-char))))

(defun cntlpanel-widget-slider-value-create (widget)
  ""
  (let* ((percentage (widget-get widget :percentage))
         (length (widget-get widget :length)))
    (widget-create-child-and-convert widget 'item :value (cntlpanel--widget-slider-build-bar percentage length))))

(define-widget 'cntlpanel-widget-slider 'default
  "A custom slider widget.

Parameters:
PERCENTAGE: percentage of slider bar filled, in decimal.
LENGTH: length in characters."
  :convert-widget #'widget-types-convert-widget
  :copy #'widget-types-copy
  :format "%v"
  :value-create #'cntlpanel-widget-slider-value-create)


(provide 'cntlpanel-widgets)
;;; cntlpanel-widgets.el ends here
