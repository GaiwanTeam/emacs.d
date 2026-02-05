;;; rainbow-vector-notation.el --- Colorize RGB vector notation in buffers -*- lexical-binding: nil -*-

;; Keywords: faces
;; Version: 1.0

;;; Commentary:
;;
;; This minor mode extends rainbow-mode to colorize RGB vector notation,
;; e.g. [55 86 163] is displayed with a blue background.
;;
;; Requires rainbow-mode to be loaded first.

;;; Code:

(require 'rainbow-mode)

;;; RGB Vector colors

(defvar rainbow-vector-colors-font-lock-keywords
  '(("\\[\\s-*\\([0-9]\\{1,3\\}\\)\\s-*\\([0-9]\\{1,3\\}\\)\\s-*\\([0-9]\\{1,3\\}\\)\\s-*\\]"
     (0 (rainbow-colorize-vector))))
  "Font-lock keywords to add for RGB vector colors.")

(defun rainbow-colorize-vector ()
  "Colorize an RGB vector match with itself."
  (let ((r (string-to-number (match-string-no-properties 1)))
        (g (string-to-number (match-string-no-properties 2)))
        (b (string-to-number (match-string-no-properties 3))))
    ;; Validate that all values are between 0 and 255
    (when (and (>= r 0) (<= r 255)
               (>= g 0) (<= g 255)
               (>= b 0) (<= b 255))
      (rainbow-colorize-match (format "#%02X%02X%02X" r g b)))))

;;; Mode

(defun rainbow-vector-notation-turn-on ()
  "Turn on rainbow-vector-notation-mode."
  (font-lock-add-keywords nil
                          rainbow-vector-colors-font-lock-keywords
                          t))

(defun rainbow-vector-notation-turn-off ()
  "Turn off rainbow-vector-notation-mode."
  (font-lock-remove-keywords
   nil
   rainbow-vector-colors-font-lock-keywords))

;;;###autoload
(define-minor-mode rainbow-vector-notation-mode
  "Colorize RGB vector notation like [55 86 163].
This extends rainbow-mode to recognize and colorize RGB vector notation."
  :lighter " Rvec"
  (if rainbow-vector-notation-mode
      (rainbow-vector-notation-turn-on)
    (rainbow-vector-notation-turn-off))
  ;; Call font-lock-mode to refresh the buffer
  (font-lock-mode 1))

(provide 'rainbow-vector-notation)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; End:
;;; rainbow-vector-notation.el ends here
