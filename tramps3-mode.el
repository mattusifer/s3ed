(require 'tramps3-constants)
(require 'tramps3-util)
(require 'tramps3-io)

;; tramps3 mode hooks
(defvar tramps3-mode-hook nil)
(defvar tramps3-mode-map (make-sparse-keymap))

(defun is-tramps3-mode-active ()
  (and (member 'tramps3-mode minor-mode-list) (symbolp 'tramps3-mode)
       (symbol-value 'tramps3-mode)))

(defun tramps3-after-save-hook ()
  "Push changes to s3 after save"
  (when (is-tramps3-mode-active)
    (tramps3-s3-cp (buffer-file-name) (tramps3-buffer-s3-path))))
(add-hook 'after-save-hook 'tramps3-after-save-hook)

(defun tramps3-before-revert-hook ()
  "Pull changes before revert"
  (when (is-tramps3-mode-active)
    (tramps3-s3-cp (tramps3-buffer-s3-path) (buffer-file-name))))
(add-hook 'before-revert-hook 'tramps3-before-revert-hook)

(define-minor-mode tramps3-mode
  "Minor mode for tramps3"
  :lighter " tramps3"
  :keymap tramps3-mode-map)

(provide 'tramps3-mode)
