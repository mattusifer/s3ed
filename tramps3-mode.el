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
    (s3-cp (buffer-file-name) (buffer-s3-path))))
(add-hook 'after-save-hook 'tramps3-after-save-hook)

(defun tramps3-before-revert-hook ()
  "Pull changes before revert"
  (when (is-tramps3-mode-active)
    (s3-cp (buffer-s3-path) (buffer-file-name))))
(add-hook 'before-revert-hook 'tramps3-before-revert-hook)

(define-key tramps3-mode-map (kbd "C-G") (lambda () (interactive) (tramps3-refresh-directory)))
(define-key tramps3-mode-map (kbd "C-<return>") (lambda () (interactive) (tramps3-open-file (dired-get-filename))))

(define-minor-mode tramps3-mode
  "Minor mode for tramps3"
  :lighter " tramps3"
  :keymap tramps3-mode-map)

(provide 'tramps3-mode)