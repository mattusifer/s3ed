(require 'tramps3-constants)

;; tramps3 mode hooks
(defvar tramps3-mode-hook nil)

(defun is-tramps3-mode-active ()
  (and (member 'tramps3-mode minor-mode-list) (symbolp 'tramps3-mode)
       (symbol-value 'tramps3-mode)))

(defun buffer-s3-path ()
  (replace-regexp-in-string TMP_S3_DIR "s3:/" (buffer-file-name)))

;; push changes to s3 after save
(defun tramps3-after-save-hook ()
  (when (is-tramps3-mode-active)
    (shell-command (format "aws s3 cp --sse AES256 %s %s" (buffer-file-name) (buffer-s3-path)))))
(add-hook 'after-save-hook 'tramps3-after-save-hook)

;; pull changes before revert
(defun tramps3-before-revert-hook ()
  (when (is-tramps3-mode-active)
    (shell-command (format "aws s3 cp --sse AES256 %s %s" (buffer-s3-path) (buffer-file-name)))))
(add-hook 'before-revert-hook 'tramps3-before-revert-hook)

(defvar tramps3-mode-map (make-sparse-keymap))

;; add to mode map

;; other mode things

(define-minor-mode tramps3-mode
  "Minor mode for tramps3"
  :lighter " tramps3"
  :keymap tramps3-mode-map
 )

(provide 'tramps3-mode)
