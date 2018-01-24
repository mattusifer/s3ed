;;; tramps3-mode.el --- Behavior for tramps3 file and dired buffers

;; Copyright (C) 2018 Matt Usifer

;; Author: Matt Usifer <mattusifer@gmail.com>

;; Tramps3 is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Tramps3 is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements the `tramps3-mode' minor mode, as well as
;; behavior for tramps3 file and dired buffers.

;;; Code:

(require 'dash)
(require 'dired)

(require 'tramps3-util)
(require 'tramps3-io)

;; tramps3 mode hooks
(defvar tramps3-mode-hook nil)
(defvar tramps3-mode-map (make-sparse-keymap))

(defun tramps3-is-active ()
  "Check whether tramps3 is active."
  (and tramps3-mode (tramps3-string-starts-with default-directory tramps3-tmp-s3-dir)))

(defun tramps3-after-save-hook ()
  "Push change to s3 after save."
  (when (tramps3-is-active)
    (tramps3-s3-cp (buffer-file-name) (tramps3-buffer-s3-path))))
(add-hook 'after-save-hook 'tramps3-after-save-hook)

(defun tramps3-before-revert-hook ()
  "Pull change before revert."
  (when (tramps3-is-active)
    (tramps3-s3-cp (tramps3-buffer-s3-path) (buffer-file-name))))
(add-hook 'before-revert-hook 'tramps3-before-revert-hook)

;;; dired functions
(defun tramps3-dired-do-s3-delete (orig-dired-do-delete &rest args)
  "A wrapper around dired's ‘dired-do-delete’ function.
The original function and arguments are available as ORIG-DIRED-DO-DELETE and ARGS."
  (if (tramps3-is-active)
    (let* ((current-local-file (dired-get-filename))
           (current-s3-file (tramps3-local-path-to-s3-path current-local-file)))
      (when  (y-or-n-p (format "Delete %s? " (tramps3-local-path-to-s3-path
                                             (dired-get-filename))))
        (if (and (tramps3-is-directory current-local-file)
                 (y-or-n-p (format "Recursively delete %s? " current-s3-file)))
            (progn
              (delete-directory current-local-file t)
              (tramps3-s3-rm current-s3-file t t))
          (progn
            (delete-file current-local-file)
            (tramps3-s3-rm current-s3-file nil t)))))
    (apply orig-dired-do-delete args)))
(advice-add 'dired-do-delete :around #'tramps3-dired-do-s3-delete)

(defun tramps3-dired-do-s3-flagged-delete (orig-dired-do-flagged-delete &rest args)
  "A wrapper around dired's ‘dired-do-flagged-delete’ function.
The original function and arguments are available as ORIG-DIRED-DO-FLAGGED-DELETE and ARGS."
  (if (tramps3-is-active)
      (let* ((current-local-files (dired-get-marked-files)))
        (when (y-or-n-p (format "Delete %s marked files? " (length current-local-files)))
          (dolist (current-local-file current-local-files)
            (let ((current-s3-file (tramps3-local-path-to-s3-path current-local-file)))
              (if (and (tramps3-is-directory current-local-file)
                       (y-or-n-p (format "Recursively delete %s? " current-s3-file)))
                  (delete-directory current-local-file t)
                (delete-file current-local-file))
              (tramps3-s3-rm current-s3-file (tramps3-is-directory current-local-file) t)))))
    (apply orig-dired-do-flagged-delete args)))
(advice-add 'dired-do-flagged-delete :around #'tramps3-dired-do-s3-flagged-delete)

(defun tramps3-dired-do-s3-refresh (orig-dired-do-refresh &rest args)
  "A wrapper around dired's dired-do-refresh function.
The original function and arguments are available as ORIG-DIRED-DO-REFRESH and ARGS."
  (if (and (tramps3-is-active) (tramps3-is-dired-active))
      (tramps3-refresh-directory))
  (apply orig-dired-do-refresh args))
(advice-add 'revert-buffer :around #'tramps3-dired-do-s3-refresh)

(defun tramps3-dired-do-s3-rename (orig-dired-do-rename &rest args)
  "A wrapper around dired's ‘dired-do-rename’ function.
The original function and arguments are available as ORIG-DIRED-DO-RENAME and ARGS."
  (if (tramps3-is-active)
      (let* ((current-local-file (dired-get-filename))
             (current-s3-file (tramps3-local-path-to-s3-path current-local-file))
             (current-s3-parent-dir (tramps3-parent-directory current-s3-file)))
        (-when-let* ((dest-s3-file (tramps3-completing-read current-s3-parent-dir
                                                            (format "Rename %s to"
                                                                    current-s3-file)))
                     (dest-local-file (tramps3-s3-path-to-local-path dest-s3-file)))
          (rename-file current-local-file dest-local-file t)
          (tramps3-s3-mv current-s3-file dest-s3-file
                         (tramps3-is-directory current-local-file) t)))
    (apply orig-dired-do-rename args)))
(advice-add 'dired-do-rename :around #'tramps3-dired-do-s3-rename)

(defun tramps3-dired-do-s3-copy (orig-dired-do-copy &rest args)
  "A wrapper around dired's ‘dired-do-copy’ function.
The original function and arguments are available as ORIG-DIRED-DO-COPY and ARGS."
  (if (tramps3-is-active)
      (let* ((current-local-file (dired-get-filename))
             (current-s3-file (tramps3-local-path-to-s3-path current-local-file))
             (dest-file (tramps3-completing-read "" (format "Copy %s to" current-s3-file))))
        (if (tramps3-string-starts-with dest-file tramps3-s3-uri-scheme)
            (let ((dest-local-file (tramps3-s3-path-to-local-path dest-file)))
              (make-directory (tramps3-parent-directory dest-local-file) t)
              (if (tramps3-is-directory current-s3-file)
                  (copy-directory current-local-file dest-local-file)
                (copy-file current-local-file dest-local-file))
              (tramps3-s3-cp current-s3-file dest-file
                             (tramps3-is-directory current-local-file) t))
          (progn
            (tramps3-s3-cp current-s3-file current-local-file
                           (tramps3-is-directory current-local-file))
            (if (tramps3-is-directory current-local-file)
                (progn
                  (copy-directory current-local-file dest-file)
                  (dired dest-file))
              (progn
                (copy-file current-local-file dest-file)
                (dired (tramps3-parent-directory dest-file)))))))
    (apply orig-dired-do-copy args)))
(advice-add 'dired-do-copy :around #'tramps3-dired-do-s3-copy)

(defun tramps3-dired-find-s3-file (orig-dired-find-file &rest args)
  "A wrapper around dired's ‘dired-find-file’ function.
The original function and arguments are available as ORIG-DIRED-FIND-FILE and ARGS."
  (if (tramps3-is-active)
      (let* ((current-local-file (dired-get-filename)))
        (if (tramps3-is-directory current-local-file)
            (progn
              (apply orig-dired-find-file args)
              (revert-buffer t t))
          (progn
            (tramps3-s3-cp (tramps3-local-path-to-s3-path current-local-file) current-local-file)
            (apply orig-dired-find-file args))))
    (apply orig-dired-find-file args)))
(advice-add 'dired-find-file :around #'tramps3-dired-find-s3-file)

(defun tramps3-dired-do-shell-command (orig-dired-do-shell-command &rest args)
  "A wrapper around dired's ‘dired-do-shell-command’ function.
The original function and arguments are available as ORIG-DIRED-FIND-FILE and ARGS."
  (if (tramps3-is-active)
      (let* ((current-local-file (dired-get-filename))
             (current-s3-file (tramps3-local-path-to-s3-path current-local-file)))
        (if (seq-contains tramps3-pipeable-commands (car (split-string (car args))))
            (if (tramps3-is-directory current-local-file)
                (message (format "%s: This command cannot be applied to an s3 directory" tramps3-app-name))
              (tramps3-s3-cp-pipe current-s3-file (car args)))
          (message (format "%s: This command is not supported by %s" tramps3-app-name tramps3-app-name))))
    (apply orig-dired-do-shell-command args)))
(advice-add 'dired-do-shell-command :around #'tramps3-dired-do-shell-command)

(defun tramps3-dired-do-async-shell-command (orig-dired-do-async-shell-command &rest args)
  "A wrapper around dired's ‘dired-do-async-shell-command’ function.
The original function and arguments are available as ORIG-DIRED-FIND-FILE and ARGS."
  (if (tramps3-is-active)
      (let* ((current-local-file (dired-get-filename))
             (current-s3-file (tramps3-local-path-to-s3-path current-local-file)))
        (if (seq-contains tramps3-pipeable-commands (car (split-string (car args))))
            (if (tramps3-is-directory current-local-file)
                (message (format "%s: This command cannot be applied to an s3 directory" tramps3-app-name))
              (tramps3-s3-cp-pipe current-s3-file (car args) t))
          (message (format "%s: This command is not supported by %s" tramps3-app-name tramps3-app-name))))
    (apply orig-dired-do-async-shell-command args)))
(advice-add 'dired-do-async-shell-command :around #'tramps3-dired-do-async-shell-command)

(define-minor-mode tramps3-mode
  "Minor mode for tramps3"
  :lighter " tramps3"
  :keymap tramps3-mode-map
  :global t)

(provide 'tramps3-mode)

;;; tramps3-mode.el ends here
