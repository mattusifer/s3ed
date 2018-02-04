;;; s3ed-mode.el --- Behavior for s3ed file and dired buffers

;; Author: Matt Usifer <mattusifer@gmail.com>

;; S3ed is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; S3ed is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements the `s3ed-mode' minor mode, as well as
;; behavior for s3ed file and dired buffers.

;;; Code:

(require 'dash)
(require 'dired)

(require 's3ed-util)
(require 's3ed-io)

;; s3ed mode hooks
(defvar s3ed-mode-hook nil)
(defvar s3ed-mode-map (make-sparse-keymap))

(defun s3ed-is-active ()
  "Check whether s3ed is active."
  (and s3ed-mode (s3ed-string-starts-with default-directory s3ed-tmp-s3-dir)))

(defun s3ed-after-save-hook ()
  "Push change to s3 after save."
  (when (s3ed-is-active)
    (s3ed-s3-cp (buffer-file-name) (s3ed-buffer-s3-path))))
(add-hook 'after-save-hook 's3ed-after-save-hook)

(defun s3ed-before-revert-hook ()
  "Pull change before revert."
  (when (s3ed-is-active)
    (s3ed-s3-cp (s3ed-buffer-s3-path) (buffer-file-name))))
(add-hook 'before-revert-hook 's3ed-before-revert-hook)

;;; dired functions
(defun s3ed-dired-do-s3-delete (orig-dired-do-delete &rest args)
  "A wrapper around dired's ‘dired-do-delete’ function.
The original function and arguments are available as ORIG-DIRED-DO-DELETE and ARGS."
  (if (s3ed-is-active)
    (let* ((current-local-file (dired-get-filename))
           (current-s3-file (s3ed-local-path-to-s3-path current-local-file)))
      (when  (y-or-n-p (format "Delete %s? " (s3ed-local-path-to-s3-path
                                             (dired-get-filename))))
        (if (and (s3ed-is-directory current-local-file)
                 (y-or-n-p (format "Recursively delete %s? " current-s3-file)))
          (s3ed-s3-rm current-s3-file t t)
          (s3ed-s3-rm current-s3-file nil t))))
    (apply orig-dired-do-delete args)))
(advice-add 'dired-do-delete :around #'s3ed-dired-do-s3-delete)

(defun s3ed-dired-do-s3-flagged-delete (orig-dired-do-flagged-delete &rest args)
  "A wrapper around dired's ‘dired-do-flagged-delete’ function.
The original function and arguments are available as ORIG-DIRED-DO-FLAGGED-DELETE and ARGS."
  (if (s3ed-is-active)
      (let* ((current-local-files (dired-get-marked-files)))
        (when (y-or-n-p (format "Delete %s marked files? " (length current-local-files)))
          (dolist (current-local-file current-local-files)
            (let ((current-s3-file (s3ed-local-path-to-s3-path current-local-file)))
              (when (or (not (s3ed-is-directory current-local-file))
                      (and (s3ed-is-directory current-local-file)
                           (y-or-n-p (format "Recursively delete %s? " current-s3-file))))
                (s3ed-s3-rm current-s3-file (s3ed-is-directory current-local-file) t))))))
    (apply orig-dired-do-flagged-delete args)))
(advice-add 'dired-do-flagged-delete :around #'s3ed-dired-do-s3-flagged-delete)

(defun s3ed-dired-do-s3-refresh (orig-dired-do-refresh &rest args)
  "A wrapper around dired's dired-do-refresh function.
The original function and arguments are available as ORIG-DIRED-DO-REFRESH and ARGS."
  (when (and (s3ed-is-active) (s3ed-is-dired-active))
    (s3ed-refresh-tmp-dir))
  (apply orig-dired-do-refresh args))
(advice-add 'revert-buffer :around #'s3ed-dired-do-s3-refresh)

(defun s3ed-dired-do-s3-rename (orig-dired-do-rename &rest args)
  "A wrapper around dired's ‘dired-do-rename’ function.
The original function and arguments are available as ORIG-DIRED-DO-RENAME and ARGS."
  (if (s3ed-is-active)
      (let* ((current-local-file (dired-get-filename))
             (current-s3-file (s3ed-local-path-to-s3-path current-local-file))
             (current-s3-parent-dir (s3ed-parent-directory current-s3-file)))
        (-when-let* ((dest-s3-file (s3ed-completing-read current-s3-parent-dir
                                                            (format "Rename %s to"
                                                                    current-s3-file))))
          (s3ed-s3-mv current-s3-file dest-s3-file
                         (s3ed-is-directory current-local-file) t)
          (s3ed-refresh-tmp-dir)))
    (apply orig-dired-do-rename args)))
(advice-add 'dired-do-rename :around #'s3ed-dired-do-s3-rename)

(defun s3ed-dired-do-s3-copy (orig-dired-do-copy &rest args)
  "A wrapper around dired's ‘dired-do-copy’ function.
The original function and arguments are available as ORIG-DIRED-DO-COPY and ARGS."
  (if (s3ed-is-active)
      (let* ((current-local-file (dired-get-filename))
             (current-s3-file (s3ed-local-path-to-s3-path current-local-file))
             (dest-file (s3ed-completing-read "" (format "Copy %s to" current-s3-file))))
        (if (s3ed-string-starts-with dest-file s3ed-s3-uri-scheme)
            (progn
              (s3ed-s3-cp current-s3-file dest-file
                             (s3ed-is-directory current-local-file) t)
              (s3ed-refresh-tmp-dir))
          (progn
            (s3ed-s3-cp current-s3-file current-local-file
                           (s3ed-is-directory current-local-file))
            (if (s3ed-is-directory current-local-file)
                (progn
                  (copy-directory current-local-file dest-file)
                  (dired dest-file))
              (progn
                (copy-file current-local-file dest-file)
                (dired (s3ed-parent-directory dest-file)))))))
    (apply orig-dired-do-copy args)))
(advice-add 'dired-do-copy :around #'s3ed-dired-do-s3-copy)

(defun s3ed-dired-find-s3-file (orig-dired-find-file &rest args)
  "A wrapper around dired's ‘dired-find-file’ function.
The original function and arguments are available as ORIG-DIRED-FIND-FILE and ARGS."
  (if (s3ed-is-active)
      (let* ((current-local-file (dired-get-filename)))
        (if (s3ed-is-directory current-local-file)
            (progn
              (s3ed-refresh-tmp-dir current-local-file)
              (apply orig-dired-find-file args))
          (progn
            (s3ed-s3-cp (s3ed-local-path-to-s3-path current-local-file) current-local-file)
            (apply orig-dired-find-file args))))
    (apply orig-dired-find-file args)))
(advice-add 'dired-find-file :around #'s3ed-dired-find-s3-file)

(defun s3ed-dired-do-shell-command (orig-dired-do-shell-command &rest args)
  "A wrapper around dired's ‘dired-do-shell-command’ function.
The original function and arguments are available as ORIG-DIRED-DO-SHELL-COMMAND and ARGS."
  (if (s3ed-is-active)
      (let* ((current-local-files (if (dired-get-marked-files) (dired-get-marked-files)
                                    `(,(dired-get-filename))))
             (current-s3-files (--map (s3ed-local-path-to-s3-path it) current-local-files)))
        (if (seq-contains s3ed-streamable-commands (car (split-string (car args))))
            (if (-any? 's3ed-is-directory current-local-files)
                (message (format "%s: This command cannot be applied to an s3 directory" s3ed-app-name))
              (s3ed-s3-cp-streams current-s3-files (car args)))
          (message (format "%s: This command is not supported by %s" s3ed-app-name s3ed-app-name))))
    (apply orig-dired-do-shell-command args)))
(advice-add 'dired-do-shell-command :around #'s3ed-dired-do-shell-command)

(defun s3ed-dired-do-async-shell-command (orig-dired-do-async-shell-command &rest args)
  "A wrapper around dired's ‘dired-do-async-shell-command’ function.
The original function and arguments are available as ORIG-DIRED-DO-ASYNC-SHELL-COMMAND and ARGS."
  (if (s3ed-is-active)
      (let* ((current-local-files (if (dired-get-marked-files) (dired-get-marked-files)
                                    `(,(dired-get-filename))))
             (current-s3-files (--map (s3ed-local-path-to-s3-path it) current-local-files)))
        (if (seq-contains s3ed-streamable-commands (car (split-string (car args))))
            (if (-any? 's3ed-is-directory current-local-files)
                (message (format "%s: This command cannot be applied to an s3 directory" s3ed-app-name))
              (s3ed-s3-cp-streams current-s3-files (car args) t))
          (message (format "%s: This command is not supported by %s" s3ed-app-name s3ed-app-name))))
    (apply orig-dired-do-async-shell-command args)))
(advice-add 'dired-do-async-shell-command :around #'s3ed-dired-do-async-shell-command)

(define-minor-mode s3ed-mode
  "Minor mode for s3ed"
  :lighter " s3ed"
  :keymap s3ed-mode-map
  :global t)

(provide 's3ed-mode)

;;; s3ed-mode.el ends here
