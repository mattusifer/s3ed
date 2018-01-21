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

(defun is-tramps3-mode-active ()
  "Check whether tramps3 is an active minor mode."
  (and (member 'tramps3-mode minor-mode-list) (symbolp 'tramps3-mode)
       (symbol-value 'tramps3-mode)))

(defun tramps3-after-save-hook ()
  "Push change to s3 after save."
  (when (is-tramps3-mode-active)
    (tramps3-s3-cp (buffer-file-name) (tramps3-buffer-s3-path))))
(add-hook 'after-save-hook 'tramps3-after-save-hook)

(defun tramps3-before-revert-hook ()
  "Pull change before revert."
  (when (is-tramps3-mode-active)
    (tramps3-s3-cp (tramps3-buffer-s3-path) (buffer-file-name))))
(add-hook 'before-revert-hook 'tramps3-before-revert-hook)

;;; dired functions
(defun tramps3-dired-do-s3-delete (orig-dired-do-delete &rest args)
  "A wrapper around dired's ‘dired-do-delete’ function.
The original function and arguments are available as ORIG-DIRED-DO-DELETE and ARGS."
  (if (is-tramps3-mode-active)
    (let* ((current-local-file (dired-get-filename))
           (current-s3-file (tramps3-local-path-to-s3-path current-local-file)))
      (when  (y-or-n-p (format "Delete %s? " (tramps3-local-path-to-s3-path
                                             (dired-get-filename))))
        (if (and (tramps3-is-directory current-local-file)
                 (y-or-n-p (format "Recursively delete %s? " current-s3-file)))
            (progn
              (delete-directory current-local-file t)
              (tramps3-s3-rm current-s3-file t))
          (progn
            (delete-file current-local-file)
            (tramps3-s3-rm current-s3-file)))
        (revert-buffer t t)))
    (apply orig-dired-do-delete args)))
(advice-add 'dired-do-delete :around #'tramps3-dired-do-s3-delete)

(defun tramps3-dired-do-s3-flagged-delete (orig-dired-do-flagged-delete &rest args)
  "A wrapper around dired's ‘dired-do-flagged-delete’ function.
The original function and arguments are available as ORIG-DIRED-DO-FLAGGED-DELETE and ARGS."
  (if (is-tramps3-mode-active)
      (let* ((current-local-files (dired-get-marked-files)))
        (when (y-or-n-p (format "Delete %s marked files? " (length current-local-files)))
          (dolist (current-local-file current-local-files)
            (let ((current-s3-file (tramps3-local-path-to-s3-path current-local-file)))
              (if (and (tramps3-is-directory current-local-file)
                       (y-or-n-p (format "Recursively delete %s? " current-s3-file)))
                  (delete-directory current-local-file t)
                (delete-file current-local-file))
              (tramps3-s3-rm current-s3-file (tramps3-is-directory current-local-file))
              (revert-buffer t t)))))
    (apply orig-dired-do-flagged-delete args)))
(advice-add 'dired-do-flagged-delete :around #'tramps3-dired-do-s3-flagged-delete)

(defun tramps3-dired-do-s3-refresh (orig-dired-do-refresh &rest args)
  "A wrapper around dired's dired-do-refresh function.
The original function and arguments are available as ORIG-DIRED-DO-REFRESH and ARGS."
  (if (and (is-tramps3-mode-active) (tramps3-is-dired-active))
      (tramps3-refresh-directory))
  (apply orig-dired-do-refresh args))
(advice-add 'revert-buffer :around #'tramps3-dired-do-s3-refresh)

(defun tramps3-dired-do-s3-rename (orig-dired-do-rename &rest args)
  "A wrapper around dired's ‘dired-do-rename’ function.
The original function and arguments are available as ORIG-DIRED-DO-RENAME and ARGS."
  (if (is-tramps3-mode-active)
      (let* ((current-local-file (dired-get-filename))
             (current-s3-file (tramps3-local-path-to-s3-path current-local-file))
             (current-s3-parent-dir (tramps3-parent-directory current-s3-file)))
        (-when-let* ((dest-s3-file (tramps3-completing-read current-s3-parent-dir
                                                            (format "Rename %s to"
                                                                    current-s3-file)))
                     (dest-local-file (tramps3-s3-path-to-local-path dest-s3-file)))
          (rename-file current-local-file dest-local-file t)
          (tramps3-s3-mv current-s3-file dest-s3-file (tramps3-is-directory
                                                       current-local-file))
          (revert-buffer t t)))
    (apply orig-dired-do-rename args)))
(advice-add 'dired-do-rename :around #'tramps3-dired-do-s3-rename)

(defun tramps3-dired-do-s3-copy (orig-dired-do-copy &rest args)
  "A wrapper around dired's ‘dired-do-copy’ function.
The original function and arguments are available as ORIG-DIRED-DO-COPY and ARGS."
  (if (is-tramps3-mode-active)
      (let* ((current-local-file (dired-get-filename))
             (current-s3-file (tramps3-local-path-to-s3-path current-local-file))
             (current-s3-parent-dir (tramps3-parent-directory current-s3-file)))
        (-when-let* ((dest-s3-file (tramps3-completing-read current-s3-parent-dir
                                                            (format "Copy %s to"
                                                                    current-s3-file)))
                     (dest-local-file (tramps3-s3-path-to-local-path dest-s3-file)))
          (if (tramps3-is-directory current-s3-file)
              (copy-directory current-local-file dest-local-file)
            (copy-file current-local-file dest-local-file))
          (tramps3-s3-cp current-s3-file dest-s3-file (tramps3-is-directory
                                                       current-local-file))
          (revert-buffer t t)))
    (apply orig-dired-do-copy args)))
(advice-add 'dired-do-copy :around #'tramps3-dired-do-s3-copy)

(defun tramps3-dired-find-s3-file (orig-dired-find-file &rest args)
  "A wrapper around dired's ‘dired-find-file’ function.
The original function and arguments are available as ORIG-DIRED-FIND-FILE and ARGS."
  (if (is-tramps3-mode-active)
      (let* ((current-local-file (dired-get-filename)))
        (if (tramps3-is-directory current-local-file)
            (progn
              (apply orig-dired-find-file args)
              (tramps3-mode)
              (revert-buffer t t))
          (progn
            (tramps3-s3-cp (tramps3-local-path-to-s3-path current-local-file) current-local-file)
            (apply orig-dired-find-file args)
            (tramps3-mode))))
    (apply orig-dired-find-file args)))
(advice-add 'dired-find-file :around #'tramps3-dired-find-s3-file)

(define-minor-mode tramps3-mode
  "Minor mode for tramps3"
  :lighter " tramps3"
  :keymap tramps3-mode-map)

(provide 'tramps3-mode)

;;; tramps3-mode.el ends here
