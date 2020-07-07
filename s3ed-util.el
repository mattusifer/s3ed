;;; s3ed-util.el --- Various utilities

;; Author: Matt Usifer <mattusifer@gmail.com>
;; Package-Requires: ((emacs "25.1") (dash "2.17.0") (s "1.12.0"))

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

;; This library provides several functions commonly used throughout
;; s3ed

;;; Code:

(require 's)
(require 'seq)

(defconst s3ed-s3-uri-scheme "s3://")

(defun s3ed-is-root-s3-path (path)
  "Check if PATH is at the root of s3."
  (equal path s3ed-s3-uri-scheme))

(defun s3ed-is-dired-active ()
  "True if we are in a dired buffer."
  (equal major-mode 'dired-mode))

(defun s3ed-shell-command-no-message (cmd &optional ret msg)
  "Run CMD, inhibiting messages from underlying process.
If 'RET' is not nil, results from CMD will be returned.
Default messages will be replaced with custom message 'MSG' if it is provided.
If the error code is greater than 0, an error will be raised."
  (when msg (message msg))
  (let* ((inhibit-message t)
         (program-name (car (s-split " " cmd)))
         (program-args (cdr (s-split " " cmd)))
         (program-result (with-temp-buffer
                           (list (apply #'call-process program-name nil
                                        (current-buffer) nil program-args)
                                 (buffer-string))))
         (program-exit-code (car program-result))
         (program-output (car (cdr program-result))))
    (if (> program-exit-code 0) (error (format "Error running command '%s': %s" cmd program-output))
      (when ret program-output))))

(defun s3ed-completing-read-backspace (cur-base)
  "If CUR-BASE is at the root, backspace acts normally.
Otherwise, backspace will go up one directory."
  (if (not (s3ed-is-root-s3-path cur-base))
      (condition-case nil
          ;; use normal backspace behavior if no error was found
        (backward-delete-char 1)

        ;; if an error was found, we are at the beginning of the
        ;; line. Recurse to the parent directory of the current path.
        (error (throw 'backspace nil)))

    ;; if we're at the base, allow errors, don't recurse
    (backward-delete-char 1)))

;; finding files
(defun s3ed-completing-read (base msg)
  "Use ‘completing-read’ to find files in s3 starting at BASE.
MSG will be displayed to the user at prompt."
  (if (s-prefix? s3ed-s3-uri-scheme base)
      (let* ((choices (seq-remove (lambda (el) (not el)) (s3ed-s3-ls base)))
             (choice (progn
                       (define-key minibuffer-local-map (kbd "<backspace>")
                         (lambda () (interactive) (s3ed-completing-read-backspace base)))
                       (catch 'backspace (completing-read (format "%s: %s" msg base) choices)))))

        ;; unbind backspace key
        (define-key minibuffer-local-map (kbd "<backspace>") nil)

        ;; no choice means a backspace was entered, recurse upwards
        (if (not choice)
            (s3ed-completing-read (concat (mapconcat 'identity (butlast (butlast (split-string base "/"))) "/") "/")
                                     msg)
          (if (seq-contains choices choice)
              (if (and (not (string-match "/\\'" choice)))
                  (concat base choice)
                (s3ed-completing-read (concat base choice) msg))
            (concat base choice))))
    (let* ((choice (completing-read (format "%s: %s" msg base) `(,s3ed-s3-uri-scheme "Elsewhere"))))
      (if (s3ed-is-root-s3-path choice)
          (s3ed-completing-read choice msg)
        (read-file-name (format "%s: " msg) "")))))

(provide 's3ed-util)

;;; s3ed-util.el ends here
