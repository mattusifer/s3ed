;;; tramps3-io.el --- I/O operations both locally and on s3

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

;; This library provides functions for interacting directly with files on
;; s3, as well as some functions for interactinve with local files.

;;; Code:

(require 'dash)

(require 'tramps3-util)

(defconst tramps3-app-name "tramps3")
(defconst tramps3-tmp-s3-dir (concat "/tmp/" tramps3-app-name))

(defconst tramps3-pipeable-commands
  '("head" "cat" "grep" "tail" "sed" "awk" "cut" "wc" "sort" "tr" "uniq"))

;; s3 functions

(defun tramps3-get-transfer-message (src dest async)
  "Get message to display when transferring data from SRC to DEST."
  (format "%s: Transferring data %s s3%s..." tramps3-app-name
          (if (tramps3-is-s3-path src) (if (tramps3-is-s3-path dest)
                                           "within" "from") "to")
          (if async " in the background" "")))

(defun tramps3-s3-ls (path)
  "List an s3 PATH."
  (cl-flet ((parse-s3-ls-raw-output (raw-line) (car (-take-last 1 (split-string it)))))
    (--remove (or (string= "" it) (not it))
              (--map (if (tramps3-is-root-s3-path path)
                         (concat (parse-s3-ls-raw-output it) "/")
                       (parse-s3-ls-raw-output it))
                     (split-string (tramps3-shell-command-no-message
                                    (format "aws s3 ls %s" path) t
                                    (format "%s: Listing files on s3..." tramps3-app-name)) "\n")))))

(defun tramps3-s3-cp (src dest &optional recursive async)
  "Copy s3 SRC file to DEST.  If specified, this will be a RECURSIVE operation."
  (let ((command (format "aws s3 cp %s --sse AES256 %s %s"
                         (if recursive "--recursive" "") src dest))
        (msg (tramps3-get-transfer-message src dest async)))
    (if async
        (progn
          (apply 'start-process "tramps3-cp" "*tramps3*" (split-string command))
          (message msg))
      (tramps3-shell-command-no-message command :msg msg))))

(defun tramps3-s3-cp-pipe (src command &optional async)
  "Copy s3 SRC file to DEST.  If specified, this will be a RECURSIVE operation."
  (let ((command (format "aws s3 cp %s - | %s" src command)))
    (if async
        (async-shell-command command)
        (shell-command command))))

(defun tramps3-s3-mv (src dest &optional recursive async)
  "Move s3 SRC file to DEST.  If specified, this will be a RECURSIVE operation."
  (let ((command (format "aws s3 mv %s --sse AES256 %s %s"
                         (if recursive "--recursive" "") src dest))
        (msg (tramps3-get-transfer-message src dest async)))
    (if async
        (progn
          (apply 'start-process "tramps3-mv" "*tramps3*" (split-string command))
          (message msg))
      (tramps3-shell-command-no-message command :msg msg))))

(defun tramps3-s3-rm (path &optional recursive async)
  "Remove file or directory PATH from s3. If specified, this will be a RECURSIVE operation."
  (let ((msg (format "%s: Removing data from s3%s..." tramps3-app-name
                     (if async " in the background" "")))
        (command (format "aws s3 rm %s %s" (if recursive "--recursive" "") path)))
    (if async
        (progn
          (apply 'start-process "tramps3-rm" "*tramps3*" (split-string command))
          (message msg))
        (tramps3-shell-command-no-message command :msg msg))))



;; local filesystem functions
(defun tramps3-file-exists (path)
  "Check that PATH exists."
  (condition-case nil
      (progn (tramps3-shell-command-no-message (format "file %s" path)) t)
    (error nil)))

(defun tramps3-mkdirs (paths)
  "Create directories at the given PATHS."
  (dolist (subgroup (-partition-all 100 paths))
    (tramps3-shell-command-no-message (format "mkdir -p %s" (mapconcat 'identity subgroup " "))
                                      :msg "tramps3: Creating local directories...")))

(defun tramps3-create-empty-file (filename)
  "Create FILENAME if it doesn't exist."
  (tramps3-shell-command-no-message (format "touch %s" filename)
                                    :msg "tramps3: Creating dummy file..."))

(defun tramps3-create-empty-files (filenames)
  "Create all files in FILENAMES if they don't exist."
  (dolist (subgroup (-partition-all 100 filenames))
    (tramps3-shell-command-no-message (format "touch %s"
                                              (mapconcat 'identity subgroup " "))
                                      :msg "tramps3: Creating dummy files...")))

(defun tramps3-clear-tmp-dir ()
  "Clear out the tramps3 tmp directory."
  (when (tramps3-file-exists tramps3-tmp-s3-dir)
    (tramps3-shell-command-no-message (format "rm -r %s" tramps3-tmp-s3-dir))))

;; validation

(defun tramps3-is-s3-path (path)
  "Confirm that this PATH is a valid s3 path."
  (tramps3-string-starts-with path "s3"))

(defun tramps3-is-directory (path)
  "Confirm that this PATH is a directory."
  (if (tramps3-is-s3-path path)
      (tramps3-string-ends-with path "/")
    (equal 0 (tramps3-shell-command-no-message (format "test -d %s" path)))))

;; s3 to local translation, path functions

(defun tramps3-local-path-to-s3-path (path)
  "Convert local PATH to an s3 path."
  (let ((s3-path (replace-regexp-in-string tramps3-tmp-s3-dir "s3:/" path)))
    (when (tramps3-is-s3-path s3-path)
      (if (and (tramps3-is-directory path) (not (tramps3-string-ends-with s3-path "/")))
          (concat s3-path "/")
        s3-path))))

(defun tramps3-s3-path-to-local-path (path)
  "Convert s3 PATH to a local path."
  (when (tramps3-is-s3-path path)
    (let ((local-path (replace-regexp-in-string "s3:/" tramps3-tmp-s3-dir path)))
      local-path)))

(defun tramps3-buffer-s3-path ()
  "Get associated s3 path of current buffer."
  (tramps3-local-path-to-s3-path (buffer-file-name)))

(defun tramps3-parent-directory (path)
  "Get parent directory path of PATH."
  (if (tramps3-string-ends-with path "/")
      (concat (mapconcat 'identity(-drop-last 2 (split-string path "/")) "/") "/")
    (concat (mapconcat 'identity (-drop-last 1 (split-string path "/")) "/") "/")))

;; working with files

(defun tramps3-refresh-directory (&optional input-dir)
  "Refresh INPUT-DIR from s3. If no input dir is specified, PWD will be used."
  (let* ((current-directory (if input-dir input-dir (nth 1 (split-string (pwd)))))
         (s3-directory (tramps3-local-path-to-s3-path current-directory))
         (file-list (-filter (lambda (f) f) (tramps3-s3-ls s3-directory)))
         (full-s3-paths (-map (lambda (file) (concat s3-directory file))
                                 file-list))
         (organized-file-list (--separate (tramps3-is-directory it) full-s3-paths))
         (new-dirs (-map (lambda (f) (tramps3-s3-path-to-local-path f))
                         (car organized-file-list)))
         (new-files (-map (lambda (f) (tramps3-s3-path-to-local-path f))
                          (car (-take-last 1 organized-file-list)))))

    ;; reset local tmp directory
    (tramps3-clear-tmp-dir)
    (if (tramps3-is-directory s3-directory)
        (make-directory current-directory t)
      (make-directory (tramps3-parent-directory current-directory) t))

    ;; rebuild from files s3
    (when new-dirs (tramps3-mkdirs new-dirs))
    (when new-files (tramps3-create-empty-files new-files))

    ;; revert the buffer if necessary
    (when (and (not (tramps3-is-dired-active))
               (equal (buffer-file-name) input-dir))
      (revert-buffer t t))))

(provide 'tramps3-io)

;;; tramps3-io.el ends here
