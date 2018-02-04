;;; s3ed-io.el --- I/O operations both locally and on s3

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

;; This library provides functions for interacting directly with files on
;; s3, as well as some functions for interactinve with local files.

;;; Code:

(require 'dash)

(require 's3ed-util)

(defconst s3ed-app-name "s3ed")
(defconst s3ed-tmp-s3-dir (concat "/tmp/" s3ed-app-name))

(defconst s3ed-streamable-commands
  '("head" "cat" "grep" "tail" "sed" "awk" "cut" "wc" "sort" "tr" "uniq" "lbzcat" "gzcat"))

;; s3 functions

(defun s3ed-get-transfer-message (src dest async)
  "Get message to display when transferring data from SRC to DEST.  If specified, this will be an ASYNC operation."
  (format "%s: Transferring data %s s3%s..." s3ed-app-name
          (if (s3ed-is-s3-path src) (if (s3ed-is-s3-path dest)
                                           "within" "from") "to")
          (if async " in the background" "")))

(defun s3ed-s3-ls (path)
  "List an s3 PATH."
  (cl-flet ((parse-s3-ls-raw-output (raw-line) (car (-take-last 1 (split-string it)))))
    (--remove (or (string= "" it) (not it))
              (--map (if (s3ed-is-root-s3-path path)
                         (concat (parse-s3-ls-raw-output it) "/")
                       (parse-s3-ls-raw-output it))
                     (split-string (s3ed-shell-command-no-message
                                    (format "aws s3 ls %s" path) t
                                    (format "%s: Listing files on s3..." s3ed-app-name)) "\n")))))

(defun s3ed-s3-cp (src dest &optional recursive async)
  "Copy s3 SRC file to DEST.  If specified, this will be a RECURSIVE and/or ASYNC operation."
  (let ((command (format "aws s3 cp %s --sse AES256 %s %s"
                         (if recursive "--recursive" "") src dest))
        (msg (s3ed-get-transfer-message src dest async)))
    (if async
        (progn
          (apply 'start-process "s3ed-cp" "*s3ed*" (split-string command))
          (message msg))
      (s3ed-shell-command-no-message command :msg msg))))

(defun s3ed-s3-cp-streams (srcs command &optional async)
  "Stream s3 SRCS into COMMAND.  If specified, this will be an ASYNC operation."
  (let ((command (format "(%s) | %s" (mapconcat 'identity (--map (format "aws s3 cp %s -;" it)
                                                                 srcs) " ") command)))
    (if async
        (async-shell-command command)
        (shell-command command))))

(defun s3ed-s3-mv (src dest &optional recursive async)
  "Move s3 SRC file to DEST.  If specified, this will be a RECURSIVE and/or ASYNC operation."
  (let ((command (format "aws s3 mv %s --sse AES256 %s %s"
                         (if recursive "--recursive" "") src dest))
        (msg (s3ed-get-transfer-message src dest async)))
    (if async
        (progn
          (apply 'start-process "s3ed-mv" "*s3ed*" (split-string command))
          (message msg))
      (s3ed-shell-command-no-message command :msg msg))))

(defun s3ed-s3-rm (path &optional recursive async)
  "Remove file or directory PATH from s3. If specified, this will be a RECURSIVE and/or ASYNC operation."
  (let ((msg (format "%s: Removing data from s3%s..." s3ed-app-name
                     (if async " in the background" "")))
        (command (format "aws s3 rm %s %s" (if recursive "--recursive" "") path)))
    (if async
        (progn
          (apply 'start-process "s3ed-rm" "*s3ed*" (split-string command))
          (message msg))
        (s3ed-shell-command-no-message command :msg msg))))

;; local filesystem functions

(defun s3ed-mkdirs (paths)
  "Create directories at the given PATHS."
  (dolist (subgroup (-partition-all 100 paths))
    (s3ed-shell-command-no-message (format "mkdir -p %s" (mapconcat 'identity subgroup " "))
                                      :msg "s3ed: Creating local directories...")))

(defun s3ed-create-empty-file (filename)
  "Create FILENAME if it doesn't exist."
  (s3ed-shell-command-no-message (format "mkdir -p %s && touch %s" (s3ed-parent-directory filename)
                                            filename)
                                    :msg "s3ed: Creating dummy file..."))

(defun s3ed-create-empty-files (filenames)
  "Create all files in FILENAMES if they don't exist."
  (dolist (subgroup (-partition-all 100 filenames))
    (s3ed-mkdirs (--map (s3ed-parent-directory it) subgroup))
    (s3ed-shell-command-no-message (format "touch %s"
                                              (mapconcat 'identity subgroup " "))
                                      :msg "s3ed: Creating dummy files...")))

(defun s3ed-rm (file-or-directory)
  "Recursively remove FILE-OR-DIRECTORY.
This will only run if FILE-OR-DIRECTORY is in the s3ed-tmp-s3-dir."
  (when (s3ed-string-starts-with file-or-directory s3ed-tmp-s3-dir)
    (shell-command (format "rm -rf %s" file-or-directory))))

;; validation

(defun s3ed-is-s3-path (path)
  "Confirm that this PATH is a valid s3 path."
  (s3ed-string-starts-with path "s3"))

(defun s3ed-is-directory (path)
  "Confirm that this PATH is a directory."
  (if (s3ed-is-s3-path path)
      (s3ed-string-ends-with path "/")
    (equal 0 (s3ed-shell-command-no-message (format "test -d %s" path)))))

;; s3 to local translation, path functions

(defun s3ed-local-path-to-s3-path (path)
  "Convert local PATH to an s3 path."
  (let ((s3-path (replace-regexp-in-string s3ed-tmp-s3-dir "s3:/" path)))
    (when (s3ed-is-s3-path s3-path)
      (if (and (s3ed-is-directory path) (not (s3ed-string-ends-with s3-path "/")))
          (concat s3-path "/")
        s3-path))))

(defun s3ed-s3-path-to-local-path (path)
  "Convert s3 PATH to a local path."
  (when (s3ed-is-s3-path path)
    (let ((local-path (replace-regexp-in-string "s3:/" s3ed-tmp-s3-dir path)))
      local-path)))

(defun s3ed-buffer-s3-path ()
  "Get associated s3 path of current buffer."
  (s3ed-local-path-to-s3-path (buffer-file-name)))

(defun s3ed-parent-directory (path)
  "Get parent directory path of PATH."
  (if (s3ed-string-ends-with path "/")
      (concat (mapconcat 'identity (-drop-last 2 (split-string path "/")) "/") "/")
    (concat (mapconcat 'identity (-drop-last 1 (split-string path "/")) "/") "/")))

;; working with files

(defun s3ed-refresh-tmp-dir (&optional input-dir)
  "Refresh all active s3ed buffers, including INPUT-DIR if provided."
  (let* ((all-active-files-dirs (--map (with-current-buffer it (if (s3ed-is-dired-active)
                                                                   default-directory
                                                                 buffer-file-name))
                                       (buffer-list)))
         (all-s3ed-files-dirs (--separate (s3ed-is-directory
                                              (s3ed-local-path-to-s3-path it))
                                             (--filter (s3ed-string-starts-with
                                                        it s3ed-tmp-s3-dir)
                                                       (add-to-list 'all-active-files-dirs
                                                                    input-dir))))
         (active-directory (condition-case nil (if (s3ed-is-dired-active) default-directory
                                                          (s3ed-parent-directory buffer-file-name))
                                      (error nil))))
    (when active-directory (make-directory active-directory t))
    (s3ed-rm s3ed-tmp-s3-dir)

    (dolist (current-directory (car all-s3ed-files-dirs))
      (make-directory current-directory t)
      (when (or (equal current-directory active-directory) (equal current-directory input-dir))
          (let* ((s3-directory (s3ed-local-path-to-s3-path current-directory))
                 (file-list (-filter (lambda (f) f) (s3ed-s3-ls s3-directory)))
                 (full-s3-paths (-map (lambda (file) (concat s3-directory file))
                                      file-list))
                 (organized-file-list (--separate (s3ed-is-directory it) full-s3-paths))
                 (s3-dirs (-map (lambda (f) (s3ed-s3-path-to-local-path f))
                                (car organized-file-list)))
                 (s3-files (-map (lambda (f) (s3ed-s3-path-to-local-path f))
                                 (car (-take-last 1 organized-file-list)))))

            ;; reset local tmp directory and delete the current directory
            ;; rebuild from files s3
            (when s3-dirs (s3ed-mkdirs s3-dirs))
            (when s3-files (s3ed-create-empty-files s3-files)))))

    (dolist (current-file (car (-take-last 1 all-s3ed-files-dirs)))
      (let ((s3-file (s3ed-local-path-to-s3-path current-file)))
        (s3ed-s3-cp s3-file current-file)))))

(provide 's3ed-io)

;;; s3ed-io.el ends here
