;;; tramps3-tramp.el --- Literally use tramps3 from tramp

;; Copyright (C) 2018 Matt Usifer

;; Author: Matt Usifer <mattusifer@gmail.com>
;; Version: 0.1.0
;; Keywords: s3 tools
;; Homepage: https://github.com/mattusifer/tramps3

;; Tramps3 requires at least GNU Emacs 24.4

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

;; Add /s3: as a new tramp method

;;; Code:

(defconst tramps3-tramp-s3-method "s3")

(add-to-list 'tramp-methods `(,tramps3-tramp-s3-method))

(add-to-list 'tramp-foreign-file-name-handler-alist
             (cons 'tramps3-tramp-s3-file-name-p 'tramps3-tramp-s3-file-name-handler))

(defconst tramps3-tramp-s3-file-name-handler-alist
  '(;; `access-file' performed by default handler.
    (add-name-to-file . ignore)
    ;; `byte-compiler-base-file-name' performed by default handler.
    (copy-directory . tramps3-copy-directory)
    (copy-file . ignore)
    (delete-directory . ignore)
    (delete-file . ignore)
    ;; `diff-latest-backup-file' performed by default handler.
    (directory-file-name . ignore)
    (directory-files . ignore)
    (directory-files-and-attributes . ignore)
    (dired-call-process . ignore)
    (dired-compress-file . ignore)
    ;;(dired-recursive-delete-directory .
    (dired-uncache . ignore)
    (expand-file-name . ignore)
    (file-accessible-directory-p . ignore)
    (file-acl . ignore)
    (file-attributes . ignore)
    (file-directory-p . ignore)
    (file-executable-p . ignore)
    (file-exists-p . ignore)
    ;; `file-in-directory-p' performed by default handler.
    (file-local-copy . ignore)
    (file-modes . ignore)
    (file-name-all-completions . ignore)
    (file-name-as-directory . ignore)
    (file-name-completion . ignore)
    (file-name-directory . ignore)
    (file-name-nondirectory . ignore)
    (file-newer-than-file-p . ignore)
    (file-notify-add-watch . ignore)
    (file-notify-rm-watch . ignore)
    (file-ownership-preserved-p . ignore)
    (file-readable-p . ignore)
    (file-regular-p . ignore)
    (file-remote-p . ignore)
    ;; `file-selinux-context' performed by default handler.
    ;; `file-truename' performed by default handler.
    (file-writable-p . ignore)
    ;; `find-file-noselect' performed by default handler.
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . ignore)
    (insert-file-contents . ignore)
    (insert-file-contents-literally . ignore)
    (load . ignore)
    (make-auto-save-file-name . ignore)
    (make-directory . ignore)
    (make-symbolic-link . ignore)
    (process-file . ignore)
    (rename-file . ignore)
    (set-file-acl . ignore)
    (set-file-modes . ignore)
    (set-file-selinux-context . ignore)
    (set-file-times . ignore)
    (set-visited-file-modtime . ignore)
    (shell-command . ignore)
    (start-file-process . ignore)
    (unhandled-file-name-directory . ignore)
    (vc-registered . ignore)
    (verify-visited-file-modtime . ignore)
    (write-region . ignore))
  "Alist of handler functions.
Operations not mentioned here will be handled by the default Emacs primitives.")

;; It must be a `defsubst' in order to push the whole code into
;; tramp-loaddefs.el.  Otherwise, there would be recursive autoloading.
;;;###tramp-autoload
(defsubst tramps3-tramp-s3-file-name-p (filename)
  "Check if it's a FILENAME for s3."
  (string= (tramp-file-name-method (tramp-dissect-file-name filename))
           tramps3-tramp-s3-method))

(defun tramps3-tramp-s3-handle-file-readable-p (filename)
  "Like `file-readable-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-readable-p"
      ;; Examine `file-attributes' cache to see if request can be
      ;; satisfied without remote operation.
      ;;TODO we need to do actual check using rest calls
      (tramp-check-cached-permissions v ?r))))

;;;###tramp-autoload
(defun tramps3-tramp-s3-file-name-handler (operation &rest args)
  "Invoke the s3 related OPERATION.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION.
Optional argument ARGS is a list of arguments to pass to the OPERATION."

  (when (and tramp-locked (not tramp-locker))
    (setq tramp-locked nil)
    (tramp-error
     (car-safe tramp-current-connection) 'file-error
     "Forbidden reentrant call of Tramp"))
  (let ((tl tramp-locked))
    (setq tramp-locked t)
    (unwind-protect
        (let ((tramp-locker t))
          (save-match-data
            (let ((fn (assoc operation tramps3-tramp-s3-file-name-handler-alist)))
              (if fn
                  (apply (cdr fn) args)
                (tramp-run-real-handler operation args)))))
      (setq tramp-locked tl))))

;;;###tramp-autoload
(add-to-list 'tramp-foreign-file-name-handler-alist
             (cons 'tramps3-tramp-s3-file-name-p 'tramps3-tramp-s3-file-name-handler))

;;hadoop rest api https://hadoop.apache.org/docs/r1.0.4/webs3.html
