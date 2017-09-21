(require 'tramps3-util)

;;;;;;;;;;;;;;;;;;;;
;; s3 functions

(defun s3-ls (path)
  "List an s3 path"
  (seq-remove (lambda (el) (string= "" el))
              (mapcar (lambda (el) (car (last (split-string el " "))))
                      (split-string (shell-command-no-message (format "aws s3 ls %s" path) t)
                                    "\n"))))

(defun s3-cp (src dest)
  "Copy s3 src file to dest"
  (shell-command-no-message (format "aws s3 cp --sse AES256 %s %s" src dest)))

(defun s3-cp-r (src dest)
  "Recursively copy s3 src directory to dest"
  (shell-command-no-message (format "aws s3 cp --sse AES256 --recursive %s %s" src dest)))

;;;;;;;;;;;;;;;;;;;;
;; local filesystem functions

(defun mkdir (path)
  (shell-command-no-message (format "mkdir -p %s" path)))

(defun mkdirs (paths)
  (shell-command-no-message (format "mkdir -p %s" (mapconcat 'identity paths " "))))

(defun create-empty-file (filename)
  (shell-command-no-message (format "touch %s" filename)))

(defun create-empty-files (filenames)
  (shell-command-no-message (format "touch %s"
                                    (mapconcat 'identity filenames " "))))

;;;;;;;;;;;;;;;;;;;;
;; validation

(defun is-s3-path (path)
  "Confirm that this path is a valid s3 path"
  (string/starts-with path "s3"))

(defun is-directory (path)
  "Confirm that this path is a directory"
  (if (is-s3-path path)
      (string/ends-with path "/")
    (condition-case nil
        (equal 0 (shell-command-no-message (format "test -d %s" path)))
      (error nil))))

;;;;;;;;;;;;;;;;;;;;
;; s3 to local translation, path functions

(defun local-path-to-s3-path (path)
  "Convert local path to an s3 path"
  (let ((s3-path (replace-regexp-in-string TMP_S3_DIR "s3:/" path)))
    (when (is-s3-path s3-path)
      (if (and (is-directory path) (not (string/ends-with s3-path "/")))
          (concat s3-path "/")
        s3-path))))

(defun s3-path-to-local-path (path)
  "Convert s3 path to a local path"
  (if (is-s3-path path)
      (let ((local-path (replace-regexp-in-string "s3:/" TMP_S3_DIR path)))
        local-path)
    (signal 'error "Invalid s3 path provided")))

(defun buffer-s3-path ()
  "Get associated s3 path of current buffer"
  (local-path-to-s3-path (buffer-file-name)))

(defun parent-directory (path)
  "Get parent directory path of path"
  (if (string/ends-with path "/")
      (concat (mapconcat 'identity (butlast (butlast (split-string path "/"))) "/") "/")
    (concat (mapconcat 'identity (butlast (split-string path "/")) "/") "/")))

;;;;;;;;;;;;;;;;;;;;
;; working with files

(defun tramps3-open-file (input-file)
  "Open tramps3 buffer at input-file. Will be a refreshed dired buffer if it is a directory."
  (unless (is-directory input-file)
    (s3-cp (local-path-to-s3-path input-file) input-file))
  (tramps3-refresh-directory input-file)
  (find-file input-file)
  (tramps3-mode))

(defun tramps3-refresh-directory (&optional input-dir)
  "Refresh input-dir from s3 by making empty directories and creating empty files"
  (let* ((current-directory (if input-dir input-dir (nth 1 (split-string (pwd)))))
         (s3-directory (local-path-to-s3-path current-directory))
         (s3-parent-directory (local-path-to-s3-path (if (is-directory current-directory)
                                                         current-directory
                                                       (parent-directory current-directory)))))
    (if (and s3-directory
             (not (equal s3-directory "s3://")))
        (let* ((file-list (s3-ls s3-directory))
               (full-local-paths (mapcar (lambda (file)
                                           (s3-path-to-local-path (concat s3-parent-directory file)))
                                         file-list))
               (organized-file-list (seq-group-by (lambda (path) (string/ends-with path "/")) full-local-paths)))
          (dolist (group organized-file-list)
            (if (pop group)
                ;; make all directories
                (mkdirs group)

              ;; make empty files
              (create-empty-files group)))

          ;; revert the buffer if necessary
          (when (or (is-dired-active)
                  (equal (buffer-file-name) input-dir))
              (revert-buffer t t)))
      (signal 'error "Not an in an s3 directory, or invalid directory provided."))))

(provide 'tramps3-io)
