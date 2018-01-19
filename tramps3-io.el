(require 'tramps3-util)

;;;;;;;;;;;;;;;;;;;;
;; s3 functions

(defun tramps3-get-transfer-message (src dest)
  "Get message to display when copying data"
  (format "%s: Transferring data %s s3..." TRAMPS3_APP_NAME
          (if (tramps3-is-s3-path src) (if (tramps3-is-s3-path dest) "within" "from") "to")))

(defun tramps3-s3-ls (path)
  "List an s3 path"
  (condition-case nil
      (seq-remove (lambda (el) (string= "" el))
                  (mapcar (lambda (el) (car (last (split-string el " "))))
                          (split-string (tramps3-shell-command-no-message (format "aws s3 ls %s" path) t
                                                                          (format "%s: Listing files on s3..." TRAMPS3_APP_NAME))
                                        "\n")))
    (error (message "CREDS NOT CONFIGURED"))))

(defun tramps3-s3-cp (src dest &optional recursive)
  "Copy s3 src file to dest"
  (let ((msg (tramps3-get-transfer-message src dest)))
    (tramps3-shell-command-no-message (format "aws s3 cp %s --sse AES256 %s %s"
                                              (if recursive "--recursive" "") src dest)
                                      :msg msg)))

(defun tramps3-s3-mv (src dest &optional recursive)
  "Copy s3 src file to dest"
  (let ((msg (tramps3-get-transfer-message src dest)))
    (tramps3-shell-command-no-message (format "aws s3 mv %s --sse AES256 %s %s"
                                              (if recursive "--recursive" "") src dest)
                                      :msg msg)))

(defun tramps3-s3-rm (path &optional recursive)
  (let ((msg (format "%s: Removing data from s3..." TRAMPS3_APP_NAME)))
    (tramps3-shell-command-no-message (format "aws s3 rm %s %s" (if recursive "--recursive" "") path)
                                      :msg msg)))

;;;;;;;;;;;;;;;;;;;;
;; local filesystem functions

(defun tramps3-mkdirs (paths)
  (tramps3-shell-command-no-message (format "mkdir -p %s" (mapconcat 'identity paths " "))
                                    :msg "tramps3: Creating local directories..."))

(defun tramps3-create-empty-file (filename)
  (tramps3-shell-command-no-message (format "touch %s" filename)
                                    :msg "tramps3: Creating dummy file..."))

(defun tramps3-create-empty-files (filenames)
  (tramps3-shell-command-no-message (format "touch %s"
                                            (mapconcat 'identity filenames " "))
                                    :msg "tramps3: Creating dummy files..."))

;;;;;;;;;;;;;;;;;;;;
;; validation

(defun tramps3-is-s3-path (path)
  "Confirm that this path is a valid s3 path"
  (tramps3-string-starts-with path "s3"))

(defun tramps3-is-directory (path)
  "Confirm that this path is a directory"
  (if (tramps3-is-s3-path path)
      (tramps3-string-ends-with path "/")
    (condition-case nil
        (equal 0 (tramps3-shell-command-no-message (format "test -d %s" path)))
      (error nil))))

;;;;;;;;;;;;;;;;;;;;
;; s3 to local translation, path functions

(defun tramps3-local-path-to-s3-path (path)
  "Convert local path to an s3 path"
  (let ((s3-path (replace-regexp-in-string TRAMPS3_TMP_S3_DIR "s3:/" path)))
    (when (tramps3-is-s3-path s3-path)
      (if (and (tramps3-is-directory path) (not (tramps3-string-ends-with s3-path "/")))
          (concat s3-path "/")
        s3-path))))

(defun tramps3-s3-path-to-local-path (path)
  "Convert s3 path to a local path"
  (when (tramps3-is-s3-path path)
    (let ((local-path (replace-regexp-in-string "s3:/" TRAMPS3_TMP_S3_DIR path)))
      local-path)))

(defun tramps3-buffer-s3-path ()
  "Get associated s3 path of current buffer"
  (tramps3-local-path-to-s3-path (buffer-file-name)))

(defun tramps3-parent-directory (path)
  "Get parent directory path of path"
  (if (tramps3-string-ends-with path "/")
      (concat (mapconcat 'identity (butlast (butlast (split-string path "/"))) "/") "/")
    (concat (mapconcat 'identity (butlast (split-string path "/")) "/") "/")))

;;;;;;;;;;;;;;;;;;;;
;; working with files

(defun tramps3-refresh-directory (&optional input-dir)
  "Refresh input-dir from s3 by making empty directories and creating empty files"
  (let* ((current-directory (if input-dir input-dir (nth 1 (split-string (pwd)))))
         (s3-directory (tramps3-local-path-to-s3-path current-directory))
         (s3-tramps3-parent-directory (tramps3-local-path-to-s3-path (if (tramps3-is-directory current-directory)
                                                                         current-directory
                                                                       (tramps3-parent-directory current-directory)))))
    (if (and s3-directory
             (not (equal s3-directory "s3://")))
        (let* ((file-list (tramps3-s3-ls s3-directory))
               (full-local-paths (mapcar (lambda (file)
                                           (tramps3-s3-path-to-local-path (concat s3-tramps3-parent-directory file)))
                                         file-list))
               (organized-file-list (seq-group-by (lambda (path) (tramps3-string-ends-with path "/")) full-local-paths)))

          ;; clear out local directory
          (when (tramps3-is-directory current-directory)
            (delete-directory current-directory t)
            (make-directory current-directory t))

          ;; rebuild from files s3
          (dolist (group organized-file-list)
            (if (pop group)
                ;; make all directories
                (tramps3-mkdirs group)

              ;; make empty files
              (tramps3-create-empty-files group)))

          ;; revert the buffer if necessary
          (when (or (tramps3-is-dired-active)
                    (equal (buffer-file-name) input-dir))
            (revert-buffer t t)))
      (signal 'error "Not an in an s3 directory, or invalid directory provided."))))


(defun tramps3-open-file (input-file)
  "Open tramps3 buffer at input-file. Will be a refreshed dired buffer if it is a directory."
  (unless (tramps3-is-directory input-file)
    (tramps3-s3-cp (tramps3-local-path-to-s3-path input-file) input-file))
  (tramps3-refresh-directory input-file)
  (find-file input-file)
  (tramps3-mode))

(defun tramps3-write-file (input-file)
  "Save input file to s3"
  (unless (tramps3-is-directory input-file)
    (tramps3-s3-cp input-file (tramps3-local-path-to-s3-path input-file)))
  (tramps3-refresh-directory input-file)
  (find-file input-file)
  (tramps3-mode))

(defun tramps3-delete-directory (&optional input-dir)
  "Delete local file or directory, as well as corresponding path on s3"
  (let* ((current-directory (if input-dir input-dir (nth 1 (split-string (pwd)))))
         (s3-directory (tramps3-local-path-to-s3-path current-directory))
         (recursive (when (tramps3-is-directory s3-directory)
                      (y-or-n-p (format "%s: Recursively delete %s?" TRAMPS3_APP_NAME s3-directory)))))
    (if (tramps3-is-directory current-directory)
        (delete-directory current-directory recursive)
      (delete-file current-directory))
    (tramps3-s3-rm s3-directory recursive)
    (revert-buffer t t)))

(defun tramps3-move-directory (&optional input-dir copy)
  "Copy local file or directory (recursively), as well as corresponding path on s3"
  (let* ((current-directory (if input-dir input-dir (nth 1 (split-string (pwd)))))
         (s3-directory (tramps3-local-path-to-s3-path current-directory))
         (completing-read-msg (format (if copy "Copy %s to" "Move %s to") s3-directory))
         (dest-s3-directory (tramps3-completing-read (tramps3-parent-directory s3-directory)
                                                     completing-read-msg))
         (dest-local-directory (tramps3-s3-path-to-local-path dest-s3-directory)))
    (make-directory (tramps3-parent-directory dest-local-directory) t)
    (if copy
      (if (tramps3-is-directory current-directory)
          (progn (tramps3-s3-cp s3-directory dest-s3-directory t)
                 (copy-directory current-directory dest-local-directory t))
        (progn (tramps3-s3-cp s3-directory dest-s3-directory)
               (copy-file current-directory dest-local-directory t)))
      (progn
        (tramps3-s3-mv s3-directory dest-s3-directory (tramps3-is-directory current-directory))
        (rename-file current-directory dest-local-directory t)))
    (revert-buffer t t)))

(provide 'tramps3-io)
