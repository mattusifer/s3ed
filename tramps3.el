(require 'tramps3-constants)
(require 'tramps3-mode)

;; utility functions

(defun tramps3-find-file ()
  "Interactive function for finding files in s3"
  (interactive)
  ;; check if this is an s3 path
  (let* ((base-path (if (is-tramps3-mode-active)
                        (tramps3-local-path-to-s3-path default-directory)
                      "s3://"))
         (s3-path (tramps3-completing-read base-path "Find S3 file")))
    (if (tramps3-is-s3-path s3-path)
        (let (;; full path to tmp file
              (tmp-path (format "%s/%s" TRAMPS3_TMP_S3_DIR
                                (mapconcat 'identity (nthcdr 2 (split-string s3-path "/")) "/")))

              ;; parent directory of temp file
              (tmp-dir (format "%s/%s" TRAMPS3_TMP_S3_DIR
                               (mapconcat 'identity (butlast (nthcdr 2 (split-string s3-path "/"))) "/"))))

          ;; create tmp dir
          (make-directory tmp-dir t)

          ;; open file or directory (in dired)
          (tramps3-open-file tmp-path)

          (tramps3-mode))
      (message "S3 path is required"))))

(defun tramps3-save-file ()
  "Interactive function for saving files to s3"
  (interactive)
  ;; check if this is an s3 path
  (let* ((base-path (if (is-tramps3-mode-active)
                        (tramps3-local-path-to-s3-path default-directory)
                      "s3://"))
         (s3-path (tramps3-completing-read base-path "Save file to S3")))
    (if (tramps3-is-s3-path s3-path)
        (let (;; full path to tmp file
              (tmp-path (format "%s/%s" TRAMPS3_TMP_S3_DIR
                                (mapconcat 'identity (nthcdr 2 (split-string s3-path "/")) "/")))
              (full-file-path (format "%s/%s" tmp-path
                                      (car (last (split-string s3-path "/"))))))

          ;; create tmp dir
          (make-directory tmp-path t)

          (write-file full-file-path)

          (tramps3-write-file full-file-path)

          (tramps3-mode))
      (message "S3 path is required"))))

(provide 'tramps3)

;; (global-set-key (kbd "C-c C-s C-f") 'tramps3-find-file)

;; (global-set-key (kbd "C-c C-s C-s") 'tramps3-save-file)
