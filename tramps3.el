(require 'tramps3-constants)
(require 'tramps3-mode)

;; utility functions

(defun tramps3-find-file ()
  "Interactive function for finding files in s3"
  (interactive)
  ;; check if this is an s3 path
  (let ((s3-path (tramps3-completing-read "s3://" "Find S3 file")))
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

(provide 'tramps3)

;; (global-set-key (kbd "C-c C-s C-f") 'tramps3-find-file)
