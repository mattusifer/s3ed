(require 'tramps3-constants)
(require 'tramps3-mode)

;; utility functions

(defun tramps3-run-command (msg callback)
  ;; check if this is an s3 path
  (let* ((base-path (if (is-tramps3-mode-active)
                        (tramps3-local-path-to-s3-path default-directory)
                      "s3://"))
         (s3-path (tramps3-completing-read base-path msg)))
    (if (tramps3-is-s3-path s3-path)
        (let (;; full path to tmp file
              (tmp-path (format "%s/%s" TRAMPS3_TMP_S3_DIR
                                (mapconcat 'identity (nthcdr 2 (split-string s3-path "/")) "/")))

              ;; parent directory of temp file
              (tmp-dir (format "%s/%s" TRAMPS3_TMP_S3_DIR
                               (mapconcat 'identity (butlast (nthcdr 2 (split-string s3-path "/"))) "/"))))

          ;; create tmp dir
          (condition-case nil
              (delete-directory TRAMPS3_TMP_S3_DIR t)
            (error nil))
          (make-directory tmp-dir t)

          ;; run callback
          (funcall callback))
      (message "S3 path is required"))))

(defun tramps3-find-file ()
  "Interactive function for finding files in s3"
  (interactive)
  (tramps3-run-command "Find S3 file" (lambda () (tramps3-open-file tmp-path))))

(defun tramps3-save-file ()
  "Interactive function for finding files in s3"
  (interactive)
  (tramps3-run-command "Save S3 file" (lambda () (tramps3-write-file tmp-path))))

(provide 'tramps3)
