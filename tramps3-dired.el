(require 'tramps3-mode)
(require 'tramps3-io)

(defun tramps3-dired-do-s3-delete (orig-dired-do-delete &rest args)
  (if (and (is-tramps3-mode-active)
           (y-or-n-p (format "Delete %s?" (tramps3-local-path-to-s3-path (dired-get-filename)))))

      (let ((current-local-file (dired-get-filename)))
        (progn
          (if (and (tramps3-is-directory current-local-file)
                   (y-or-n-p (format "Recursively delete %s?" (tramps3-local-path-to-s3-path current-local-file))))
              (delete-directory current-local-file t)
            (delete-file current-local-file))
          (tramps3-s3-rm (tramps3-local-path-to-s3-path current-local-file) (tramps3-is-directory current-local-file))
          (revert-buffer t t)))))

(advice-add 'dired-do-delete :around #'tramps3-dired-do-s3-delete)

(defun tramps3-dired-do-s3-flagged-delete (orig-dired-do-flagged-delete &rest args)
  (if (and (is-tramps3-mode-active))
      (let ((current-local-files (dired-get-marked-files)))
        (if (y-or-n-p (format "Delete %s marked files?" (length current-local-files)))
            (progn
              (dolist (current-local-file current-local-files)
                (progn
                  (if (and (tramps3-is-directory current-local-file)
                           (y-or-n-p (format "Recursively delete %s?" (tramps3-local-path-to-s3-path current-local-file))))
                      (delete-directory current-local-file t)
                    (delete-file current-local-file))
                  (tramps3-s3-rm (tramps3-local-path-to-s3-path current-local-file)
                                 (tramps3-is-directory current-local-file))
                  (revert-buffer t t))))))))

(advice-add 'dired-do-flagged-delete :around #'tramps3-dired-do-s3-flagged-delete)


(defun tramps3-dired-do-s3-refresh (orig-dired-do-refresh &rest args)
  (if (and (is-tramps3-mode-active)
           (tramps3-is-dired-active))
      (tramps3-refresh-directory))
  (apply orig-dired-do-refresh args))

(advice-add 'revert-buffer :around #'tramps3-dired-do-s3-refresh)

(defun tramps3-dired-do-s3-copy (orig-dired-do-copy &rest args)
  (if (is-tramps3-mode-active)
      (let* ((dest-filepath (tramps3-completing-read (tramps3-parent-directory (tramps3-local-path-to-s3-path (dired-get-filename)))
                                                     (format "Copy %s to:" (tramps3-local-path-to-s3-path (dired-get-filename)))))
             (current-local-file (dired-get-filename)))
        (progn
          (if (tramps3-is-directory current-local-file)
              (copy-directory current-local-file (tramps3-s3-path-to-local-path dest-filepath))
            (copy-file current-local-file (tramps3-s3-path-to-local-path dest-filepath)))
          (tramps3-s3-cp (tramps3-local-path-to-s3-path current-local-file) dest-filepath
                         (tramps3-is-directory current-local-file))
          (revert-buffer t t)))))

(advice-add 'dired-do-copy :around #'tramps3-dired-do-s3-copy)


(defun tramps3-dired-do-s3-rename (orig-dired-do-rename &rest args)
  (if (is-tramps3-mode-active)
      (let* ((dest-filepath (tramps3-completing-read (tramps3-parent-directory (tramps3-local-path-to-s3-path (dired-get-filename)))
                                                     (format "Rename %s to:" (tramps3-local-path-to-s3-path (dired-get-filename)))))
             (current-local-file (dired-get-filename)))
        (progn
          (rename-file current-local-file (tramps3-s3-path-to-local-path dest-filepath) t)
          (tramps3-s3-mv (tramps3-local-path-to-s3-path current-local-file) dest-filepath
                         (tramps3-is-directory current-local-file))
          (revert-buffer t t)))))

(advice-add 'dired-do-rename :around #'tramps3-dired-do-s3-rename)

(defun tramps3-dired-do-s3-copy (orig-dired-do-copy &rest args)
  (if (is-tramps3-mode-active)
      (let* ((dest-filepath (tramps3-completing-read (tramps3-parent-directory (tramps3-local-path-to-s3-path (dired-get-filename)))
                                                     (format "Copy %s to:" (tramps3-local-path-to-s3-path (dired-get-filename)))))
             (current-local-file (dired-get-filename)))
        (progn
          (if (tramps3-is-directory (tramps3-local-path-to-s3-path current-local-file))
              (copy-directory current-local-file (tramps3-s3-path-to-local-path dest-filepath))
            (copy-file current-local-file (tramps3-s3-path-to-local-path dest-filepath)))
          (tramps3-s3-cp (tramps3-local-path-to-s3-path current-local-file) dest-filepath t)
          (revert-buffer t t)))))

(advice-add 'dired-do-copy :around #'tramps3-dired-do-s3-copy)

(defun tramps3-dired-find-s3-file (orig-dired-find-file &rest args)
  (if (is-tramps3-mode-active)
      (let* ((current-local-file (dired-get-filename))
             (tramps3-is-directory (tramps3-local-path-to-s3-path current-local-file)))
        (if (tramps3-is-directory current-local-file)
            (progn
              (apply orig-dired-find-file args)
              (revert-buffer t t))
          (progn
            (tramps3-s3-cp (tramps3-local-path-to-s3-path current-local-file) current-local-file)
            (apply orig-dired-find-file args)
            (tramps3-mode))))))

(advice-add 'dired-find-file :around #'tramps3-dired-find-s3-file)
