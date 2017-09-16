(require 'tramps3-constants)
(require 'tramps3-mode)

;; utility functions

(defun string/starts-with (s prefix)
  "Return non-nil if string s starts with prefix."
      (cond ((>= (length s) (length prefix))
             (string-equal (substring s 0 (length prefix)) prefix))
            (t nil)))

(defun is-s3-path (path)
  "Confirm that this path is a valid s3 path"
  (string/starts-with path "s3"))

(defun tramps3-completing-read-backspace (cur-base)
  "Adjust normal completing-read behavior - go up to parent directory 
when backspace is pressed at the beginning of the string"
  (if (not (equal cur-base "s3://"))
      (condition-case nil
          ;; use normal backspace behavior if no error was found
          (backward-delete-char 1)

        ;; if an error was found, we are at the beginning of the
        ;; line. Recurse to the parent directory of the current path.
        (error (tramps3-completing-read (concat (mapconcat 'identity (butlast (butlast (split-string cur-base "/"))) "/") "/"))))

    ;; if we're at the base, allow errors, don't recurse
    (backward-delete-char 1)))

(defun exit-all-minibuffers ()
  "Exit all tramps3 nested minibuffers"
  (top-level)
  (setq enable-recursive-minibuffers tramps3-old-recursive-minibuffer-setting))

;; finding files
(defun tramps3-completing-read (base)
  "Use completing-read to find files in s3"
  (let* ((bucket (equal base "s3://"))
         (choices (seq-remove (lambda (el) (not el))
                              (mapcar (lambda (el) (car (last (split-string el " "))))
                                      (split-string (shell-command-to-string (format "aws s3 ls %s" base)) "\n"))))
         (choice (minibuffer-with-setup-hook
                     (lambda ()
                       (setq tramps3-old-recursive-minibuffer-setting enable-recursive-minibuffers)
                       (setq enable-recursive-minibuffers t)
                       (define-key (current-local-map) (kbd "<backspace>") (lambda () (interactive) (tramps3-completing-read-backspace base)))
                       (define-key (current-local-map) (kbd "C-g") (lambda () (interactive) (exit-all-minibuffers))))
                   (completing-read (format "Find S3 File: %s" base) choices))))
    (if (seq-contains choices choice)
        (if (and (not bucket) (not (string-match "/\\'" choice)))
            (concat base choice)
          (tramps3-completing-read (if bucket (format "%s%s/" base choice) (concat base choice))))

      (concat base choice))))

(defun tramps3-find-file ()
  "Interactive function for finding files in s3"
  (interactive)
  ;; check if this is an s3 path
  (let ((s3-path (tramps3-completing-read "s3://")))
    (if (is-s3-path s3-path)
        (let (;; full path to tmp file
              (tmp-path (format "%s/%s" TMP_S3_DIR
                                (mapconcat 'identity (nthcdr 2 (split-string s3-path "/")) "/")))

              ;; parent directory of temp file
              (tmp-dir (format "%s/%s" TMP_S3_DIR
                               (mapconcat 'identity (butlast (nthcdr 2 (split-string s3-path "/"))) "/"))))

          ;; create tmp dir
          (make-directory tmp-dir t)

          ;; copy file from aws (if this is a file)
          (if (not (string-match "/\\'" tmp-path))
              (shell-command (format "aws s3 cp %s %s" s3-path tmp-path)))

          ;; use original find-file on tmp file
          (find-file tmp-path)

          (tramps3-mode))
      (message "S3 path is required"))))

(provide 'tramps3)

;; (global-set-key (kbd "C-c C-s C-f") 'tramps3-find-file)


