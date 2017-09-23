(defun tramps3-string-starts-with (s prefix)
  "Return non-nil if string s starts with prefix."
      (cond ((>= (length s) (length prefix))
             (string-equal (substring s 0 (length prefix)) prefix))
            (t nil)))

(defun tramps3-string-ends-with (s suffix)
  "Return non-nil if string s starts with prefix."
  (not (equal nil (string-match (format "%s\\'" suffix) s))))

(defun tramps3-is-dired-active ()
  "t if we are in a dired buffer"
  (equal major-mode 'dired-mode))

(defun tramps3-shell-command-no-message (cmd &optional ret msg)
  "Inhibit messages from the emacs builtin shell-command
functions, replace with custom message 'msg' if provided. if
'ret' is not nil, results from shell command will be returned"
  (when msg (message msg))
  (let ((inhibit-message t))
    (if ret
        (shell-command-to-string cmd)
      (shell-command cmd))))

(defun tramps3-completing-read-backspace (cur-base msg)
  "Adjust normal completing-read behavior - go up to parent directory 
when backspace is pressed at the beginning of the string"
  (if (not (equal cur-base "s3://"))
      (condition-case nil
          ;; use normal backspace behavior if no error was found
          (backward-delete-char 1)

        ;; if an error was found, we are at the beginning of the
        ;; line. Recurse to the parent directory of the current path.
        (error (progn
                 (tramps3-completing-read (concat (mapconcat 'identity (butlast (butlast (split-string cur-base "/"))) "/") "/")
                                          msg)
                 (exit-recursive-edit))))

    ;; if we're at the base, allow errors, don't recurse
    (backward-delete-char 1)))

(defun tramps3-exit-all-minibuffers ()
  "Exit all tramps3 nested minibuffers"
  (top-level)
  (setq enable-recursive-minibuffers tramps3-old-recursive-minibuffer-setting))

;; finding files
(defun tramps3-completing-read (base msg)
  "Use completing-read to find files in s3"
  (let* ((bucket (equal base "s3://"))
         (choices (seq-remove (lambda (el) (not el)) (tramps3-s3-ls base)))
         (choice (minibuffer-with-setup-hook
                     (lambda ()
                       (setq tramps3-old-recursive-minibuffer-setting enable-recursive-minibuffers)
                       (setq enable-recursive-minibuffers t)
                       (define-key (current-local-map) (kbd "<backspace>") (lambda () (interactive) (tramps3-completing-read-backspace base msg)))
                       (define-key (current-local-map) (kbd "C-g") (lambda () (interactive) (tramps3-exit-all-minibuffers))))
                   (completing-read (format "%s: %s" msg base) choices))))
    (if (seq-contains choices choice)
        (if (and (not bucket) (not (string-match "/\\'" choice)))
            (concat base choice)
          (tramps3-completing-read (if bucket (format "%s%s/" base choice) (concat base choice)) msg))

      (concat base choice))))

(provide 'tramps3-util)
