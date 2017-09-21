(defun string/starts-with (s prefix)
  "Return non-nil if string s starts with prefix."
      (cond ((>= (length s) (length prefix))
             (string-equal (substring s 0 (length prefix)) prefix))
            (t nil)))

(defun string/ends-with (s suffix)
  "Return non-nil if string s starts with prefix."
  (not (equal nil (string-match (format "%s\\'" suffix) s))))

(defun is-dired-active ()
  "t if we are in a dired buffer"
  (equal major-mode 'dired-mode))

(defun shell-command-no-message (cmd &optional ret)
  "Inhibit messages from the emacs builtin shell-command functions"
  (let ((inhibit-message t))
    (if ret
        (shell-command-to-string cmd)
      (shell-command cmd))))

(provide 'tramps3-util)