;;; tramps3-tests.el --- tests for tramps3

;;; Code:

(require 'dash)

(require 'tramps3-util)

(defmacro tramps3-setup-teardown-test-dir (&rest body)
  (let ((dir (make-symbol "dir")))
    `(let ((,dir (file-name-as-directory (make-temp-file "tramps3-" t))))
       (let ((tramps3-test-directory ,dir))
         ,@body)
       (delete-directory ,dir t))))

(ert-deftest tramps3-string-starts-with ()
  (should (tramps3-string-starts-with "string" "s"))
  (should (not (tramps3-string-starts-with "xstring" "s"))))

(ert-deftest tramps3-string-ends-with ()
  (should (tramps3-string-ends-with "string" "g"))
  (should (not (tramps3-string-ends-with "string" "s"))))

(ert-deftest tramps3-is-dired-active ()
  (should (not (tramps3-is-dired-active)))
  (dired "/")
  (should (tramps3-is-dired-active)))

(require 'tramps3-io)

(ert-deftest tramps3-s3-ls-test ()
  (let* ((inhibit-message t)
         (res (tramps3-s3-ls "s3://tramps3/")))
    (should (equal res '("test/" "testdir/" "testfile")))))

(ert-deftest tramps3-refresh-directory-test ()
  (tramps3-setup-teardown-test-dir
   (let* ((inhibit-message t)
          (tramps3-tmp-s3-dir (substring tramps3-test-directory 0 -1))
          (tramps3-subdir (format "%s/tramps3/" tramps3-tmp-s3-dir)))
     (tramps3-refresh-directory tramps3-subdir)
     (let ((organized-file-list (--separate (tramps3-is-directory
                                             (format "%s%s" tramps3-subdir it))
                                            (directory-files tramps3-subdir))))
       (should (and (member "test" (car organized-file-list))
                    (member "testdir" (car organized-file-list))))
       (should (and (member "testfile" (car (-take-last 1 organized-file-list)))))))))

(ert-deftest tramps3-mkdirs-test ()
  (tramps3-setup-teardown-test-dir
   (let ((inhibit-message t))
     (tramps3-mkdirs `(,(format "%s/test1" tramps3-test-directory)
                       ,(format "%s/test2" tramps3-test-directory)
                       ,(format "%s/test3" tramps3-test-directory)))
     (should (and (member "test1" (directory-files tramps3-test-directory))
                  (member "test2" (directory-files tramps3-test-directory))
                  (member "test3" (directory-files tramps3-test-directory))))
     (should (and (tramps3-is-directory (format "%s/test1" tramps3-test-directory))
                  (tramps3-is-directory (format "%s/test2" tramps3-test-directory))
                  (tramps3-is-directory (format "%s/test3" tramps3-test-directory)))))))

(ert-deftest tramps3-create-empty-file-test ()
  (tramps3-setup-teardown-test-dir
   (let ((test-file (format "%s/test1" tramps3-test-directory)))
     (let ((inhibit-message t))
       (tramps3-create-empty-file test-file))
     (should (member "test1" (directory-files tramps3-test-directory)))
     (should (equal "" (with-temp-buffer
                         (insert-file-contents test-file)
                         (buffer-string)))))))

(ert-deftest tramps3-is-s3-path-test ()
  (should (tramps3-is-s3-path "s3://path/"))
  (should (not (tramps3-is-s3-path "/not/s3"))))

(ert-deftest tramps3-is-directory-test ()
  (tramps3-setup-teardown-test-dir
   (let ((directory-name (format "%s/test-directory" tramps3-test-directory)))
     (make-directory directory-name t)
     (should (tramps3-is-directory directory-name)))))

(ert-deftest tramps3-local-path-to-s3-path-test ()
  (should (equal (tramps3-local-path-to-s3-path (format "%s/local/path" tramps3-tmp-s3-dir))
                 "s3://local/path"))
  (should (not (tramps3-local-path-to-s3-path "typo/local/path"))))

(ert-deftest tramps3-s3-path-to-local-path-test ()
  (should (equal (tramps3-s3-path-to-local-path "s3://s3/path")
                 (format "%s/s3/path" tramps3-tmp-s3-dir)))
  (should (not (tramps3-s3-path-to-local-path "s4://typo/local/path"))))

(ert-deftest tramps3-parent-directory-test ()
  (should (equal (tramps3-parent-directory "/parent/child")
                 "/parent/"))
  (should (equal (tramps3-parent-directory "/parent/child-dir/")
                 "/parent/")))

;;; tramps3-tests.el ends here
