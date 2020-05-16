;;; s3ed-tests.el --- tests for s3ed

;;; Code:

(require 'dash)
(require 's)

(require 's3ed-util)

(defmacro s3ed-setup-teardown-test-dir (&rest body)
  (let ((dir (make-symbol "dir")))
    `(let ((,dir (file-name-as-directory (make-temp-file "s3ed-" t))))
       (let ((s3ed-test-directory ,dir))
         ,@body)
;       (delete-directory ,dir t)
       )))

(defmacro s3ed-setup-teardown-aws-test-dir (&rest body)
  (let ((aws-test-dir (make-symbol "aws-test-dir")))
    `(let ((,aws-test-dir "/tmp/s3ed-aws-testing"))
       (when (file-exists-p ,aws-test-dir) (delete-directory ,aws-test-dir t))
       ,@body
       (when (file-exists-p ,aws-test-dir) (delete-directory ,aws-test-dir t)))))

(ert-deftest s3ed-is-dired-active ()
  (should (not (s3ed-is-dired-active)))
  (dired "/")
  (should (s3ed-is-dired-active)))

(require 's3ed-io)

(ert-deftest s3ed-s3-ls-test ()
  (s3ed-setup-teardown-aws-test-dir
   (let* ((inhibit-message t)
          (res (s3ed-s3-ls "s3://s3ed/")))
     (should (equal res '("test/" "testdir/" "testfile"))))))

(ert-deftest s3ed-s3-mv-test ()
  (s3ed-setup-teardown-aws-test-dir
   (let* ((inhibit-message t))
     (s3ed-s3-mv "s3://s3ed/testdir/" "s3://s3ed/testdir-renamed/" t)
     (s3ed-s3-mv "s3://s3ed/testfile" "s3://s3ed/testfile-renamed")
     (should (equal (s3ed-s3-ls "s3://s3ed/") '("test/" "testdir-renamed/" "testfile-renamed"))))))

(ert-deftest s3ed-s3-cp-test ()
  (s3ed-setup-teardown-aws-test-dir
   (let* ((inhibit-message t))
     (s3ed-s3-cp "s3://s3ed/testdir/" "s3://s3ed/testdir-copied/" t)
     (s3ed-s3-cp "s3://s3ed/testfile" "s3://s3ed/testfile-copied")
     (should (equal (s3ed-s3-ls "s3://s3ed/") '("test/" "testdir-copied/" "testdir/"
                                                      "testfile" "testfile-copied"))))))

(ert-deftest s3ed-s3-rm-test ()
  (s3ed-setup-teardown-aws-test-dir
   (let* ((inhibit-message t))
     (s3ed-s3-rm "s3://s3ed/testdir/" t)
     (s3ed-s3-rm "s3://s3ed/testfile" t)
     (should (equal (s3ed-s3-ls "s3://s3ed/") '("test/"))))))

(ert-deftest s3ed-refresh-tmp-dir-test ()
  (s3ed-mode)
  (s3ed-setup-teardown-test-dir
   (let* ((inhibit-message t)
          (s3ed-tmp-s3-dir (substring s3ed-test-directory 0 -1))
          (s3ed-subdir (format "%s/s3ed/" (get-s3ed-tmp-s3-dir)))
          (s3ed-subdir2 (format "%s/another-bucket/" (get-s3ed-tmp-s3-dir))))
     (make-directory s3ed-subdir t)
     (make-directory s3ed-subdir2 t)
     (dired s3ed-subdir) ;; background s3ed buffer
     (dired s3ed-subdir2) ;; active buffer
     (s3ed-refresh-tmp-dir)

     ;; background buffer dir should exist, but not be refreshed
     (let ((organized-file-list (--separate (s3ed-is-directory
                                             (format "%s%s" s3ed-subdir it))
                                            (directory-files s3ed-subdir))))
       (should (eq 2 (length (car organized-file-list))))
       (should (eq 0 (length (car (-take-last 1 organized-file-list))))))

     ;; active buffer dir should be fully refreshed
     (let ((organized-file-list (--separate (s3ed-is-directory
                                             (format "%s%s" s3ed-subdir2 it))
                                            (directory-files s3ed-subdir2))))
       (should (and (member "dir-in-another-bucket" (car organized-file-list))
                    (member "dir2-in-another-bucket" (car organized-file-list))))
       (should (member "s3ed" (car (-take-last 1 organized-file-list))))))))

(ert-deftest s3ed-mkdirs-test ()
  (s3ed-setup-teardown-test-dir
   (let ((inhibit-message t))
     (s3ed-mkdirs `(,(format "%s/test1" s3ed-test-directory)
                       ,(format "%s/test2" s3ed-test-directory)
                       ,(format "%s/test3" s3ed-test-directory)))
     (should (and (member "test1" (directory-files s3ed-test-directory))
                  (member "test2" (directory-files s3ed-test-directory))
                  (member "test3" (directory-files s3ed-test-directory))))
     (should (and (s3ed-is-directory (format "%s/test1" s3ed-test-directory))
                  (s3ed-is-directory (format "%s/test2" s3ed-test-directory))
                  (s3ed-is-directory (format "%s/test3" s3ed-test-directory)))))))

(ert-deftest s3ed-create-empty-file-test ()
  (s3ed-setup-teardown-test-dir
   (let ((test-file (format "%s/test1" s3ed-test-directory)))
     (let ((inhibit-message t))
       (s3ed-create-empty-file test-file))
     (should (member "test1" (directory-files s3ed-test-directory)))
     (should (equal "" (with-temp-buffer
                         (insert-file-contents test-file)
                         (buffer-string)))))))

(ert-deftest s3ed-is-s3-path-test ()
  (should (s3ed-is-s3-path "s3://path/"))
  (should (not (s3ed-is-s3-path "/not/s3"))))

(ert-deftest s3ed-is-directory-test ()
  (s3ed-setup-teardown-test-dir
   (let ((directory-name (format "%stest-directory" s3ed-test-directory)))
     (message directory-name)
     (make-directory directory-name t)
     (should (s3ed-is-directory directory-name)))))

(ert-deftest s3ed-local-path-to-s3-path-test ()
  (should (equal (s3ed-local-path-to-s3-path (format "%s/local/path" (get-s3ed-tmp-s3-dir)))
                 "s3://local/path"))
  (should (not (s3ed-local-path-to-s3-path "typo/local/path"))))

(ert-deftest s3ed-s3-path-to-local-path-test ()
  (should (equal (s3ed-s3-path-to-local-path "s3://s3/path")
                 (format "%s/s3/path" (get-s3ed-tmp-s3-dir))))
  (should (not (s3ed-s3-path-to-local-path "s4://typo/local/path"))))

(ert-deftest s3ed-parent-directory-test ()
  (should (equal (s3ed-parent-directory "/parent/child")
                 "/parent/"))
  (should (equal (s3ed-parent-directory "/parent/child-dir/")
                 "/parent/")))

;;; s3ed-tests.el ends here
