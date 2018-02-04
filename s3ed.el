;;; s3ed.el --- Tramp-like access to s3

;; Author: Matt Usifer <mattusifer@gmail.com>
;; Version: 0.1.0
;; Keywords: s3 tools
;; Package-Requires: ((emacs "24.4") (seq) (dash))
;; Homepage: https://github.com/mattusifer/s3ed

;; S3ed is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; S3ed is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; S3ed provides an interface to Amazon S3 from within Emacs.  S3ed
;; is inspired by TRAMP, and strives to provide near-seamless access to
;; S3 from standard Emacs functions.

;;; Code:

(require 's3ed-mode)

;; define the two entry points to s3ed - s3ed-find-file and s3ed-save-file

(defun s3ed-find-file ()
  "Open s3ed buffer at input-file.
Will be a refreshed dired buffer if it is a directory."
  (interactive)
  (if s3ed-mode
      (let* ((current-s3-base-path (if (s3ed-is-active)
                                       (s3ed-local-path-to-s3-path default-directory)
                                     "s3://"))
             (current-s3-file-path (s3ed-completing-read current-s3-base-path
                                                            "Find S3 file"))
             (current-local-file-path (s3ed-s3-path-to-local-path current-s3-file-path)))
        (if (s3ed-is-directory current-s3-file-path)
            (progn
              (s3ed-refresh-tmp-dir current-local-file-path)
              (dired current-local-file-path))
          (progn
            (s3ed-s3-cp (s3ed-local-path-to-s3-path current-local-file-path)
                           current-local-file-path)
            (find-file current-local-file-path))))
    (when (y-or-n-p "S3ed mode is disabled, do you want to enable s3ed? ")
      (s3ed-mode)
      (s3ed-find-file))))

(defun s3ed-save-file ()
  "Save input file to s3."
  (interactive)
  (if s3ed-mode
      (let* ((current-s3-base-path (if (s3ed-is-active)
                                       (s3ed-local-path-to-s3-path default-directory)
                                     "s3://"))
             (current-s3-file-path (s3ed-completing-read current-s3-base-path "Save S3 file"))
             (current-local-file-path (s3ed-s3-path-to-local-path current-s3-file-path)))
        (write-file current-local-file-path)
        (s3ed-s3-cp current-local-file-path current-s3-file-path)
        (s3ed-refresh-tmp-dir)
        (find-file current-local-file-path))
    (when (y-or-n-p "S3ed mode is disabled, do you want to enable s3ed? ")
      (s3ed-mode)
      (s3ed-save-file))))

(provide 's3ed)

;;; s3ed.el ends here
