;;; 0x0-upload.el --- Tests for 0x0.el -*- lexical-binding: t -*-

(require '0x0)
(require 'ert)

(defun 0x0--test-service (service)
  "Upload and retrieve a random string for SERVICE."
  (let ((rand (make-string 256 0)))
    (dotimes (i (length rand))
      (setf (aref rand i) (+ ?a (random (- ?z ?a)))))
    (let* ((resp (0x0-upload-string rand service))
           result)
      (with-current-buffer (url-retrieve-synchronously resp)
        (goto-char (point-min))
        (forward-paragraph)
        (delete-region (point-min) (1+ (point)))
        (setq result (buffer-string)))
      (should (or (equal rand result)
                  (save-match-data
                    (and (string-match "[[:space:]]*\\'" result)
                         (replace-match "" nil nil result))))))))

(dolist (service (mapcar #'car 0x0-services))
  (let ((name (intern (format "0x0-test-%s" (symbol-name service))))
        (name* (intern (format "0x0-test-%s-curl" (symbol-name service)))))
    (eval `(ert-deftest ,name ()
             (let ((0x0-use-curl-if-installed nil))
               (0x0--test-service ',service))))
    (eval `(ert-deftest ,name* ()
             (let ((0x0-use-curl-if-installed t))
               (0x0--test-service ',service))))))

(provide '0x0-tests)

;;; 0x0-tests.el ends her
