;;; 0x0-test.el --- Tests for 0x0.el -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

(require '0x0)
(require 'ert)
(require 'cl-macs)

(defun 0x0--test-service (server)
  "Upload and retrieve a random string for SERVER."
  (let ((rand (make-string 256 0)))
    (dotimes (i (length rand))
      (setf (aref rand i) (+ ?a (random (- ?z ?a)))))
    (with-temp-buffer
      (insert rand)
      (let* ((resp (0x0-upload-text server))
             result)
        (with-current-buffer (url-retrieve-synchronously resp)
          (goto-char (point-min))
          (forward-paragraph)
          (delete-region (point-min) (1+ (point)))
          (setq result (buffer-string)))
        (should (or (equal rand result)
                    (save-match-data
                      (and (string-match "[[:space:]]*\\'" result)
                           (replace-match "" nil nil result)))))))))

;;; Integration tests that actually call the services.
(dolist (server-key (mapcar #'car 0x0-servers))
  (let* ((server (cdr (assq server-key 0x0-servers)))
         (name (intern (format "0x0-test-%s-curl" server-key))))
    (ert-set-test name
                  (make-ert-test
                   :name name
                   :tags '(:integration)
                   :body (lambda () (0x0--test-service server))))))

;; Developer unit tests
(ert-deftest 0x0-unit-calculate-timeout ()
  "should calculate an estimated file timeout based on file size."
  (let ((server (list :min-age 30 :max-age 365 :max-size (* 1024 1024 512))))
    (should (equal (0x0--calculate-timeout server (* 1024 1024 512)) 30.0))
    (should (equal (round (0x0--calculate-timeout server 1)) 365))
    (should (equal (0x0--calculate-timeout server 268435456) 71.875))))

(ert-deftest 0x0-unit-send-fallback-to-url ()
  (cl-letf (((symbol-function 'executable-find) (lambda (cmd)
                                                  (should (equal cmd "curl"))
                                                  (ert-fail "Unexpected call to executable-find")))
            ((symbol-function '0x0--url) (lambda (_p _b)
                                           "success!"))
            ((symbol-function '0x0--curl) (lambda (_ca _b)
                                            (ert-fail '("unexpected call to 0x0--curl")))))
    (let* ((0x0-use-curl nil)
           (server (list
                    :scheme "https"
                    :host "example.com"
                    :curl-args-fun #'ignore))
           (bounds (list :start 10 :end 20)))
      (should (equal
               (0x0--send server "upload.txt" bounds)
               "success!")))))

(ert-deftest 0x0-unit-use-curl-if-installed ()
  (cl-letf (((symbol-function 'executable-find) (lambda (cmd)
                                                  (should (equal cmd "curl"))))
            ((symbol-function '0x0--url) (lambda (_ca _b)
                                           (ert-fail '("unexpected call to 0x0--url"))))
            ((symbol-function '0x0--curl) (lambda (curl-args _b)
                                            (should (equal curl-args
                                                           '("-s" "-S" "-F"
                                                             "file=@-;filename=upload.txt"
                                                             "https://example.com")))
                                            "success!")))
    (let* ((0x0-use-curl 'if-installed)
           (server (list
                    :scheme "https"
                    :host "example.com"
                    :curl-args-fun #'0x0--make-0x0-curl-args))
           (bounds (list :start 10 :end 20)))
      (should (equal
               (0x0--send server "upload.txt" bounds)
               "success!")))))

(provide '0x0-test)
;;; 0x0-test.el ends here
