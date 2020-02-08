;;; 0x0.el --- Upload to 0x0.st -*- lexical-binding: t -*-

;; Author: Philip K. <philip@warpmail.net>
;; Version: 0.1.1
;; Keywords: comm
;; Package-Requires: ((emacs "24.1"))
;; URL: https://git.sr.ht/~zge/nullpointer-emacs

;; This file is NOT part of Emacs.
;;
;; This file is in the public domain, to the extent possible under law,
;; published under the CC0 1.0 Universal license.
;;
;; For a full copy of the CC0 license see
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

;;; Commentary:
;;
;; This package defines three main functions: `0x0-upload',
;; `0x0-upload-file' and `0x0-upload-string', which respectively upload
;; (parts of) the current buffer, a file on your disk and a string from
;; the minibuffer to a 0x0.st comparable server.
;;
;; Besides the built-in `url' library, this package has no dependencies,
;; especially no external ones, such as curl.
;;
;; See https://0x0.st/ for more details, and consider donating to
;; https://liberapay.com/lachs0r/donate if you like the service.

(require 'url)

;;; Code:

(defgroup 0x0 nil
  "Upload data to 0x0.st-compatible servers."
  :group 'convenience
  :prefix "0x0-")

(defcustom 0x0-services
  `((0x0
     :host "0x0.st"
     :query "file"
     :min-age 30
     :max-age 365
     :max-size ,(* 1024 1024 512))
    (ttm
     :host "ttm.sh"
     :query "file"
     :min-age 30
     :max-age 365
     :max-size ,(* 1024 1024 256)))
  "Alist of different 0x0-like services.

The car is a symbol identifying the service, the cdr a plist,
with the following keys:

    :host		- domain name of the server (string, necessary)
    :path		- server path to send POST request to (string,
				  optional)
    :no-tls		- is tls not supported (bool, nil by default)
    :query		- query string used for file upload (string,
				  necessary)
    :min-age	- on 0x0-like servers, minimal number of days
				  a file is kept online (number, optional)
    :max-age	- on 0x0-like servers, maximal number of days
				  a file is kept online (number, optional)
    :max-size	- file limit for this server (number, optional)

This variable only describes servers, but doesn't set anything.
See `0x0-default-host' if you want to change the server you use."
  :type '(alist :key-type symbol
                :value-type (plist :value-type sexp)))

(defcustom 0x0-default-host '0x0
  "Symbol describing server to use.

The symbol must be a key from the alist `0x0-services'."
  :type `(choice ,@(mapcar #'car 0x0-services)))

(defcustom 0x0-use-curl-if-installed t
  "Automatically check if curl is installed."
  :type 'boolean)

(defvar 0x0--filename nil)
(defvar 0x0--use-file nil)
(defvar 0x0--server nil)

(defun 0x0--calculate-timeout (size)
  "Calculate days a file of size SIZE would last."
  (condition-case nil
      (let ((min-age (float (plist-get 0x0--server :min-age)))
            (max-age (float (plist-get 0x0--server :max-age)))
            (max-size (float (plist-get 0x0--server :max-size))))
        (+ min-age (* (- min-age max-age)
                      (expt (- (/ size max-size) 1.0) 3))))
    (wrong-type-argument nil)))

(defun 0x0--use-curl (start end)
  "Backend function for uploading using curl.

Operate on region between START and END."
  (let ((buf (generate-new-buffer "*0x0 response*")))
    (call-process-region start end "curl"
                         nil buf nil
                         "-s" "-S" "-F"
                         (format (if 0x0--use-file
                                     "%s=@%s"
                                   "%s=@-;filename=%s")
                                 (plist-get 0x0--server :query)
                                 0x0--filename)
                         (plist-get 0x0--server :host))
    buf))

(defun 0x0--use-url (start end)
  "Backend function for uploading using `url' functions.

Operate on region between START and END."
  (let* ((boundary (format "%X-%X-%X" (random) (random) (random)))
         (url-request-extra-headers
          `(("Content-Type" . ,(concat "multipart/form-data; boundary=" boundary))))
         (url-request-data
          (let ((source (current-buffer))
                (filename (or 0x0--filename (buffer-name))))
            (with-temp-buffer
              (insert "--" boundary "\r\n")
              (insert "Content-Disposition: form-data; ")
              (insert "name=\"" (plist-get 0x0--server :query)
                      "\"; filename=\"" filename "\"\r\n")
              (insert "Content-type: text/plain\r\n\r\n")
              (insert-buffer-substring-no-properties source start end)
              (insert "\r\n--" boundary "--")
              (buffer-string))))
         (url-request-method "POST"))
    (with-current-buffer
        (url-retrieve-synchronously
         (concat (if (plist-get 0x0--server :no-tls)
                     "http" "https")
                 "://" (plist-get 0x0--server :host)))
      (goto-char (point-min))
      (save-match-data
        (when (search-forward-regexp "^[[:space:]]*$" nil t)
          (delete-region (point-min) (match-end 0))))
      (current-buffer))))

(defun 0x0--choose-service ()
  (if current-prefix-arg
      (intern (completing-read "Service: "
                               (mapcar #'car 0x0-services)
                               nil t nil nil
                               0x0-default-host))
    0x0-default-host))

;;;###autoload
(defun 0x0-upload (start end service)
  "Upload current buffer to `0x0-url' from START to END.

If START and END are not specified, upload entire buffer."
  (interactive (list (if (use-region-p) (region-beginning) (point-min))
                     (if (use-region-p) (region-end) (point-max))
                     (0x0--choose-service)))
  (let ((0x0--server (cdr (assq service 0x0-services))))
    (unless 0x0--server
      (error "Service %s unknown." service))
    (unless (plist-get 0x0--server :host)
      (error "Service %s has no :host field" service))
    (unless (plist-get 0x0--server :query)
      (error "Service %s has no :query field" service))
    (let ((resp (if (and 0x0-use-curl-if-installed
                         (executable-find "curl"))
                    (0x0--use-curl start end)
                  (0x0--use-url start end)))
          (timeout (0x0--calculate-timeout (- end start))))
      (with-current-buffer resp
        (goto-char (point-min))
        (save-match-data
          (unless (search-forward-regexp
                   (concat "^"
                           (regexp-opt '("http" "https"))
                           "://"
                           (regexp-quote (plist-get 0x0--server :host))
                           ".*$")
                   nil t)
            (error "Failed to upload/parse. See %s for more details"
                   (buffer-name resp)))
          (kill-new (match-string 0))
          (message (concat (format "Yanked `%s' into kill ring." (match-string 0) )
                           (and timeout (format " Should last ~%2g days." timeout))))))
      (kill-buffer resp))))

;;;###autoload
(defun 0x0-upload-file (file service)
  "Upload FILE to `0x0-url'."
  (interactive (list (read-file-name "Upload file: ")
                     (0x0--choose-service)))
  (with-temp-buffer
    (unless 0x0-use-curl-if-installed
      (insert-file-contents file))
    (let ((0x0--filename file)
          (0x0--use-file t))
      (0x0-upload (point-min) (point-max) service))))

;;;###autoload
(defun 0x0-upload-string (string service)
  "Upload STRING to `0x0-url'."
  (interactive (list (read-string "Upload string: ")
                     (0x0--choose-service)))
  (with-temp-buffer
    (insert string)
    (let ((0x0--filename "upload.txt"))
      (0x0-upload (point-min) (point-max) service))))

(provide '0x0)

;;; 0x0.el ends here
