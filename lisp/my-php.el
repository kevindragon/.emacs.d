
(require 'cl-lib)
(require 'json)

(defvar kj/php-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory where all of the example files are located.")

(defvar kj/php-completions-cache nil)

(defun kj/php-completions ()
  (interactive)
  (message "[%s] start call kj/php-completions"
           (current-date-time-format "%Y %b %d %H:%M:%S %a %Z"))
  (when (null kj/php-completions-cache)
    (message "Extracting classes and functions ...")
    (let ((obj (json-read-file (concat kj/php-directory "php_manual_en.json"))))
      (setq kj/php-completions-cache
            (mapcar (lambda (o) (symbol-name (car o))) obj))))
  (message "[%s] end call kj/php-completions"
           (current-date-time-format "%Y %b %d %H:%M:%S %a %Z"))
  kj/php-completions-cache)

(defun kj/php-company-prefix-handle ()
  (when (eq major-mode 'php-mode)
    (let* ((pfx (or (company-grab "$[a-zA-Z]+")
                    (company-grab-symbol)))
           (cdts (cl-remove-if-not (lambda (c) (string-prefix-p pfx c))
                                   (kj/php-completions))))
      (message (format "prefix: %s" pfx))
      (if cdts pfx nil))))

(defun kj/company-php-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'kj/company-php-backend))
    (prefix (kj/php-company-prefix-handle))
    (candidates (cl-remove-if-not (lambda (c) (string-prefix-p arg c))
                                  (kj/php-completions)))))

(provide 'my-php)
