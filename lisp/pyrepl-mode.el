;;; pyrepl.el --- Python repl programming environment  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Kevin Jiang

;; Author: Kevin Jiang <wenlin1988@126.com>
;; Keywords: Python, repl
;; URL: https://kevinjiang.info

(require 'python)

(defvar pyrepl--shell-last-py-buffer nil
  "Help keep track of python buffer when changing to pyshell.")

(defun pyrepl--get-or-create-process (&optional sit)
  "Get or create an inferior Python process for current buffer and return it.

If SIT is non-nil, sit for that many seconds after creating a
Python process. This allows the process to start up."
  (let* ((bufname (format "*%s*" (python-shell-get-process-name nil)))
         (proc (get-buffer-process bufname)))
    (if proc
        proc
      (unless (executable-find python-shell-interpreter)
        (error "Python shell interpreter `%s' cannot be found. Please set `python-shell-interpreter' to a valid python binary."
               python-shell-interpreter))
      (let ((default-directory (projectile-project-root)))
        (run-python (python-shell-parse-command) nil t)
        (python-shell-send-string
         (format "import sys;sys.path.append('%s')" default-directory)))
      (when sit (sit-for sit))
      ;; (when (elpy-project-root)
      ;;   (python-shell-send-string
      ;;    (format "import sys;sys.path.append('%s')" (elpy-project-root))))
      (get-buffer-process bufname))))

(defun pyrepl-switch-to-shell ()
  (interactive)
  (setq pyrepl--shell-last-py-buffer (buffer-name))
  (pop-to-buffer (process-buffer (pyrepl--get-or-create-process))))

(defun pyrepl-send-region-or-buffer-and-step (&optional arg)
  (interactive "P")
  (python-shell-send-buffer arg))

;;; buffer-local的一个例子
;; (make-variable-buffer-local
;;  (defvar foo-count 0
;;    "Number of foos inserted into the current buffer."))
;; (defun insert-foo ()
;;   (interactive)
;;   (setq foo-count (1+ foo-count))
;;   (insert "foo"))

;;;###autoload
(define-minor-mode pyrepl-mode
  "docstring"
  :lighter " p>"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-z") 'pyrepl-switch-to-shell)
            map))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --profile=dev"
      python-shell-interpreter-interactive-arg "-i --simple-prompt")
(set (make-local-variable 'forward-sexp-function) nil)

;;;###autoload
(add-hook 'python-mode-hook 'pyrepl-mode)

(provide 'pyrepl-mode)
;;; pyrepl.el ends here
