;;; log-mode.el --- Major mode for variant log format  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Kevin Jiang

;; Author: Kevin Jiang <wenlin1988@126.com>
;; Keywords: data

(require 'autorevert)

(defvar log-mode-hook nil
  "*Hook run when entering Log mode.")

(defface log-mode--font-lock-debug-face '((t (:foreground "Gray45")))
  "*Font Lock face used to highlight DEBUG log records.")
(defvar log-mode--font-lock-debug-face
  (make-face 'log-mode--font-lock-debug-face))

(defface log-mode--font-lock-info-face '((t (:foreground "ForestGreen")))
  "*Font Lock face used to highlight INFO log records.")
(defvar log-mode--font-lock-info-face
  (make-face 'log-mode--font-lock-info-face))

(defface log-mode--font-lock-config-face '((t (:foreground "ForestGreen")))
  "*Font Lock face used to highlight CONFIG log records.")
(defvar log-mode--font-lock-config-face
  (make-face 'log-mode--font-lock-config-face))

(defface log-mode--font-lock-warn-face '((t (:foreground "DodgerBlue")))
  "*Font Lock face used to highlight WARN log records.")
(defvar log-mode--font-lock-warn-face
  (make-face 'log-mode--font-lock-warn-face))

(defface log-mode--font-lock-error-face '((t (:foreground "Red")))
  "*Font Lock face used to highlight ERROR log records.")
(defvar log-mode--font-lock-error-face
  (make-face 'log-mode--font-lock-error-face))

(defface log-mode--font-lock-fatal-face '((t (:foreground "Red" :bold t)))
  "*Font Lock face used to highlight FATAL log records.")
(defvar log-mode--font-lock-fatal-face
  (make-face 'log-mode--font-lock-fatal-face))

(setq log-mode--highlights
      '(("\\bINFO\\b" . log-mode--font-lock-info-face)
        ("\\bDEBUG\\b" . log-mode--font-lock-debug-face)
        ("\\bERROR\\b" . log-mode--font-lock-error-face)
        ("\\bFATAL\\b" . log-mode--font-lock-fatal-face)
        ("\\bWARNING\\b" . log-mode--font-lock-warn-face)))

;;;###autoload
(define-derived-mode log-mode text-mode "Log"
  "Major mode for variant log format"
  (read-only-mode)
  (auto-revert-tail-mode)
  (set (make-local-variable 'font-lock-defaults) '(log-mode--highlights))
  (run-mode-hooks 'log-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\log\\'" . log-mode))

(provide 'log-mode)

;;; log-mode.el ends here
