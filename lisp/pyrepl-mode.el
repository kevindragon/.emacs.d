;;; pyrepl.el --- Python repl programming environment  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Kevin Jiang

;; Author: Kevin Jiang <wenlin1988@126.com>
;; Keywords: Python, repl
;; URL: https://kevinjiang.info

(require 'python)
(require 'elpy-shell)


;;;;;;;;;;;;;;
;;; Navigation

(defvar elpy-nav-expand--initial-position nil
  "Initial position before expanding to indentation.")
(make-variable-buffer-local 'elpy-nav-expand--initial-position)

(defun elpy-goto-definition ()
  "Go to the definition of the symbol at point, if found."
  (interactive)
  (let ((location (elpy-rpc-get-definition)))
    (if location
        (elpy-goto-location (car location) (cadr location))
      (error "No definition found"))))

(defun elpy-goto-assignment ()
  "Go to the assignment of the symbol at point, if found."
  (interactive)
  (let ((location (elpy-rpc-get-assignment)))
    (if location
        (elpy-goto-location (car location) (cadr location))
      (error "No assignment found"))))

(defun elpy-goto-definition-other-window ()
  "Go to the definition of the symbol at point in other window, if found."
  (interactive)
  (let ((location (elpy-rpc-get-definition)))
    (if location
        (elpy-goto-location (car location) (cadr location) 'other-window)
      (error "No definition found"))))

(defun elpy-goto-assignment-other-window ()
  "Go to the assignment of the symbol at point in other window, if found."
  (interactive)
  (let ((location (elpy-rpc-get-assignment)))
    (if location
        (elpy-goto-location (car location) (cadr location) 'other-window)
      (error "No assignment found"))))

(defun elpy-goto-location (filename offset &optional other-window-p)
  "Show FILENAME at OFFSET to the user.

If OTHER-WINDOW-P is non-nil, show the same in other window."
  (ring-insert find-tag-marker-ring (point-marker))
  (let ((buffer (find-file-noselect filename)))
    (if other-window-p
        (pop-to-buffer buffer t)
      (switch-to-buffer buffer))
    (goto-char (1+ offset))
    (recenter 0)))

(defun elpy-nav-forward-block ()
  "Move to the next line indented like point.

This will skip over lines and statements with different
indentation levels."
  (interactive "^")
  (let ((indent (current-column))
        (start (point))
        (cur nil))
    (when (/= (% indent python-indent-offset)
              0)
      (setq indent (* (1+ (/ indent python-indent-offset))
                      python-indent-offset)))
    (python-nav-forward-statement)
    (while (and (< indent (current-indentation))
                (not (eobp)))
      (when (equal (point) cur)
        (error "Statement does not finish"))
      (setq cur (point))
      (python-nav-forward-statement))
    (when (< (current-indentation)
             indent)
      (goto-char start))))

(defun elpy-nav-backward-block ()
  "Move to the previous line indented like point.

This will skip over lines and statements with different
indentation levels."
  (interactive "^")
  (let ((indent (current-column))
        (start (point))
        (cur nil))
    (when (/= (% indent python-indent-offset)
              0)
      (setq indent (* (1+ (/ indent python-indent-offset))
                      python-indent-offset)))
    (python-nav-backward-statement)
    (while (and (< indent (current-indentation))
                (not (bobp)))
      (when (equal (point) cur)
        (error "Statement does not start"))
      (setq cur (point))
      (python-nav-backward-statement))
    (when (< (current-indentation)
             indent)
      (goto-char start))))

(defun elpy-nav-forward-indent ()
  "Move forward to the next indent level, or over the next word."
  (interactive "^")
  (if (< (current-column) (current-indentation))
      (let* ((current (current-column))
             (next (* (1+ (/ current python-indent-offset))
                      python-indent-offset)))
        (goto-char (+ (point-at-bol)
                      next)))
    (let ((eol (point-at-eol)))
      (forward-word)
      (when (> (point) eol)
        (goto-char (point-at-bol))))))

(defun elpy-nav-backward-indent ()
  "Move backward to the previous indent level, or over the previous word."
  (interactive "^")
  (if (and (<= (current-column) (current-indentation))
           (/= (current-column) 0))
      (let* ((current (current-column))
             (next (* (1- (/ current python-indent-offset))
                      python-indent-offset)))
        (goto-char (+ (point-at-bol)
                      next)))
    (backward-word)))

(defun elpy-nav-move-line-or-region-down (&optional beg end)
  "Move the current line or active region down."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (if beg
      (elpy--nav-move-region-vertically beg end 1)
    (elpy--nav-move-line-vertically 1)))

(defun elpy-nav-move-line-or-region-up (&optional beg end)
  "Move the current line or active region down."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (if beg
      (elpy--nav-move-region-vertically beg end -1)
    (elpy--nav-move-line-vertically -1)))

(defun elpy--nav-move-line-vertically (dir)
  "Move the current line vertically in direction DIR."
  (let* ((beg (point-at-bol))
         (end (point-at-bol 2))
         (col (current-column))
         (region (delete-and-extract-region beg end)))
    (forward-line dir)
    (save-excursion
      (insert region))
    (goto-char (+ (point) col))))

(defun elpy--nav-move-region-vertically (beg end dir)
  "Move the current region vertically in direction DIR."
  (let* ((point-before-mark (< (point) (mark)))
         (beg (save-excursion
                (goto-char beg)
                (point-at-bol)))
         (end (save-excursion
                (goto-char end)
                (if (bolp)
                    (point)
                  (point-at-bol 2))))
         (region (delete-and-extract-region beg end)))
    (goto-char beg)
    (forward-line dir)
    (save-excursion
      (insert region))
    (if point-before-mark
        (set-mark (+ (point)
                     (length region)))
      (set-mark (point))
      (goto-char (+ (point)
                    (length region))))
    (setq deactivate-mark nil)))

(defun elpy-open-and-indent-line-below ()
  "Open a line below the current one, move there, and indent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun elpy-open-and-indent-line-above ()
  "Open a line above the current one, move there, and indent."
  (interactive)
  (move-beginning-of-line 1)
  (save-excursion
    (insert "\n"))
  (indent-according-to-mode))

(defun elpy-nav-expand-to-indentation ()
  "Select surrounding lines with current indentation."
  (interactive)
  (setq elpy-nav-expand--initial-position (point))
  (let ((indentation (current-indentation)))
    (if (= indentation 0)
        (progn
          (push-mark (point))
          (push-mark (point-max) nil t)
          (goto-char (point-min)))
      (while (<= indentation (current-indentation))
        (forward-line -1))
      (forward-line 1)
      (push-mark (point) nil t)
      (while (<= indentation (current-indentation))
        (forward-line 1))
      (backward-char))))

(defadvice keyboard-quit (before collapse-region activate)
  "Abort elpy selection by indentation on quit."
  (when (eq last-command 'elpy-nav-expand-to-indentation)
    (goto-char elpy-nav-expand--initial-position)))

(defun elpy-nav-normalize-region ()
  "If the first or last line are not fully selected, select them completely."
  (let ((beg (region-beginning))
        (end (region-end)))
    (goto-char beg)
    (beginning-of-line)
    (push-mark (point) nil t)
    (goto-char end)
    (unless (= (point) (line-beginning-position))
      (end-of-line))))

(defun elpy-nav-indent-shift-right (&optional _count)
  "Shift current line by COUNT columns to the right.

COUNT defaults to `python-indent-offset'.
If region is active, normalize the region and shift."
  (interactive)
  (if (use-region-p)
      (progn
        (elpy-nav-normalize-region)
        (python-indent-shift-right (region-beginning) (region-end) current-prefix-arg))
    (python-indent-shift-right (line-beginning-position) (line-end-position) current-prefix-arg)))

(defun elpy-nav-indent-shift-left (&optional _count)
  "Shift current line by COUNT columns to the left.

COUNT defaults to `python-indent-offset'.
If region is active, normalize the region and shift."
  (interactive)
  (if (use-region-p)
      (progn
        (elpy-nav-normalize-region)
        (python-indent-shift-left (region-beginning) (region-end) current-prefix-arg))
    (python-indent-shift-left (line-beginning-position) (line-end-position) current-prefix-arg)))


;;;###autoload
(define-minor-mode pyrepl-mode
  "docstring"
  :lighter " p>"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'elpy-shell-send-region-or-buffer)
    (define-key map (kbd "C-c C-z") 'elpy-shell-switch-to-shell)
    (define-key map (kbd "C-c C-k") 'elpy-shell-kill)
    (define-key map (kbd "C-c C-K") 'elpy-shell-kill-all)
    (define-key map (kbd "<C-return>") 'elpy-shell-send-statement-and-step)
    (define-key inferior-python-mode-map (kbd "C-c C-z") 'elpy-shell-switch-to-buffer)
    map))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --profile=dev"
      python-shell-interpreter-interactive-arg "-i --simple-prompt")
(set (make-local-variable 'forward-sexp-function) nil)

;;;###autoload
(add-hook 'python-mode-hook 'pyrepl-mode)
(add-hook 'inferior-python-mode 'pyrepl-mode)

(provide 'pyrepl-mode)
;;; pyrepl-mode.el ends here
