;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun kj/insert-current-datetime ()
  (interactive)
  (let ((current-date-time-format "%Y %b %d %H:%M:%S %a %Z"))
    (insert (format-time-string current-date-time-format))))

(defun kj/php-ns-dir (file-abs-path)
  (file-name-nondirectory
   (directory-file-name (file-name-directory file-abs-path))))

(defun kj/php-get-namespace (file-abs-path root ns)
  (let ((dir (directory-file-name (file-name-directory file-abs-path))))
    (if (string-prefix-p root dir)
        (progn
          (kj/php-get-namespace (directory-file-name dir)
                             root
                             (cons (kj/php-ns-dir file-abs-path) ns)))
      ns)))

(defun kj/php-namespace ()
  (interactive)
  (when (and buffer-file-name (ignore-errors (projectile-project-root)))
    (mapconcat
     (lambda (x) (capitalize x))
     (kj/php-get-namespace buffer-file-name (projectile-project-root) '())
     "\\")))

(defun kj/php-class-name ()
  (interactive)
  (when buffer-file-name
    (file-name-sans-extension
     (file-name-nondirectory buffer-file-name))))

(defun kj/copy-buffer-file-name ()
  (interactive)
  (when buffer-file-name
    (kill-new buffer-file-name)))

(defun kj/copy-buffer-name ()
  (interactive)
  (kill-new (buffer-name)))

(require 's)

(defun kj/copy-project-file-name ()
  (interactive)
  (when buffer-file-name
    (kill-new
     (s-replace
      (or (ignore-errors (projectile-project-root)) "")
      "" buffer-file-name))))

;;;; web mode
(defun kj/web-mode-indent-to (num)
  (setq web-mode-markup-indent-offset num)
  (setq web-mode-css-indent-offset num)
  (setq web-mode-code-indent-offset num))
(defun kj/web-mode-indent-4 ()
  (interactive)
  (kj/web-mode-indent-to 4))
(defun kj/web-mode-indent-2 ()
  (interactive)
  (kj/web-mode-indent-to 2))


(defun uniq-lines (beg end)
  "Unique lines in region.
Called from a program, there are two arguments:
BEG and END (region to sort)."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (kill-line 1)
        (yank)
        (let ((next-line (point)))
          (while
              (re-search-forward
               (format "^%s" (regexp-quote (car kill-ring))) nil t)
            (replace-match "" nil nil))
          (goto-char next-line))))))

(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key (kbd "<M-S-up>") 'move-text-up)
(global-set-key (kbd "<M-S-down>") 'move-text-down)
