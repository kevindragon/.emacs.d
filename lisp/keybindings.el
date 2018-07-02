;;; keybindings.el
;; key bindings for myself


;; modify original key bindings
(global-set-key (kbd "C-h m")
                (lambda ()
                  (interactive)
                  (describe-mode)
                  (other-window 1)))

(global-set-key (kbd "C-h b")
                (lambda ()
                  (interactive)
                  (describe-bindings)
                  (other-window 1)))

(global-set-key (kbd "C-S-o")
                (lambda ()
                  (interactive)
                  (move-end-of-line 1)
                  (newline)
                  (indent-for-tab-command)))

;; ranger
(global-set-key (kbd "C-x M-m r") 'ranger)

;; magit key bindings
(global-set-key (kbd "C-x g") 'magit-status)

;;; window operations
;; window split horizontal and vertical toggle
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))
(global-set-key (kbd "C-x |") 'toggle-window-split)

;; maximum frame
(global-set-key (kbd "C-x w m") 'toggle-frame-maximized)

;; nxml
(add-hook 'nxml-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c M-/")
                           'comment-line)))
