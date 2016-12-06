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

;; magit key bindings
(global-set-key (kbd "C-x g") 'magit-status)

;; fold
(global-set-key (kbd "C-c z c") 'vimish-fold)
(global-set-key (kbd "C-c z o") 'vimish-fold-unfold)

(global-set-key "\M-\C-g" 'org-plot/gnuplot)
