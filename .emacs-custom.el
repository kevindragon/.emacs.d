;;; .emacs-custom.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Kevin Jiang

;; Author: Kevin Jiang <wenlin1988@126.com>
;; Keywords: abbrev, abbrev, abbrev,
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe-mode nil nil (fringe))
 '(package-selected-packages
   '(flycheck-mypy zeal-at-point yaml-mode xquery-mode window-numbering which-key vue-mode use-package spacemacs-theme smex ripgrep ranger rainbow-delimiters quickrun python-pytest py-autopep8 powerline phpunit php-mode origami mode-icons markdown-preview-mode lsp-vue logview keyfreq js2-refactor htmlize hexo go-imports go-eldoc go-autocomplete flycheck-pycheckers expand-region exec-path-from-shell ensime dockerfile-mode diminish csharp-mode company-quickhelp company-lsp company-go clj-refactor ag))
 '(treemacs-silent-refresh t)
 '(window-divider-default-bottom-width 1)
 '(window-divider-default-places t)
 '(window-divider-default-right-width 1)
 '(window-divider-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-fringe-info ((t (:inherit success))))
 '(font-lock-comment-face ((t (:foreground "light slate gray"))))
 '(fringe ((t (:background "ghost white"))))
 '(window-divider ((t (:foreground "gray60")))))
