;;; init Emacs
;; author: Kevin Jiang
;; E-mail: wenlin1988@126.com

;; setup my info
(setq user-full-name "Kevin Jiang")
(setq user-mail-address "wenlin1988@126.com")

;;; replace yes no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;;; coding system
;; (prefer-coding-system 'cp950)
;; (prefer-coding-system 'gb2312)
;; (prefer-coding-system 'cp936)
;; (prefer-coding-system 'gb18030)
;; (prefer-coding-system 'utf-16)
;; (prefer-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8-unix)

;;; set location on frame title
(defun frame-title-string ()
  "Return the file name of current buffer, using ~ if under home directory"
  (let ((fname (or (buffer-file-name (current-buffer)) (buffer-name)))
        (max-len 100))
    (when (string-match (getenv "HOME") fname)
      (setq fname (replace-match "~" t t fname)))
    (if (> (length fname) max-len)
        (setq fname (concat "..." (substring fname (- (length fname) max-len)))))
    fname))
(setq frame-title-format '("Kevin@"(:eval (frame-title-string))))

(setq make-backup-files nil)
(setq auto-save-default nil)

;;; no toolbar
(tool-bar-mode 0)

;;; no menu bar
(menu-bar-mode 0)

;; disable scroll bar
(scroll-bar-mode 0)

(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date nil)

;;(transient-mark-mode t)
(global-hl-line-mode 1)

;; remember cursor position, for emacs 25.1 or later
(save-place-mode 1)

;; set high kill ring
(setq-default kill-ring-max 200000)

;; tab size
(setq default-tab-width 4)
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;; Toggle visualization of matching parens
(show-paren-mode 1)
;; Toggle automatic parens pairing
(electric-pair-mode 1)

(setq select-enable-clipboard t)

;; display line and column number in the mode line
(line-number-mode 1)
(column-number-mode 1)

;; remove trailling whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; keep a list of recently opened files
(recentf-mode 1)

;; make ibuffer default
(defalias 'list-buffers 'ibuffer)

(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

(require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq package-check-signature nil)

;;; install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
;;; configure use-package
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-expand-minimally t)
(setq use-package-enable-imenu-support t)

;;; exec-path-from-shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns w32))
  :config
  (exec-path-from-shell-initialize))

;;; 基础包
(use-package websocket)
(use-package request)
(use-package request-deferred)
(use-package dash)
(use-package s)

(use-package dashboard
  :init
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((projects . 5)
                          (recents  . 20)
                          (agenda . 5)
                          (bookmarks . 5)
                          (registers . 5))))

(use-package spacemacs-theme
  :init (load-theme 'spacemacs-light t))

;;; 按使用习惯排序操作
(use-package smex)

;;; ivy
(use-package ivy
  :config (progn (ivy-mode 1)
                 (setq ivy-use-virtual-buffers t))
  :diminish (ivy-mode . ""))
(use-package swiper
  :bind ("C-s" . swiper))
(use-package counsel
  :bind (("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x)))

(use-package org
  :config
  (when (string-equal system-type "windows-nt")
    (setq org-directory "c:/workspace/orgs")
    (setq org-agenda-files '("c:/workspace/orgs/todo.org"))
    (setq org-default-notes-file (concat org-directory "/notes.org")))
  (setq org-log-done 'time))

;;; yasnippet
(use-package yasnippet
  :init (yas-global-mode 1))
(use-package yasnippet-snippets)

;;; auto insert
(defun autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))
(use-package autoinsert
  :init
  ;; Don't want to be prompted before insertion:
  (setq auto-insert-query nil)
  (setq auto-insert-directory (locate-user-emacs-file "templates"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)
  (yas-minor-mode-on)
  :config
  (define-auto-insert "\\.php$" ["template.php" autoinsert-yas-expand])
  (define-auto-insert "\\.py$" ["template.py" autoinsert-yas-expand])
  )

;;; ripgrep
(use-package ripgrep)
(use-package rg
  :config (rg-enable-default-bindings))

;;; company
(use-package company
  :init (setq company-dabbrev-downcase nil)
  :diminish " ⓒ"
  :config
  (setq company-minimum-prefix-length 2)
  (add-hook 'company-mode-hook 'company-quickhelp-mode))
(add-hook 'after-init-hook 'global-company-mode)

(use-package company-quickhelp)

(use-package window-numbering
  :init (add-hook 'after-init-hook 'window-numbering-mode t))

(use-package which-key
  :diminish which-key-mode
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :init (add-hook 'after-init-hook 'which-key-mode t))

(use-package origami
  :bind (("C-c z o" . origami-open-node)
         ("C-c z f" . origami-close-node)
         ("C-c z a" . origami-close-all-nodes)
         ("C-c z r" . origami-open-all-nodes))
  :config (global-origami-mode))

(use-package symbol-overlay
  :init (symbol-overlay-mode 1)
  :hook ((emacs-lisp-mode . symbol-overlay-mode))
  :bind (("C-c M-i" . symbol-overlay-put)
         ("C-c M-n" . symbol-overlay-switch-forward)
         ("C-c M-p" . symbol-overlay-switch-backward))
  :diminish " õ")

(use-package restclient
  :mode ("\\.http" . restclient-mode))

(use-package magit
  :bind ("M-n g" . magit-status)
  :config
  (setq magit-commit-show-diff nil
        truncate-lines nil))

(use-package projectile
  :diminish projectile-mode
  :init (projectile-mode +1)
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package treemacs
  :config
  (custom-set-variables '(treemacs-silent-refresh t)))
(use-package lsp-treemacs)

(use-package plantuml-mode
  :config
  (setq plantuml-jar-path "c:/software/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar))

(use-package yaml-mode)

(use-package xquery-mode)

;;; Python
(use-package python-pytest)
(use-package ein
  :defer t
  :config
  (require 'ein)
  (require 'ein-notebook)
  (require 'ein-subpackages))
;;; python lsp python microsoft
(require 'pyrepl-mode)
(defun my-python-hook ()
  (set (make-local-variable 'forward-sexp-function) nil)
  ;; ipython
  ;; (setq python-shell-interpreter "ipython"
  ;;       python-shell-interpreter-args "-i --simple-prompt --profile=dev"
  ;;       python-shell-interpreter-interactive-arg "-i --simple-prompt")
  ;; jupyter
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter"))
(use-package lsp-python-ms
  :hook ((python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp)
                          (flycheck-mode)
                          (setq-local flycheck-checker 'python-flake8)))
         (python-mode . symbol-overlay-mode)
         (python-mode . my-python-hook)
         ))

(defun kj/inferior-python-mode-hook ()
  (when (string-equal system-type "windows-nt")
    (set-terminal-coding-system 'gbk)
    (set-buffer-process-coding-system 'gbk 'gbk)))
(use-package python
  :hook
  (inferior-python-mode . kj/inferior-python-mode-hook)
  :config
  (set (make-local-variable 'forward-sexp-function) nil)
  (setq mode-name "ⓟ"))

(use-package flycheck
  ;; :init (global-flycheck-mode)
  )

;;; log-mode
(add-to-list 'load-path (concat user-emacs-directory "packages/log-mode"))
(require 'log-mode)

;;; web mode
(use-package web-mode
  :mode ("\\.tpl$" "\\.[agj]sp$" "\\.html?$" "\\.erb$" "\\.vue$")
  :init (progn
          (setq web-mode-markup-indent-offset 2)
          (setq web-mode-css-indent-offset 2)
          (setq web-mode-code-indent-offset 2)
          (setq web-mode-enable-current-column-highlight t))
  :hook ((web-mode . company-mode)
         (web-mode . (lambda ()
                       (flycheck-mode))))
  :config
  (setq web-mode-script-padding 0
        web-mode-style-padding 0))

(use-package vue-mode)

(use-package rust-mode)

(use-package lsp-mode
  :hook ((rust-mode web-mode js-mode typescript-mode) . lsp-deferred)
  :commands (lsp lsp-deferred)
  :config
  (lsp-signature-mode -1)
  (when (string= major-mode "python-mode")
    (setq lsp-prefer-flymake nil)))
(use-package company-lsp
  :init (setq company-lsp-cache-candidates 'auto))

(use-package lsp-ui
  :config
  (define-key lsp-ui-mode-map (kbd "C-c t d") #'lsp-ui-doc-mode)
  (define-key lsp-ui-mode-map (kbd "C-c d s") #'lsp-ui-doc-show)
  (define-key lsp-ui-mode-map (kbd "C-c d h") #'lsp-ui-doc-hide))

(use-package highlight-indentation
  :diminish ((highlight-indentation-mode . "")))

;; mode line mode names settings
(use-package diminish
  :diminish ((abbrev-mode . " A")
             (auto-revert-mode . "")))

;;set transparent effect
(setq alpha-list '((100 100) (95 65) (85 55) (75 45) (65 35)))
(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       (message "alpha is %s" a))
     (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))))
(global-set-key [(f11)] 'loop-alpha)

(load "myfuns.el")
(load "keybindings.el")
(load "randomize-region.el")

;;; custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-python-ms-cache "Library")
 '(lsp-python-ms-log-level "Trace")
 '(lsp-signature-render-documentation nil)
 '(lsp-ui-doc-alignment 'frame)
 '(lsp-ui-doc-delay 0.2)
 '(lsp-ui-doc-position 'at-point)
 '(package-selected-packages
   '(dashboard inferior-python inferior-python-mode lsp-treemacs spacemacs-theme origami rust-mode tide js2-refactor web-mode mmm-mode vue-mode typescript-mode flycheck ripgrep lsp-python-ms rg magit yaml-mode symbol-overlay which-key window-numbering exec-path-from-shell use-package))
 '(send-mail-function 'mailclient-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "light slate gray"))))
 '(fringe ((t (:background "ghost white"))))
 '(mmm-default-submode-face ((t (:background "white"))))
 '(window-divider ((t (:foreground "gray30")))))
