;;; init Emacs
;; author: Kevin Jiang
;; E-mail: wenlin1988@126.com

(setq inhibit-startup-message t)

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

(require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

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

;;; 按使用习惯排序操作
(use-package smex)

;;; helm
;; (use-package helm
;;   :init (helm-mode 1)
;;   :config
;;   (global-set-key (kbd "M-x") 'helm-M-x)
;;   (global-set-key (kbd "C-x C-f") #'helm-find-files))

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

;;; company
(use-package company
  :init (setq company-dabbrev-downcase nil)
  :diminish " ⓒ"
  :config
  (setq company-minimum-prefix-length 2)
  (add-hook 'company-mode-hook 'company-quickhelp-mode))
(add-hook 'after-init-hook 'global-company-mode)

(use-package company-quickhelp)

;; mode line mode names settings
(use-package diminish
  :diminish ((abbrev-mode . " A")
             (auto-revert-mode . "")))

(use-package window-numbering
  :init (add-hook 'after-init-hook 'window-numbering-mode t))

(use-package which-key
  :diminish which-key-mode
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :init (add-hook 'after-init-hook 'which-key-mode t))

(use-package symbol-overlay
  :init (symbol-overlay-mode 1)
  :bind (("C-c M-i" . symbol-overlay-put)
         ("C-c M-n" . symbol-overlay-switch-forward)
         ("C-c M-p" . symbol-overlay-switch-backward))
  :diminish " õ")

(use-package restclient
  :mode ("\\.http" . restclient-mode))

(use-package treemacs
  :config
  (custom-set-variables '(treemacs-silent-refresh t)))

(use-package plantuml-mode
  :config
  (setq plantuml-jar-path "c:/software/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar))

(use-package yaml-mode)

(use-package xquery-mode)

;;; Python
(use-package elpy
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt --profile=dev"
        python-shell-interpreter-interactive-arg "-i --simple-prompt")
  (add-hook 'python-mode-hook (lambda() (setq mode-name "ⓟ")))
  :diminish ((elpy-mode . "Ẽ")
             (highlight-indentation-mode . "")))
(use-package py-autopep8
  :hook elpy-mode-hook)
(use-package python-pytest)
(use-package ein
  :defer t
  :config
  (require 'ein)
  (require 'ein-notebook)
  (require 'ein-subpackages))

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

;;; set load path
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))
(load "myfuns.el")
(load "keybindings.el")
(load "randomize-region.el")

;;; custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(elpy yaml-mode symbol-overlay which-key window-numbering helm exec-path-from-shell use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "light slate gray"))))
 '(fringe ((t (:background "ghost white"))))
 '(window-divider ((t (:foreground "gray30")))))
