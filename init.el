;;; init Emacs
;; author: Kevin Jiang
;; E-mail: wenlin1988@126.com

(setq inhibit-startup-message t)

;; setup my info
(setq user-full-name "Kevin Jiang")
(setq user-mail-address "wenlin1988@126.com")

(setq gc-cons-threshold-old gc-cons-threshold)
(setq gc-cons-threshold 1073741824)

;; set load path
(add-to-list 'load-path "~/.emacs.d/lisp")

(setq custom-file "~/.emacs.d/.emacs-custom.el")

;; set location on frame title
(defun frame-title-string ()
  "Return the file name of current buffer, using ~ if under home directory"
  (let ((fname (or
                (buffer-file-name (current-buffer))
                (buffer-name)))
        (max-len 100))
    (when (string-match (getenv "HOME") fname)
      (setq fname (replace-match "~" t t fname)))
    (if (> (length fname) max-len)
        (setq fname
              (concat "..."
                      (substring fname (- (length fname) max-len)))))
    fname))
(setq frame-title-format '("Kevin@"(:eval (frame-title-string))))

;; set font
(when (member "Courier New" (font-family-list))
  (set-face-attribute 'default nil :font "Courier New"))
(when (string-equal system-type "gnu/linux")
  (set-default-font "Monospace-10"))

;; set ckj font windows下有中文的内容很慢，用下面完美解决
(when (string-equal system-type "windows-nt")
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "宋体" :size 12)))
  ;; (set-face-attribute
  ;;  'default nil
  ;;  :font (font-spec :name "-outline-Courier New-bold-italic-normal-mono-*-*-*-*-c-*-iso10646-1"
  ;;                   :weight 'normal
  ;;                   :slant 'normal
  ;;                   :size 9.0))
  ;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
  ;;   (set-fontset-font
  ;;    (frame-parameter nil 'font)
  ;;    charset
  ;;    (font-spec :name "-outline-微软雅黑-normal-normal-normal-sans-*-*-*-*-p-*-iso10646-1"
  ;;               :weight 'normal
  ;;               :slant 'normal
  ;;               :size 10.5)))
  (let ((mypaths
         '("C:/Program Files/Git/bin"
           "C:/Program Files (x86)/Mozilla Firefox/"
           "C:/Program Files (x86)/Google/Chrome/Application"
           "C:/Program Files/Oracle/VirtualBox"
           "C:/Users/jiangkx/AppData/Local/Continuum/anaconda3"
           "C:/Users/jiangkx/AppData/Local/Continuum/anaconda3/Scripts"
           "C:/Program Files/Zeal"
           "C:/Users/jiangkx/AppData/Roaming/Composer/vendor/bin"
           "C:/msys64/usr/bin"
           ;; "C:/msys64/mingw64/bin"
           )))
    (setenv "PATH" (concat
                    (mapconcat 'identity mypaths ";") ";"
                    (getenv "PATH")))
    (setq exec-path (append
                     mypaths
                     (split-string (getenv "PATH") ";")
                     (list "." exec-directory))))
  (setq tramp-default-method "plink"))

;; coding system
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8)
(prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8-unix)

;; no toolbar
(tool-bar-mode 0)
;; no menu bar
(menu-bar-mode 0)
;; disable scroll bar
(scroll-bar-mode 0)
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
;;(transient-mark-mode t)
(global-hl-line-mode 1)

;; stop creating backup~ files
(setq make-backup-files nil)
;; stop creating #autosave# files
(setq auto-save-default nil)
;; backup in one place. flat, no tree structure
;; (setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
;; make backup to a designated dir, mirroring the full path
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
         ;; remove Windows driver letter in path, for example: “C:”
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath))
         (backupFilePath (replace-regexp-in-string
                          "//" "/"
                          (concat backupRootDir filePath "~"))))
    (make-directory (file-name-directory backupFilePath)
                    (file-name-directory backupFilePath))
    backupFilePath))
(setq make-backup-file-name-function 'my-backup-file-name)

;; remember cursor position, for emacs 25.1 or later
(save-place-mode 1)

;; set high kill ring
(setq-default kill-ring-max 100000)

;; tab size
(setq default-tab-width 4)
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;; Toggle visualization of matching parens
(show-paren-mode t)
;; Toggle automatic parens pairing
(electric-pair-mode t)

(setq x-select-enable-clipboard t)
;; (global-linum-mode 1)
(line-number-mode)
(column-number-mode)

;; replace yes no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; remove trailling whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; keep a list of recently opened files
(recentf-mode 1)
;; set F7 to list recently opened file
;; (global-set-key (kbd "<f7>") 'recentf-open-files)

;; make ibuffer default
(defalias 'list-buffers 'ibuffer)

;; 将Emacs插件库同步到本地来安装
;; https://lujun9972.github.io/blog/2018/03/06/%E5%B0%86emacs%E6%8F%92%E4%BB%B6%E5%BA%93%E5%90%8C%E6%AD%A5%E5%88%B0%E6%9C%AC%E5%9C%B0%E6%9D%A5%E5%AE%89%E8%A3%85/

(require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; (require 'benchmark-init)
;; To disable collection of benchmark data after init is done.
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-expand-minimally t)
(setq use-package-enable-imenu-support t)

;;; package
(defun install-packages ()
  (interactive)
  (setq package-list
        '(better-defaults
          ;;cnfonts
          auto-complete
          company
          yasnippet
          yasnippet-snippets
          scala-mode
          sbt-mode
          which-key
          exec-path-from-shell
          clojure-mode
          inf-clojure
          cider
          cider-decompile
          paredit
          ensime
          elpy
          anaconda-mode
          py-autopep8
          ein
          ivy
          swiper
          counsel
          smex
          markdown-mode
          magit
          web-mode
          js2-mode
          rjsx-mode
          js2-refactor
          undo-tree
          json-mode
          aggressive-indent
          window-numbering
          projectile
          rainbow-delimiters
          highlight-symbol
          highlight-thing
          origami
          ranger
          go-mode
          image+
          diminish
          hexo
          treemacs
          keyfreq
          ;;benchmark-init
          ))
  ;; fetch the list of packages available
  (unless package-archive-contents
    (package-refresh-contents))
  ;; install the missing package-list
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns w32))
  :config
  (exec-path-from-shell-initialize))

;; setup yasnippet
(use-package yasnippet
  :init (yas-global-mode 1))
(use-package yasnippet-snippets)

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
  :config
  ;; (define-auto-insert "\\.el$" ["default-lisp.el" ha/autoinsert-yas-expand])
  ;; (define-auto-insert "\\.sh$" ["default-sh.sh" ha/autoinsert-yas-expand])
  (define-auto-insert "\\.php$" ["template.php" autoinsert-yas-expand])
  ;; (define-auto-insert "/bin/"  ["default-sh.sh" ha/autoinsert-yas-expand])
  ;; (define-auto-insert "\\.html?$"
  ;;                     ["default-html.html" ha/autoinsert-yas-expand])
  )

;; enable company
(use-package company
  :init (setq company-dabbrev-downcase nil))
(add-hook 'after-init-hook 'global-company-mode)

;; mode line mode names settings
(use-package diminish
  :diminish ((abbrev-mode . " A")))

(use-package magit
  :bind ("M-n g" . magit-status)
  :config
  (setq magit-commit-show-diff nil
        truncate-lines nil))

;; ag
(use-package ag)
(use-package ripgrep)
(use-package rg
  :config (rg-enable-default-bindings (kbd "M-s")))

;; projectile
(use-package projectile
  :diminish projectile-mode
  :init (add-hook 'after-init-hook 'projectile-mode t)
  :config
  (setq projectile-mode-line
        '(:eval (format "[%s]" (projectile-project-name)))))

(load "myfuns.el")

(use-package smex)

;; use ivy
(use-package ivy
  :config (progn (ivy-mode 1)
                 (setq ivy-use-virtual-buffers t)))
(use-package swiper
  :bind ("C-s" . swiper))
(use-package counsel
  :bind (("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x)))

(use-package ranger
  :bind ("M-n r" . ranger))

(use-package zeal-at-point)

(use-package aggressive-indent)

;; clojure
(use-package cider
  :pin melpa-stable
  :bind (([f7] . cider-eval-last-sexp))
  :init (progn
          (add-hook 'cider-mode-hook 'eldoc-mode t)
          ;;(add-hook 'cider-mode-hook 'aggressive-indent-mode t)
          (setq cider-repl-display-help-banner nil
                cider-repl-use-pretty-printing nil
                cider-dynamic-indentation nil
                cider-default-cljs-repl 'figwheel
                cider-cljs-lein-repl
                "(do (require 'figwheel-sidecar.repl-api)
                   (figwheel-sidecar.repl-api/start-figwheel!)
                   (figwheel-sidecar.repl-api/cljs-repl))"))
  :config
  (progn
    (defun cider-with-profile (profile)
      "Starts up a cider repl using jack-in with the specific lein profile
   selected."
      (interactive "sProfile: ")
      (message "%s" profile)
      (let* ((profile-str (replace-regexp-in-string ":\\(.*\\)$" "\\1" profile))
             (lein-params (concat "with-profile +" profile-str
                                  " repl :headless")))
        (setq cider-lein-parameters lein-params)
        (cider-jack-in)))
    (setq cider-repl-wrap-history t)
    (when (string-equal system-type "windows-nt")
      (setq cider-jdk-src-paths
            '("C:/Program Files/Java/jdk1.8.0_111/src.zip")))))
(use-package clj-refactor)

(use-package ensime
  :pin melpa-stable)

;; web mode
(use-package web-mode
  :mode ("\\.tpl$" "\\.[agj]sp$" "\\.html?$" "\\.vue$" "\\.erb$")
  :init (progn
          (setq web-mode-markup-indent-offset 2)
          (setq web-mode-css-indent-offset 2)
          (setq web-mode-code-indent-offset 2)
          (setq web-mode-enable-current-column-highlight t)))

;; js mode
(use-package js2-mode
  :mode ("\\.js$")
  :config (setq js2-basic-offset 2))
(use-package js2-refactor
  :init (add-hook 'js2-mode-hook 'js2-refactor-mode t))
(use-package tide)

(use-package window-numbering
  :init (add-hook 'after-init-hook 'window-numbering-mode t))

(use-package which-key
  :diminish which-key-mode
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :init (add-hook 'after-init-hook 'which-key-mode t))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy))

(use-package origami
  :bind (("C-c z o" . origami-open-node)
         ("C-c z f" . origami-close-node)
         ("C-c z a" . origami-close-all-nodes)
         ("C-c z r" . origami-open-all-nodes))
  :config (global-origami-mode))

(use-package symbol-overlay
  :init (symbol-overlay-mode 1)
  :bind (("C-c M-i" . symbol-overlay-put)
         ("C-c M-n" . symbol-overlay-switch-forward)
         ("C-c M-p" . symbol-overlay-switch-backward)))

(use-package yaml-mode)

;; Racket
(use-package racket-mode
  :config (setq racket-racket-program "C:/Program Files/Racket/Racket.exe"
                racket-raco-program "C:/Program Files/Racket/raco.exe"))
(use-package geiser)

;; org mode
(use-package org
  :ensure nil
  :init (add-hook 'org-mode-hook
                  (lambda ()
                    (org-indent-mode 1)
                    (diminish 'org-indent-mode))
		  t)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb))
  :config
  (progn
    (setq org-startup-indented t)
    (setq org-src-fontify-natively t)
    (when (string-equal system-type "windows-nt")
      (setq org-directory "c:/project/orgs")
      (setq org-agenda-files '("c:/project/orgs/todo.org")))
    (setq org-default-notes-file (concat org-directory "/notes.org"))
    (setq org-todo-keywords
          '((sequence "TODO(t)" "|" "DONE(d!)")
            (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f!)")
            (sequence "|" "CANCELED(c@/!)")))
    (setq org-log-done 'time)
    (setq org-clock-persist 'history)
    (org-clock-persistence-insinuate)))
(use-package htmlize)

;; markdown
(use-package markdown-mode)

;; python
(use-package elpy
  :init (elpy-enable)
  :config
  (progn
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt --profile=dev"
          python-shell-interpreter-interactive-arg "-i --simple-prompt")
    ;; 解决 Shell Mode(cmd) 下中文乱码问题
    (when (string-equal system-type "windows-nt")
      ;; (defun kevin/windows-shell-mode-coding ()
      ;;   (set-buffer-file-coding-system 'gbk)
      ;;   (set-buffer-process-coding-system 'gbk 'gbk))
      ;; (add-hook 'inferior-python-mode-hook
      ;;           'kevin/windows-shell-mode-coding
      ;;   	t)
      ;; (add-hook 'python-mode-hook
      ;;           (lambda ()
      ;;             (add-hook 'before-save-hook 'elpy-format-code)))
      )))
(use-package py-autopep8
  :hook elpy-mode-hook)
(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))
(use-package ein
  :defer t
  :config (with-eval-after-load "ein"
            (defun advice:ein:notebooklist-open (&rest args)
              (call-interactively 'ein:force-ipython-version-check)
              'before)
            (advice-add 'ein:notebooklist-open
                        :before 'advice:ein:notebooklist-open)))

;; golang
(use-package company-go)
(use-package go-mode
  :ensure company
  :config
  (progn
    (setq-default tab-width 4)
    (add-hook 'go-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends) '(company-go))
                (company-mode))
	          t)
    (add-hook 'go-mode-hook
              (lambda ()
                (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports))
	          t)
    (add-hook
     'go-mode-hook
     #'(lambda()
         (require 'go-imports)
	     (define-key go-mode-map "\C-cI" 'go-imports-insert-import)
	     (define-key go-mode-map "\C-cR" go-imports-reload-packages-list)))
    (use-package go-eldoc
      :init (add-hook 'go-mode-hook 'go-eldoc-setup t))
    (add-hook 'before-save-hook 'gofmt-before-save)))


;; set ring sounds
;; (require 'alarm)


;; php
;;(require 'php2-mode)

(use-package php-mode
  :ensure projectile
  :mode ("\\.php[345]?\\'" "\\.inc\\'" "\\.module\\'")
  :config
  (require 'my-php)
  (add-to-list 'company-backends 'kj/company-php-backend)
  (add-hook 'php-mode-hook 'php-enable-psr2-coding-style))

;; composer global require "squizlabs/php_codesniffer=*"
;; (use-package phpcbf
;;   :init (add-hook 'php-mode-hook 'phpcbf-enable-on-save)
;;   :config (setq phpcbf-standard "PSR2"))

(use-package phpunit
  :config
  (progn (add-to-list 'auto-mode-alist '("\\.php$'" . phpunit-mode))
         (setq phpunit-program "php vendor/phpunit/phpunit/phpunit")
         (setq phpunit-default-program "php")
         (setq-default phpunit-args "vendor/phpunit/phpunit/phpunit")))

;; restclient
(use-package restclient
  :mode ("\\.http" . restclient-mode))

;; treemacs
(use-package treemacs
  ;; :config (setq treemacs-width 25)
  )

;; key freq
(use-package keyfreq
  :config (progn (keyfreq-mode 1)
                 (keyfreq-autosave-mode 1)))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)
         (prog-mode . rainbow-delimiters-mode)))

(use-package robe
  :ensure company
  :init (add-hook 'ruby-mode-hook 'robe-mode)
  :config (add-to-list 'company-backends 'company-robe))

(use-package rust-mode
  :bind (:map rust-mode-map
         ("TAB" . company-indent-or-complete-common))
  :config (setq company-tooltip-align-annotations t
                rust-format-on-save t))
(use-package racer
  :init (progn (add-hook 'rust-mode-hook #'racer-mode)
               (add-hook 'racer-mode-hook #'eldoc-mode)
               (add-hook 'racer-mode-hook #'company-mode)))
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package quickrun)

(use-package logview)

;; load key bindings myself
(load "keybindings.el")

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

(load custom-file :no-error :no-message)

;; set back gc threshold
(setq gc-cons-threshold (* gc-cons-threshold-old 2))
(setq garbage-collection-messages t)

(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "light slate gray")))))

;;; end, settings should be placed above
;;;; realy end!!!
