;; author: Kevin.Jiang
;; E-mail: kittymiky@gmail.com

;; 设置我的个人信息
(setq user-full-name "Kevin Jiang")
(setq user-mail-address "kittymiky@gmail.com")


;;; 设置界面
;; 关闭启动画面
(setq inhibit-startup-message t)
;; 关闭滚动条
(scroll-bar-mode -1)
;; 去掉工具栏
(tool-bar-mode 0)
;; 显示时间，格式如下
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(transient-mark-mode t)
;; 在status bar显示行号和列号
(line-number-mode)
(column-number-mode)
;; 显示行列号
(global-linum-mode t)


;; 在标题栏提示你目前在什么位置
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

;;; 高亮当前行
(global-hl-line-mode t)

;;; 当emacs退出时保存打开的文件
;;(desktop-save-mode 1)
;;(setq-default desktop-load-locked-desktop t)
;;(desktop-load-default)
;;(desktop-read)

;;;
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 100)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(run-at-time nil (* 10 60) 'recentf-save-list)

;; 记住上次文件打开的位置
(setq-default save-place t)

;; 当emacs退出时保存文件打开状态
(add-hook 'kill-emacs-hook
	  '(lambda()
	     (desktop-save "~/.emacs.d")))

;; 不产生备份文件
(setq make-backup-files nil)

;; 以 y/n 代表 yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; 显示括号匹配,但不跳转到另外一个括号的位置
(show-paren-mode t)
;; 开启括号自动补全
(electric-pair-mode t)

;; 把kill-ring设置大一些
(setq-default kill-ring-max 100000)


;;; 设置load-path
(add-to-list 'load-path "~/.emacs.d/lisp")


;;; install packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq package-list
      '(helm
	auto-complete
	yasnippet
	company

	evil-magit
        gitattributes-mode
        gitconfig-mode
        gitignore-mode
        git-commit
        git-messenger
        git-timemachine
        helm-gitignore
        magit
        magit-gitflow
        ;; not compatible with magit 2.1 at the time of release
        ;; magit-svn
        orgit
        smeargle
	
	vi-tilde-fringe
	cider
	inf-clojure
	scala-mode
	evil
	which-key))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (pkg package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))


;;; helm
(require 'helm-config)
(helm-mode 1)


;;; auto complete
(ac-config-default)
(setq ac-sources
      '(ac-source-filename
	ac-source-functions
	ac-source-yasnippet
	ac-source-variables
	ac-source-symbols
	ac-source-features
	ac-source-abbrev
	ac-source-words-in-same-mode-buffers
	ac-source-dictionary))
(global-auto-complete-mode t)
(yas-global-mode 1)
(add-hook 'after-init-hook 'global-company-mode)


;;; fringe的位置使用vi的波浪符号
(global-vi-tilde-fringe-mode 1)


;;; 加载自定义快捷键
(load "my-key-bindings")


;;; evil mode
;(require 'evil)
(evil-mode 1)
(add-hook 'evil-emacs-state-entry-hook
	  '(lambda()
	     (setq cursor-type 'bar)))

;;; which key
;(require 'which-key)
(which-key-mode)
