;;; init Emacs
;; author: Kevin Jiang
;; E-mail: wenlin1988@126.com

(setq inhibit-startup-message t)

;; setup my info
(setq user-full-name "Kevin Jiang")
(setq user-mail-address "wenlin1988@126.com")

;; set load path
(add-to-list 'load-path "~/.emacs.d/lisp")

;; load key bindings myself
(load "keybindings.el")

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
                    (font-spec :family "微软雅黑" :size 12))
  (let ((mypaths
         '("C:/msys64/mingw64/bin"
           "C:/msys64/usr/bin"
           "C:/Program Files (x86)/Mozilla Firefox/"
           "C:/Program Files (x86)/Google/Chrome/Application")))
    (setenv "PATH" (concat
                    (mapconcat 'identity mypaths ";") ";"
                    (getenv "PATH")))
    (setq exec-path (append
                     mypaths
                     (split-string (getenv "PATH") ";")
                     (list "." exec-directory))))))

;; coding system
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8)
(prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8-unix)

(when (display-graphic-p)
  ;; no toolbar
  (tool-bar-mode 0)
  ;; disable scroll bar
  (scroll-bar-mode 0))

(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
;;(transient-mark-mode t)

;; stop creating backup~ files
(setq make-backup-files nil)
;; stop creating #autosave# files
(setq auto-save-default nil)
;; backup in one place. flat, no tree structure
;;(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
;; make backup to a designated dir, mirroring the full path
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
         ;; remove Windows driver letter in path, for example: “C:”
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) 
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setq make-backup-file-name-function 'my-backup-file-name)

;; remember cursor position, for emacs 25.1 or later
(save-place-mode 1)

;; set high kill ring
(setq-default kill-ring-max 100000)

;; tab size
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)

;; Toggle visualization of matching parens
(show-paren-mode t)
;; Toggle automatic parens pairing
(electric-pair-mode t)

(setq x-select-enable-clipboard t)
(global-linum-mode 1)
(line-number-mode)
(column-number-mode)

;; replace yes no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;; package
(defun install-packages ()
  (interactive)
  (setq package-list
        '(better-defaults
          company
          yasnippet
          exec-path-from-shell
          treemacs
          scala-mode
          sbt-mode
          ensime
          which-key
          clojure-mode
          inf-clojure
          cider
          ;;parinfer
          ;;paredit
          elpy
          py-autopep8
          ein
          ivy
          ;;helm
          ;;swiper-helm
          swiper
          markdown-mode
          magit
          web-mode
          js2-mode
          undo-tree
          json-mode
          aggressive-indent
          window-numbering
          projectile
          rainbow-delimiters
          highlight-symbol
          origami
          ranger
          gnuplot-mode))

  ;; fetch the list of packages available
  (unless package-archive-contents
    (package-refresh-contents))

  ;; install the missing package-list
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

;; setup yasnippet
(when (package-installed-p 'yasnippet)
  (yas-global-mode t)
  (yas-minor-mode-on))

;; enable company mode
(when (package-installed-p 'company)
  (global-company-mode 1))

;; keep a list of recently opened files
(recentf-mode 1)
;; set F7 to list recently opened file
(global-set-key (kbd "<f7>") 'recentf-open-files)

;; make ibuffer default
(defalias 'list-buffers 'ibuffer)

(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)

;; exec-path-from-shell
(when (package-installed-p 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; use ivy
(when (package-installed-p 'ivy)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (when (package-installed-p 'swiper)
    (global-set-key "\C-s" 'swiper)))

;; clojure
(when (package-installed-p 'clojure-mode)
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (local-set-key (kbd "M-RET s c")
                             'cider-repl-clear-buffer)))
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (setq cider-repl-display-help-banner nil)
  (setq cider-repl-use-pretty-printing t)
  (defun cider-with-profile (profile)
    "Starts up a cider repl using jack-in with the specific lein profile
   selected."
    (interactive "sProfile: ")
    (message "%s" profile)
    (let* ((profile-str (replace-regexp-in-string ":\\(.*\\)$" "\\1" profile))
           (lein-params (concat "with-profile +" profile-str " repl :headless")))
      (setq cider-lein-parameters lein-params)
      (cider-jack-in))))

(when (package-installed-p 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(when (package-installed-p 'window-numbering)
  (window-numbering-mode))

(when (package-installed-p 'rainbow-delimiters)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;; fold
(when (package-installed-p 'origami)
  (global-origami-mode)
  (add-hook 'origami-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c z o") 'origami-open-node)
              (local-set-key (kbd "C-c z f") 'origami-close-node)
              (local-set-key (kbd "C-c z a") 'origami-close-all-nodes)
              (local-set-key (kbd "C-c z r") 'origami-open-all-nodes))))

(when (package-installed-p 'which-key)
  (which-key-mode))


;; org mode
(setq org-startup-indented t)


;; python
(when (package-installed-p 'elpy)
  (elpy-enable)
  (elpy-use-ipython)
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-b C-c")
                             (lambda ()
                               (interactive)
                               (let ((comint-buffer-maximum-size 0))
                                 (comint-clear-buffer))))))
  (when (package-installed-p 'py-autopep8)
    (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))
  (when (string-equal system-type "windows-nt")
    ;; 解决 Shell Mode(cmd) 下中文乱码问题
    ;;(set-terminal-coding-system 'gbk)
    ;;(modify-coding-system-alist 'process "*" 'gbk)
    (defun kevin/windows-shell-mode-coding ()
      (set-buffer-file-coding-system 'gbk)
      (set-buffer-process-coding-system 'gbk 'gbk))
    ;;(add-hook 'shell-mode-hook #'kevin/windows-shell-mode-coding)
    (add-hook 'inferior-python-mode-hook
              #'kevin/windows-shell-mode-coding)))
