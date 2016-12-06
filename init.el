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

;; set ckj font windows下有中文的内容很慢，用下面完美解决
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "微软雅黑" :size 12)))

;; coding system
(prefer-coding-system 'utf-8)

;; no toolbar
(tool-bar-mode 0)
;; disable scroll bar
(scroll-bar-mode 0)

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
        '(s
          seq
          popup
          queue
          dash
          company
          yasnippet
          scala-mode
          sbt-mode
          ensime
          which-key
          clojure-mode
          inf-clojure
          cider
          ivy
          ;;helm
          ;;swiper-helm
          swiper
          markdown-mode
          magit
          web-mode
          js2-mode
          undo-tree
          ;;evil
          ;;evil-leader
          json-mode
          window-numbering
          projectile
          rainbow-delimiters
          highlight-symbol
          vimish-fold
          origami
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

;; use ivy
(if (package-installed-p 'ivy)
    (progn
      (ivy-mode 1)
      (setq ivy-use-virtual-buffers t)
      (global-set-key "\C-s" 'swiper))
  (when (package-installed-p 'helm)
    (progn
      (require 'helm-config)
      (helm-mode 1)
      (global-set-key (kbd "M-x") 'helm-M-x)
      (global-set-key "\C-s" 'swiper))))

;; clojure
(when (package-installed-p 'clojure-mode)
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (local-set-key (kbd "M-RET s c")
                             'cider-repl-clear-buffer)))
  (setq cider-repl-display-help-banner nil)
  (setq cider-repl-use-pretty-printing t))

;; evil
;;(when (package-installed-p 'evil)
;;  (evil-mode 1))

(when (package-installed-p 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
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
(when (package-installed-p 'vimish-fold)
  (vimish-fold-global-mode 1))

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

