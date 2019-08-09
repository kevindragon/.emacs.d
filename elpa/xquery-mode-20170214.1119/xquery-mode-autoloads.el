;;; xquery-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "xquery-mode" "xquery-mode.el" (0 0 0 0))
;;; Generated autoloads from xquery-mode.el

(autoload 'xquery-mode "xquery-mode" "\
A major mode for W3C XQuery 1.0

\(fn)" t nil)

(add-to-list 'auto-mode-alist '(".xq[erxy]\\'" . xquery-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xquery-mode" '("xquery-" "turn-on-xquery-" "toggle-xquery-mode-indent-style")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; xquery-mode-autoloads.el ends here
