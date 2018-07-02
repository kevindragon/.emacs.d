;;; phpunit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "phpunit" "phpunit.el" (0 0 0 0))
;;; Generated autoloads from phpunit.el

(autoload 'phpunit-current-test "phpunit" "\
Launch PHPUnit on curent test.

\(fn)" t nil)

(autoload 'phpunit-current-class "phpunit" "\
Launch PHPUnit on current class.

\(fn)" t nil)

(autoload 'phpunit-current-project "phpunit" "\
Launch PHPUnit on current project.

\(fn)" t nil)

(autoload 'phpunit-group "phpunit" "\
Launch PHPUnit for group.

\(fn USE-LAST-GROUP &optional GROUP)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "phpunit" '("php")))

;;;***

;;;### (autoloads nil "phpunit-mode" "phpunit-mode.el" (0 0 0 0))
;;; Generated autoloads from phpunit-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "phpunit-mode" '("phpunit-mode")))

;;;***

;;;### (autoloads nil nil ("phpunit-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; phpunit-autoloads.el ends here
