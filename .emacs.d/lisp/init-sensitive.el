;;; init-sensitive.el --- Emacs config for sensitive files -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up minor mode for sensitive files that disables backups and auto-saves
;; for the buffer. Copied from http://anirudhsasikumar.net/blog/2005.01.21.html

;;; Code:

(defgroup init-sensitive-el nil
  "Font settings."
  :group 'convenience)

(defcustom init-sensitive-file-patterns '("\\.gpg$")
  "List of file patterns for which sensitive-mode is enabled by default."
  :type '(repeat string)
  :group 'init-sensitive-el)

(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Sensitive"
  ;; The minor mode bindings.
  nil
  (if (symbol-value sensitive-mode)
      (progn
	;; disable backups
	(set (make-local-variable 'backup-inhibited) t)
	;; disable auto-save
	(if auto-save-default
	    (auto-save-mode -1)))
    ;resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
    ;resort to default auto save setting
    (if auto-save-default
	(auto-save-mode 1))))

;; set up auto-mode-alist for sensitive-mode
(dolist (file-pattern init-sensitive-file-patterns)
  (add-to-list 'auto-mode-alist `(,file-pattern . sensitive-mode)))

(provide 'init-sensitive)

;;; init-sensitive.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
