;;; init.el --- Emacs config file -*- lexical-binding: t; -*-

;; Author: matheuristic
;; URL: https://github.com/matheuristic/dotfiles
;;; Commentary:

;; Symlink or copy this file to ~/.emacs or ~/.emacs.d/init.el

;;; Code:

;; user packages in ~/.emacs.d/lisp
(defvar lisp-dir (expand-file-name "lisp" user-emacs-directory))
(unless (file-exists-p lisp-dir) (make-directory lisp-dir))
(add-to-list 'load-path lisp-dir)
(dolist (project (directory-files lisp-dir t "\\w+"))
  (if (file-directory-p project) (add-to-list 'load-path project)))

;; third-party packages in ~/.emacs.d/site-lisp and its subdirectories
(defvar site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(unless (file-exists-p site-lisp-dir) (make-directory site-lisp-dir))
(add-to-list 'load-path site-lisp-dir)
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (if (file-directory-p project) (add-to-list 'load-path project)))

(setq init-ui-font-default-list '("IBM Plex Mono"
                                  "Iosevka Slab"
                                  "Consolas"
                                  "Menlo"
                                  "DejaVu Sans Mono")
      init-ui-font-variable-pitch-list '("ETBembo"
                                         "IBM Plex Sans"
                                         "Constantia"
                                         "Hoefler Text"
                                         "DejaVu Serif"))

(setq init-lang-python-ms-executable (concat "~/Projects/random/Microsoft"
                                             "/python-language-server/output"
                                             "/bin/Release/osx-x64/publish"
                                             "/Microsoft.Python.LanguageServer"))

(require 'init-master)

(provide 'init)
;;; init.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
