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

;; font list by priority, used in lisp/init-ui-font.el
(setq init-ui-font-default-list '("IBM Plex Mono"
                                  "Consolas"
                                  "Menlo"
                                  "DejaVu Sans Mono")
      init-ui-font-variable-pitch-list '("IBM Plex Serif"
                                         "Alegreya"
                                         "Constantia"
                                         "Charter"
                                         "DejaVu Serif"))

;; set directory where conda is installed, used in lisp/init-lang-python.el
(setq conda-anaconda-home "~/miniconda3/")

;; path to MS Python Language Server binary, used by lisp/init-lang-python.el
(setq lsp-python-ms-executable "~/.local/bin/Microsoft.Python.LanguageServer")

;; Set default directory for Org files to ~/org
(setq org-directory (file-name-as-directory (file-truename "~/org/")))

;; Use top-level Org files in default Org directory for Org agenda files
(setq org-agenda-files (file-expand-wildcards (concat org-directory "*.org")))

;; use multimarkdown for processing markdown files in markdown-mode
(setq markdown-command "multimarkdown")

(require 'init-master)

(provide 'init)
;;; init.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
