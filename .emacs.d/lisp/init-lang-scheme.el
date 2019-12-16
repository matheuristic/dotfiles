;;; init-lang-scheme.el --- Emacs config Scheme language layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up Scheme tooling

;;; Code:

;; uncomment to use chez as the default scheme in geiser
;; (defvar geiser-active-implementations '(chez))

;; Geiser
(use-package geiser
  :defer
  :pin "MELPA")

(provide 'init-lang-scheme)

;;; init-lang-scheme.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
