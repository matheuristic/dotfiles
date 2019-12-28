;;; init-lang-scheme.el --- Emacs config Scheme language layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up Scheme tooling

;;; Code:

;; uncomment to use chez by default in geiser
;; (defvar geiser-active-implementations '(chez))

;; enable paredit-mode when editing Scheme
(add-hook 'scheme-mode-hook #'paredit-mode)

;; color delimiters when editing Scheme
(with-eval-after-load 'rainbow-delimiters
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode))

;; Scheme interactive development environment for Emacs
(use-package geiser
  :defer t
  :pin "MELPA")

(provide 'init-lang-scheme)

;;; init-lang-scheme.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
