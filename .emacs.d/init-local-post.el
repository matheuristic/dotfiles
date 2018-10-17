;;; init-local-post.el --- Sample Emacs local post-initialization config file -*- lexical-binding: t; -*-

;; Author: matheuristic
;; URL: https://github.com/matheuristic/dotfiles

;;; Commentary:

;; Symlink or copy this file to ~/.emacs.d/init-local-post.el

;;; Code:

;; virtualenv activation in Emacs - MELPA Stable
(use-package pyvenv
  :init
  (setenv "WORKON_HOME" "~/miniconda3/envs")
  (pyvenv-mode 1)
  (pyvenv-tracking-mode 1))

;; return to normal mode in evil with custom key seq - MELPA Stable
(use-package evil-escape
  :after evil
  :diminish evil-escape-mode
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "jk"))

;; Docker file support - MELPA Stable
(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(provide 'init-local-post)
;;; init-local-post.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
