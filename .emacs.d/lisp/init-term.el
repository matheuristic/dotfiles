;;; init-term.el --- Emacs config shell layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Configure eshell and term

;;; Code:

;; make shell prompts read-only
(setq comint-prompt-read-only t)

;; close term buffers on exit
(defadvice term-handle-exit (after term-kill-buffer-on-exit activate)
  "Kill term buffer on term session end."
  (kill-buffer))

;; Eshell
(use-package eshell
  :ensure nil ;; built-in
  :commands (eshell eshell-command)
  :init (setq eshell-review-quick-commands nil
              eshell-smart-space-goes-to-end t
              eshell-where-to-jump 'begin)
  :config
  (require 'em-term)
  (require 'em-smart)
  (add-to-list 'eshell-visual-commands '("htop" "lftp" "ssh" "vim"))
  (add-to-list 'eshell-visual-subcommands '("diff" "git" "log" "show" "ssh" "vagrant")))

(provide 'init-term)

;;; init-term.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
