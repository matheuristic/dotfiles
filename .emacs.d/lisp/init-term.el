;;; init-term.el --- Emacs config shell layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up eshell and term

;;; Code:

;; read-only comint-mode prompts
(setq comint-prompt-read-only t)

;; close term-mode and eshell-mode buffers on exit
(defadvice term-handle-exit (after term-kill-buffer-on-exit activate)
  "Kill term buffer on term session end."
  (kill-buffer))

(provide 'init-term)

;;; init-term.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
