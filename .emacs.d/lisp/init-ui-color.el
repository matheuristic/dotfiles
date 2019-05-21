;;; init-ui-color.el --- Emacs config color layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Configure color themes

;;; Code:

;; emulate reading on e-ink devices
(use-package eink-theme
  :pin "MELPA"
  :config
  (load-theme 'eink t)
  ;; make fringe bitmaps visible (default is to make them invisible)
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default))
  ;; make the look of comment delimiters match comment
  (set-face-attribute 'font-lock-comment-delimiter-face nil
                      :foreground (face-foreground 'font-lock-comment-face)
                      :background (face-background 'font-lock-comment-face)
                      :inherit 'font-lock-comment-face)
  ;; make inactive mode-line foreground visible
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "#909090")
  ;; make strings distinct
  (set-face-attribute 'font-lock-string-face nil
                      :foreground "#606060")
  ;; make line numbers less prominent
  (set-face-attribute 'line-number nil
                      :foreground "#cacaca"
                      :weight 'light)
  ;; hl-line-mode background color
  (with-eval-after-load 'hl-line
    (set-face-attribute 'hl-line nil :background "#fedcba")))

;; retro color scheme
;; (use-package gruvbox-theme
;;   :config (load-theme 'gruvbox t))

;; color scheme that complements `variable-pitch' mode
;; (use-package poet-theme
;;   :config (load-theme 'poet t))

(provide 'init-ui-color)

;;; init-ui-color.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
