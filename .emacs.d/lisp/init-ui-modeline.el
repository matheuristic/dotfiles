;;; init-ui-modeline.el --- Emacs config UI layer mode-line settings -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up mode-line

;;; Code:

(when (display-graphic-p)
  ;; ;; display mode-line elements using tabs and ribbons
  ;; (use-package moody
  ;;   :config
  ;;   ;; uncomment below if using official Emacs for OSX
  ;;   (setq x-underline-at-descent-line t)
  ;;   (moody-replace-mode-line-buffer-identification)
  ;;   (moody-replace-vc-mode)
  ;;   ;; modify slant fn if using official Emacs for Mac OS X build to fix colors
  ;;   (if (and (eq system-type 'darwin)
  ;;            (eq window-system 'ns))
  ;;       (setq moody-slant-function 'moody-slant-apple-rgb)))

  ;; show full path in the tooltip for mode-line buffer name
  (setq-default mode-line-buffer-identification
                (list (propertize
                       "%12b"
                       'face 'mode-line-buffer-id
                       'help-echo '(format
                                    (mapconcat 'identity
                                               '("%s"
                                                 "mouse-1: Previous buffer"
                                                 "mouse-3: Next buffer")
                                               "\n")
                                    (or (buffer-file-name) (buffer-name)))
                       'mouse-face 'mode-line-highlight
                       'local-map mode-line-buffer-identification-keymap)))

  ;; hide minor-modes behind a menu, accessible via a right-click or `minions-minor-mode-menu'
  (use-package minions
    :init (minions-mode 1)
    :config (setq minions-direct '(overwrite-mode))))

(provide 'init-ui-modeline)

;;; init-ui-modeline.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
