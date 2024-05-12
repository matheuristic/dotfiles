;;; early-init.el --- Emacs early init file -*- lexical-binding: t; -*-

;; Author: matheuristic
;; URL: https://github.com/matheuristic/emacs-config
;; Generated: Sun May 12 08:43:48 2024

;;; Commentary:

;; Emacs early initialization configuration file, symlink or copy to
;; ~/.emacs.d/early-init.el or $XDG_CONFIG_HOME/.emacs.d/early-init.el

;; In Emacs 27+, the sequence of initialization is
;; 1. early-init.el
;; 2. package.el
;; 3. init.el

;; early-init.el is run before UI elements are rendered,
;; so it is best to configure UI elements here rather than init.el

;;; Code:

;; Optimizations

;; optimizations for reducing startup time (reverted later)
;; * file-name-handler-alist -> nil as it is scanned when files are loaded
;; * increase garbage collection threshold
;; * increase max bytes read from a sub-process in a single op (Emacs 27+)
(setq file-name-handler-alist-orig file-name-handler-alist
      gc-cons-threshold-orig gc-cons-threshold
      file-name-handler-alist nil ;; no special file handling during init
      gc-cons-threshold 134217728) ;; 128MB in bytes, default is 800k

;; revert optimizations after initialization
(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-orig)
            (setq gc-cons-threshold gc-cons-threshold-orig))
          t)

;; disable automatic activation of installed packages
(setq package-enable-at-startup nil)

;; optimizations for improving I/O performance
;; * increase max bytes read from a sub-process in a single op (Emacs 27+)
(when (boundp 'read-process-output-max)
  (setq read-process-output-max 1048576)) ;; 1MB in bytes, default 4096 bytes

;; Package management

;; when multiple versions of a package are installed, load the newest
(setq load-prefer-newer t)

;; add user packages in lisp/ to load path
(defvar lisp-dir (expand-file-name "lisp" user-emacs-directory))
(unless (file-exists-p lisp-dir) (make-directory lisp-dir))
(add-to-list 'load-path lisp-dir)
(dolist (project (directory-files lisp-dir t "\\w+"))
  (when (file-directory-p project) (add-to-list 'load-path project)))

;; add third-party packages in site-lisp/ and its subdirs to load path
(defvar site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(unless (file-exists-p site-lisp-dir) (make-directory site-lisp-dir))
(add-to-list 'load-path site-lisp-dir)
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project) (add-to-list 'load-path project)))

;; Visual user interface components

;; set cursor after initialization
(setq-default cursor-type 'bar)

;; hide cursor in non-selected windows
(setq-default cursor-in-non-selected-windows nil)

;; remove unused UI elements
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
;; (if (and (not (display-graphic-p))
;;          (fboundp 'menu-bar-mode))
;;     (menu-bar-mode -1))

;; use local eReader theme from ~/.emacs.d/themes/ereader-theme.el
(add-to-list 'custom-theme-load-path
             (file-name-as-directory
              (expand-file-name "themes" user-emacs-directory)))
(load-theme 'ereader t)

;;; set typefaces for graphical Emacs
;;; main fonts
;;; - note that the default face must specify :height using an integer
;;;   in units of 1/10 point but other faces can use a float specifying
;;;   height relative to its underlying face if desired
(set-face-attribute 'default nil :family "Comic Code"
                    :height (if (eq system-type 'darwin) 130 110)
                    :weight 'normal :width 'normal)
(set-face-attribute 'fixed-pitch nil :family "Comic Code"
                    :height 1.0
                    :weight 'normal :width 'normal)
(set-face-attribute 'variable-pitch nil :family "Go"
                    :height 1.0
                    :weight 'normal :width 'normal)
(set-face-attribute 'mode-line nil :family "PragmataPro"
                    :height (if (eq system-type 'darwin) 130 90)
                    :weight 'normal :width 'normal)
(set-face-attribute 'mode-line-inactive nil :family "PragmataPro"
                    :height (if (eq system-type 'darwin) 130 90)
                    :weight 'normal :width 'normal)
;;; fallback font
(set-fontset-font t nil "Symbola" nil 'append)
;;; increase min underline offset for more readable underlined words
(setq underline-minimum-offset 5)
;;; uncomment below if using a large font to not compact it during GC
(setq inhibit-compacting-font-caches t)

;; load local early init configuration
(require 'early-init-local nil t)       ; don't raise errors

(provide 'early-init)
;;; early-init.el ends here
