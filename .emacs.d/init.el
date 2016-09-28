;; Suppress splash screen
(setq inhibit-startup-message t)

;; Show column numbers
(setq column-number-mode t)

;; Show matching parentheses
(setq show-paren-delay 0)
(show-paren-mode t)

;; Show trailing whitespace
;(setq-default show-trailing-whitespace t)

;; Set backup directory
(setq backup-directory-alist '((".*" . "~/.backup")))

;; Keep Customize settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Soft tabs (use C-q to insert hard tabs)
(setq-default indent-tabs-mode nil)

;; Menus and scroll bars visible only for GUI mode
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (and (not (display-graphic-p)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (and (not (display-graphic-p)) (fboundp 'scroll-bar-mode))
  (scroll-bar-mode -1))

;; Set font for GUI mode
(defvar my-font "Source Code Pro")
(defvar my-font-height 140)
(defvar my-font-weight 'light)
(defvar my-font-width 'normal)
(when (and (display-graphic-p)
           (and my-font (not (string= my-font "")))
           (x-list-fonts my-font))
  (set-face-attribute 'default nil
                      :family my-font
                      :height my-font-height
                      :weight my-font-weight
                      :width my-font-width))

;; User packages in ~/.emacs.d/lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Third-party packages in ~/.emacs.d/site-lisp and its subdirs
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-dir)
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Package management

;; Regenerate outdated byte code
(setq load-prefer-newer t)

;; MELPA packages
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))

(package-initialize)

(unless package-archive-contents (package-refresh-contents))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Packages, see https://github.com/jwiegley/use-package

(use-package elpy
  :ensure t
  :init (with-eval-after-load 'python (elpy-enable)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package gruvbox-theme
  :ensure t
  :config (load-theme 'gruvbox t))

(use-package helm
  :ensure t
  :init
  (progn
    (require 'helm-config)
    (helm-mode t))
  :bind (("M-x" . helm-M-x)))

(use-package magit
  :ensure t)

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode))

;; load evil and its addons last as keybindings may depend on earlier packages
(use-package evil
  :ensure t
  :init
  (progn
    (setq evil-want-C-u-scroll t) ;; set C-u to half-page up (Vim behavior)
    (use-package evil-leader
      :ensure t
      :init (global-evil-leader-mode)
      :config
      (progn
        (evil-leader/set-leader "<SPC>") ;; set leader to spacebar
        (unless (featurep 'helm)
          (evil-leader/set-key
            "e" 'find-file
            "b" 'switch-to-buffer))
        (eval-after-load "helm"
          (progn
            (evil-leader/set-key
              "I" 'helm-info-emacs
              "a" 'helm-apropos
              ;"m" 'helm-man-woman
              "b" 'helm-mini ;; buffers and recent files
              "e" 'helm-find-files
              "f" 'helm-for-files
              ;"l" 'helm-locate
              "/" 'helm-find
              "o" 'helm-occur
              "y" 'helm-show-kill-ring
              ;"t" 'helm-top
              "i" 'helm-semantic-or-imenu
              ;"h" 'helm-projectile-find-file ;; requires helm-projectile
              ;"H" 'helm-projectile ;; requires helm-projectile
              )))
        (eval-after-load "magit"
          (evil-leader/set-key
            "g" 'magit-status
            ))
        (evil-leader/set-key
          "d" 'dired
          ;"k" 'kill-buffer
          )))
    (evil-mode t)))

;; done
