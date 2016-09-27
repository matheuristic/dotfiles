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
           (and (and my-font (not (string= my-font "")))
                (x-list-fonts my-font)))
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

;; MELPA packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents (package-refresh-contents))

;; Packages to use
(defvar package-list
  '(
    ;company
    elpy
    evil
    ;evil-paredit
    gruvbox-theme
    helm
    magit
    ;paredit
    undo-tree
    zenburn-theme
    )
  "Packages to install.")

;; Install packages if necessary
(dolist (p package-list)
  (unless (package-installed-p p)
    (package-install p)))

;; Color theme
(load-theme 'gruvbox t)

;; elpy
(elpy-enable)

;; evil
(setq evil-want-C-u-scroll t) ;; set C-u to half-page up (match Vim behavior)
(require 'evil)
(evil-mode t)

;; helm
(require 'helm-config)
(helm-mode t)

;; magit
(require 'magit)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)

;; done
