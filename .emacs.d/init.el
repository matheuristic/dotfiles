;;; init.el --- Emacs config file ~/.emacs.d/init.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; User interface

;; Suppress splash screen
(setq inhibit-startup-message t)

;; Turn off audible and visual bells
(setq ring-bell-function 'ignore)

;; Show column numbers
(setq column-number-mode t)

;; Show matching parentheses without delay
(setq-default show-paren-delay 0)
(show-paren-mode t)

;; Soft tabs for indentation (use C-q <TAB> to insert hard tabs)
(setq-default indent-tabs-mode nil)

;; Menus and scroll bars visible only for GUI mode
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (and (not (display-graphic-p)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (and (not (display-graphic-p)) (fboundp 'scroll-bar-mode))
  (scroll-bar-mode -1))

;;; Backup and config files

;; Set backup directory
(setq backup-directory-alist '((".*" . "~/.backup")))

;; Keep Customize settings in separate file, ignore if no such file exists
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;; Fonts

;; Set font for GUI mode
(defvar my-font "Source Code Pro")
(defvar my-font-height (if (eq system-type 'darwin) 140 110))
(defvar my-font-weight (if (eq system-type 'darwin) 'light 'regular))
(defvar my-font-width 'normal)
(when (and (display-graphic-p)
           (and my-font (not (string= my-font "")))
           (x-list-fonts my-font))
  (set-face-attribute 'default nil
                      :family my-font
                      :height my-font-height
                      :weight my-font-weight
                      :width my-font-width))

;;; Package management

;; Regenerate outdated byte code
(setq load-prefer-newer t)

;; User packages in ~/.emacs.d/lisp
(defvar lisp-dir (expand-file-name "lisp" user-emacs-directory))
(unless (file-exists-p lisp-dir)
  (make-directory lisp-dir))
(add-to-list 'load-path lisp-dir)

;; Third-party packages in ~/.emacs.d/site-lisp and its subdirs
(defvar site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(unless (file-exists-p site-lisp-dir)
  (make-directory site-lisp-dir))
(add-to-list 'load-path site-lisp-dir)
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;;; Packages from ELPA-compatible package repositories

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

;; Bootstrap use-package, https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Load evil and its addons first (so package defns can have evil keybindings)

(use-package evil
  :ensure t
  :init
  (setq-default evil-want-C-u-scroll t) ;; set C-u to half-page up (like Vim)
  (evil-mode t)
  :config
  ;; set various modes to default to emacs keybindings
  (dolist (mode '(apropos-mode comint-mode compilation-mode diff-mode
                  dired-mode erc-mode eshell-mode fundamental-mode
                  git-commit-mode git-rebase-mode grep-mode gud-mode
                  help-mode Info-mode message-mode nav-mode org-mode
                  shell-mode speedbar-mode term-mode))
    (evil-set-initial-state mode 'emacs))
  ;; useful bracket mappings (like vim-unimpaired)
  (define-key evil-normal-state-map (kbd "[ e")
    (lambda (n) (interactive "p")
      (dotimes (_ n)
        (progn (transpose-lines 1)(forward-line -2)))))
  (define-key evil-normal-state-map (kbd "] e")
    (lambda (n) (interactive "p")
      (dotimes (_ n)
        (progn (forward-line 1)(transpose-lines 1)(forward-line -1)))))
  (define-key evil-visual-state-map (kbd "[ e")
    (lambda (n) (interactive "p")
      (concat ":'<,'>move '<--" (number-to-string n))))
  (define-key evil-visual-state-map (kbd "] e")
    (lambda (n) (interactive "p")
      (concat ":'<,'>move '>+" (number-to-string n))))
  (define-key evil-normal-state-map (kbd "[ h") 'diff-hunk-prev)
  (define-key evil-normal-state-map (kbd "] h") 'diff-hunk-next)
  (define-key evil-normal-state-map (kbd "[ f")
    (lambda () (interactive)(raise-frame (previous-frame))))
  (define-key evil-normal-state-map (kbd "] f")
    (lambda () (interactive)(raise-frame (next-frame)))))

(use-package evil-leader
  :ensure t
  :init (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "b" 'switch-to-buffer
    "B" 'ibuffer
    "d" 'dired
    "e" 'find-file
    "k b" 'kill-buffer
    "k f" 'delete-frame
    "k w" 'delete-window
    "m" 'evil-show-marks
    "n f" 'new-frame
    "r" 'list-registers
    "w" 'whitespace-mode
    "y" (lambda () (interactive)(popup-menu 'yank-menu))
    "#" 'comment-or-uncomment-region))

;; Other packages

(use-package elpy
  :ensure t
  :init (with-eval-after-load 'python (elpy-enable)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (define-key evil-normal-state-map (kbd "[ l") 'flycheck-previous-error)
  (define-key evil-normal-state-map (kbd "] l") 'flycheck-next-error))

(use-package gruvbox-theme
  :ensure t
  :config (load-theme 'gruvbox t))

(use-package ido
  :ensure t
  :init
  (setq ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        ido-enable-flex-matching t
        ido-everywhere t)
  (ido-mode t))

(use-package magit
  :ensure t
  :config
  (evil-set-initial-state 'magit-mode 'emacs)
  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (evil-leader/set-key "g" 'magit-status))

(use-package rainbow-delimiters
  :ensure t
  :config (evil-leader/set-key "R" 'rainbow-delimiters-mode))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config (smex-initialize))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :config (setq evil-want-fine-undo t))

(provide 'init)
;;; init.el ends here
