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

;; Make comint-mode prompts read-only
(setq-default comint-prompt-read-only t)

;; Close term-mode and eshell-mode buffers on exit
(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  "Kill term buffers on term session ends."
  (kill-buffer))

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
  (require 'use-package)
  (setq use-package-always-ensure t))

;; Load evil first (so package defns can have evil keybindings)

(use-package evil
  :init
  (setq-default evil-want-C-u-scroll t) ;; set C-u to half-page up (like Vim)
  (evil-mode t)
  :config
  ;; emulate Vim leader key
  (defvar evil-leader "<SPC>")
  ;; function for easy normal-mode bindings with the leader key
  (defun evil-leader-set-key (key fn)
    "Defines an evil normal mode keybinding prefixed with evil-leader."
    (define-key evil-normal-state-map(kbd (concat evil-leader key)) fn))
  ;; set various modes emacs keybindings by default
  (dolist (mode '(calculator-mode
                  comint-mode
                  eshell-mode
                  eww-mode
                  fundamental-mode
                  shell-mode
                  term-mode))
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
    (lambda () (interactive)(raise-frame (next-frame))))
  ;; leader key bindings
  (evil-leader-set-key "b" 'switch-to-buffer)
  (evil-leader-set-key "d" 'dired)
  (evil-leader-set-key "e" 'find-file)
  (evil-leader-set-key "k b" 'kill-buffer)
  (evil-leader-set-key "k f" 'delete-frame)
  (evil-leader-set-key "k w" 'delete-window)
  (evil-leader-set-key "m" 'evil-show-marks)
  (evil-leader-set-key "n f" 'new-frame)
  (evil-leader-set-key "r" 'list-registers)
  (evil-leader-set-key "w" 'whitespace-mode)
  (evil-leader-set-key "y" (lambda () (interactive)(popup-menu 'yank-menu)))
  (evil-leader-set-key "#" 'comment-or-uncomment-region))

;; Other packages

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package elpy
  :init (with-eval-after-load 'python (elpy-enable)))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (when (featurep 'evil)
    (define-key evil-normal-state-map (kbd "[ l") 'flycheck-previous-error)
    (define-key evil-normal-state-map (kbd "] l") 'flycheck-next-error)))

(use-package gruvbox-theme
  :config (load-theme 'gruvbox t))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ido
  :init
  (setq ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        ido-enable-flex-matching t
        ido-everywhere t
        ido-use-virtual-buffers t)
  (ido-mode t))

(use-package ido-ubiquitous
  :init
  (ido-ubiquitous-mode t))

(use-package magit
  :config
  (when (featurep 'evil)
    (evil-set-initial-state 'magit-mode 'emacs)
    (evil-set-initial-state 'magit-popup-mode 'emacs)
    (evil-leader-set-key "g" 'magit-status)))

(use-package projectile
  :init (projectile-global-mode)
  :config
  (setq projectile-switch-project-action #'projectile-commander)
  (when (featurep 'evil)
    (evil-leader-set-key "p e" 'projectile-find-file)
    (evil-leader-set-key "p k" 'projectile-kill-buffers)
    (evil-leader-set-key "p p" 'projectile-switch-project)))

(use-package rainbow-delimiters
  :config
  (when (featurep 'evil)
    (evil-leader-set-key "R" 'rainbow-delimiters-mode)))

(use-package recentf
  :init (recentf-mode t)
  :config
  (setq recentf-max-menu-items 10
        recentf-max-saved-items 50)
  (when (featurep 'evil)
    (evil-leader-set-key "f" 'recentf-open-files)))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config (smex-initialize))

(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :config
  (when (featurep 'evil)
    (setq evil-want-fine-undo t)))

(provide 'init)
;;; init.el ends here
