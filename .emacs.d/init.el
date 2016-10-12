;;; init.el --- Emacs config file -*- lexical-binding: t; -*-

;; Author: matheuristic
;; URL: https://github.com/matheuristic/dotfiles

;;; Commentary:

;; Symlink or copy this file to ~/.emacs or ~/.emacs.d/init.el

;;; Code:

;; suppress splash screen
(setq inhibit-startup-message t)

;; turn off audible and visual bells
(setq ring-bell-function 'ignore)

;; show column numbers
(setq column-number-mode t)

;; show matching parentheses without delay
(setq-default show-paren-delay 0)
(show-paren-mode t)

;; soft tabs for indentation (use C-q <TAB> to insert real tabs)
(setq-default indent-tabs-mode nil)

;; less GUI elements
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (and (not (display-graphic-p)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))

;; read-only comint-mode prompts
(setq-default comint-prompt-read-only t)

;; allow narrowing commands
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; close term-mode and eshell-mode buffers on exit
(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  "Kill term buffer on term session end."
  (kill-buffer))

;; set backup directory
(setq backup-directory-alist '((".*" . "~/.backup")))

;; keep Customize settings in a separate file, ignore if file does not exist
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; set GUI font
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

(defun my-transpose-windows (selector)
  "Transpose buffers between current window and window after calling SELECTOR."
  (let* ((from-win (selected-window))
         (from-buf (window-buffer)))
    (funcall selector)
    (set-window-buffer from-win (window-buffer))
    (set-window-buffer (selected-window) from-buf)))

(defun my-enlarge-frame (w h)
  "Enlarge width, height of selected frame by W, H lines (shrink if negative)."
  (let* ((this-frame (selected-frame)))
    (set-frame-width this-frame (+ (frame-width this-frame) w))
    (set-frame-height this-frame (+ (frame-height this-frame) h))))

(defun my-move-frame (x y)
  "Move selected frame by X pixels horizontally and Y pixels vertically."
  (let* ((this-frame (selected-frame))
         (fpos (frame-position this-frame)))
    (set-frame-position this-frame (+ (car fpos) x) (+ (cdr fpos) y))))

(defun my-move-frame-pct (x y)
  "Move selected frame by X% and Y% of the display horizontally and vertically."
  (my-move-frame (* x (/ (x-display-pixel-width) 100))
                 (* y (/ (x-display-pixel-height) 100))))

;; regenerate outdated bytecode
(setq load-prefer-newer t)

;; packages (user) in ~/.emacs.d/lisp
(defvar lisp-dir (expand-file-name "lisp" user-emacs-directory))
(unless (file-exists-p lisp-dir)
  (make-directory lisp-dir))
(add-to-list 'load-path lisp-dir)

;; packages (third-party) in ~/.emacs.d/site-lisp and its subdirs
(defvar site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(unless (file-exists-p site-lisp-dir)
  (make-directory site-lisp-dir))
(add-to-list 'load-path site-lisp-dir)
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; ELPA-compatible package repositories
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

;; bootstrap use-package, https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'diminish)
  (require 'bind-key)
  (setq use-package-always-ensure t))

;; load evil first so other package defs can have evil bindings
(use-package evil
  :init
  (setq-default evil-want-C-u-scroll t) ;; set C-u to half-page up (like Vim)
  (evil-mode t)
  :config
  ;; emulate Vim leader key
  (defvar evil-leader "<SPC>")
  ;; functions for defining evil-leader bindings
  (defun evil-leader-set-key-normal (key fn)
    "Defines an evil normal mode keybinding prefixed with evil-leader."
    (define-key evil-normal-state-map (kbd (concat evil-leader key)) fn))
  (defun evil-leader-set-key-visual (key fn)
    "Defines an evil visual mode keybinding prefixed with evil-leader."
    (define-key evil-visual-state-map (kbd (concat evil-leader key)) fn))
  ;; set default states for specific modes
  (dolist (mode '(calculator-mode
                  comint-mode
                  dired-mode
                  eshell-mode
                  eww-mode
                  ibuffer-mode
                  inferior-emacs-lisp-mode
                  org-mode
                  shell-mode
                  term-mode))
    (evil-set-initial-state mode 'emacs))
  (dolist (mode '(diff-mode
                  special-mode))
    (evil-set-initial-state mode 'motion))
  ;; useful bracket mappings (modeled after vim-unimpaired)
  (define-key evil-normal-state-map (kbd "[ e")
    (lambda (n) (interactive "p")
      (dotimes (_ n)
        (progn (transpose-lines 1) (forward-line -2)))))
  (define-key evil-normal-state-map (kbd "] e")
    (lambda (n) (interactive "p")
      (dotimes (_ n)
        (progn (forward-line 1) (transpose-lines 1) (forward-line -1)))))
  (define-key evil-visual-state-map (kbd "[ e")
    (lambda (n) (interactive "p")
      (concat ":'<,'>move '<--" (number-to-string n))))
  (define-key evil-visual-state-map (kbd "] e")
    (lambda (n) (interactive "p")
      (concat ":'<,'>move '>+" (number-to-string n))))
  (define-key evil-normal-state-map (kbd "[ h") 'diff-hunk-prev)
  (define-key evil-normal-state-map (kbd "] h") 'diff-hunk-next)
  (define-key evil-normal-state-map (kbd "[ f") 'ns-next-frame)
  (define-key evil-normal-state-map (kbd "] f") 'ns-prev-frame)
  ;; evil-leader bindings
  (evil-leader-set-key-normal "b" 'switch-to-buffer)
  (evil-leader-set-key-normal "d" 'dired)
  (evil-leader-set-key-normal "e" 'find-file)
  (evil-leader-set-key-normal "k b" 'kill-buffer)
  (evil-leader-set-key-normal "k f" 'delete-frame)
  (evil-leader-set-key-normal "k w" 'delete-window)
  (evil-leader-set-key-normal "M" 'evil-show-marks)
  (evil-leader-set-key-normal "m f" 'make-frame)
  (evil-leader-set-key-normal "r" 'list-registers)
  (evil-leader-set-key-normal "w" 'whitespace-mode)
  (evil-leader-set-key-normal "y" (lambda () (interactive)
                                    (popup-menu 'yank-menu)))
  (evil-leader-set-key-visual "#" 'comment-or-uncomment-region))

;; load packages built on top of evil next
(use-package evil-surround
  :init
  (require 'evil)
  (global-evil-surround-mode 1))

;; load hydra next so package defs can have hydra defs and bindings
(use-package hydra
  :config
  (defhydra my-hydra/buffer (:color amaranth :columns 5)
    "Buffer"
    ("n" next-buffer "next")
    ("p" previous-buffer "previous")
    ("R" revert-buffer "revert")
    ("B" bury-buffer "bury")
    ("U" unbury-buffer "unbury")
    ("s" save-buffer "save")
    ("S" save-some-buffers "save-all")
    ("k" kill-this-buffer "kill")
    ("K" kill-matching-buffers "kill-match")
    ("c" clean-buffer-list "clean")
    ("L" (condition-case nil
             (quit-windows-on "*Buffer List*" t)
           (error (list-buffers))) "list")
    ("b" switch-to-buffer "switch" :color blue)
    ("q" nil "quit" :color blue))
  (defhydra my-hydra/desktop (:color teal)
    "Desktop"
    ("c" desktop-clear "clear")
    ("s" desktop-save "save")
    ("r" desktop-read "read")
    ("R" desktop-revert "revert")
    ("d" desktop-change-dir "dir")
    ("q" nil "quit"))
  (defhydra my-hydra/error (:color amaranth)
    "Error"
    ("n" next-error "next")
    ("p" previous-error "previous")
    ("f" first-error "first")
    ("l" (condition-case nil (while t (next-error)) (user-error nil)) "last")
    ("q" nil "quit" :color blue))
  (defhydra my-hydra/frame (:color amaranth :columns 4)
    "Frame"
    ("n" ns-next-frame "next")
    ("p" ns-prev-frame "previous")
    ("s" select-frame-by-name "select")
    ("M" toggle-frame-maximized "maximize")
    ("+" (lambda (n) (interactive "p") (my-enlarge-frame 0 n)) "enlarge-v")
    ("-" (lambda (n) (interactive "p") (my-enlarge-frame 0 (- n))) "shrink-v")
    (">" (lambda (n) (interactive "p") (my-enlarge-frame n 0)) "enlarge-h")
    ("<" (lambda (n) (interactive "p") (my-enlarge-frame (- n) 0)) "shrink-h")
    ("}" (lambda (n) (interactive "p") (my-move-frame-pct 0 n)) "move-d")
    ("{" (lambda (n) (interactive "p") (my-move-frame-pct 0 (- n))) "move-u")
    (")" (lambda (n) (interactive "p") (my-move-frame-pct n 0)) "move-r")
    ("(" (lambda (n) (interactive "p") (my-move-frame-pct (- n) 0)) "move-l")
    ("m" make-frame "make")
    ("d" delete-frame "delete")
    ("o" delete-other-frames "only")
    ("q" nil "quit" :color blue))
  (defhydra my-hydra/narrow (:color teal)
    "Narrow"
    ("n" narrow-to-region "region")
    ("p" narrow-to-page "page")
    ("d" narrow-to-defun "defun")
    ("w" widen "widen")
    ("q" quit :color blue))
  (defhydra my-hydra/org-mode (:color amaranth :columns 2)
    "Org Mode Navigation"
    ("n" outline-next-visible-heading "next heading")
    ("p" outline-previous-visible-heading "prev heading")
    ("N" org-forward-heading-same-level "next heading at same level")
    ("P" org-backward-heading-same-level "prev heading at same level")
    ("u" outline-up-heading "up heading")
    ("<tab>" outline-toggle-children "toggle children")
    ("g" org-goto "goto" :color blue)
    ("q" nil "quit" :color blue))
  (defhydra my-hydra/search (:color teal :columns 3)
    "Search"
    ("gg" grep "grep")
    ("gr" rgrep "rgrep")
    ("gl" lgrep "lgrep")
    ("gf" grep-find "grep-find")
    ("gz" rzgrep "zrgrep")
    ("gd" grep-find-dired "grep-find-dired")
    ("oo" occur "occur")
    ("om" multi-occur "multi-occur")
    ("ob" multi-occur-in-matching-buffers "multi-occur-match-buf")
    ("oO" org-occur "org-occur")
    ("kg" kill-grep "kill-grep")
    ("q"  nil "quit"))
  (defhydra my-hydra/window (:color amaranth :columns 4)
    "Window"
    ("h" windmove-left "left")
    ("j" windmove-down "down")
    ("k" windmove-up "up")
    ("l" windmove-right "right")
    ("H" (my-transpose-windows 'windmove-left) "transpose-l")
    ("J" (my-transpose-windows 'windmove-down) "transpose-d")
    ("K" (my-transpose-windows 'windmove-up) "transpose-u")
    ("L" (my-transpose-windows 'windmove-right) "transpose-r")
    ("+" enlarge-window "enlarge-v")
    ("-" shrink-window "shrink-v")
    (">" enlarge-window-horizontally "enlarge-h")
    ("<" shrink-window-horizontally "shrink-h")
    ("v" split-window-right "split-v")
    ("s" split-window-below "split-h")
    ("=" balance-windows "balance")
    ("_" balance-windows-area "balance-area")
    ("d" delete-window "delete")
    ("D" delete-windows-on "delete-match")
    ("o" delete-other-windows "only")
    ("q" nil "quit" :color blue))
  (defhydra my-hydra/zoom (:color amaranth)
    "Zoom"
    ("+" text-scale-increase "in")
    ("-" text-scale-decrease "out")
    ("0" (text-scale-adjust 0) "reset")
    ("q" nil "quit" :color blue))
  (global-set-key (kbd "C-c b") 'my-hydra/buffer/body)
  (global-set-key (kbd "C-c d") 'my-hydra/desktop/body)
  (global-set-key (kbd "C-c e") 'my-hydra/error/body)
  (global-set-key (kbd "C-c f") 'my-hydra/frame/body)
  (global-set-key (kbd "C-c n") 'my-hydra/narrow/body)
  (global-set-key (kbd "C-c o") 'my-hydra/org-mode/body)
  (global-set-key (kbd "C-c s") 'my-hydra/search/body)
  (global-set-key (kbd "C-c w") 'my-hydra/window/body)
  (global-set-key (kbd "C-c z") 'my-hydra/zoom/body))

(use-package company
  :init
  (setq-default company-dabbrev-ignore-case t)
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package csv-mode)

(use-package elpy
  :init (with-eval-after-load 'python (elpy-enable)))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (when (featurep 'hydra)
    (defhydra my-hydra/flycheck (:color amaranth :columns 6)
      "Error"
      ("F" flycheck-error-list-set-filter "filter")
      ("n" flycheck-next-error "next")
      ("p" flycheck-previous-error "previous")
      ("f" flycheck-first-error "first")
      ("l" (condition-case nil
               (while t (flycheck-next-error))
             (user-error nil)) "last")
      ("L" (condition-case nil
               (quit-windows-on "*Flycheck errors*" t)
             (error (flycheck-list-errors))) "list")
      ("q" nil "quit" :color blue))
    ;; bind over my-hydra/error
    (global-set-key (kbd "C-c e") 'my-hydra/flycheck/body))
  (when (featurep 'evil)
    (evil-set-initial-state 'flycheck-error-list-mode 'emacs)
    (define-key evil-normal-state-map (kbd "[ l") 'flycheck-previous-error)
    (define-key evil-normal-state-map (kbd "] l") 'flycheck-next-error)))

(use-package gruvbox-theme
  :config (load-theme 'gruvbox t))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (when (featurep 'evil)
    (evil-leader-set-key-normal "B" 'ibuffer)))

(use-package ido
  :init
  (setq ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        ido-enable-flex-matching t
        ido-everywhere t
        ido-use-virtual-buffers t)
  (ido-mode t))

(use-package ido-ubiquitous
  :init (ido-ubiquitous-mode t))

(use-package lispy
  :bind ("C-c l" . lispy-mode))

(use-package magit
  :bind ("C-c g" . magit-status)
  :config
  (when (featurep 'evil)
    ;; default to Emacs state in magit modes
    (dolist (mode '(magit-mode magit-popup-mode magit-repolist-mode))
      (evil-set-initial-state mode 'emacs))))

(use-package projectile
  :init (projectile-global-mode)
  :config
    (setq projectile-switch-project-action 'projectile-commander)
    (if (featurep 'hydra)
        (progn
          (defhydra my-hydra/projectile (:color teal :hint nil)
            "
Projectile: %(projectile-project-root)

Buffer  _bb_  : switch buffer              _bi_  : ibuffer
        _bk_  : kill buffers               _bo_  : switch buffer (other window)
      
File    _ff_  : find file                  _fw_  : find file dwim
        _fd_  : find file in dir           _fp_  : find file in known projects
        _fof_ : find file (other window)   _fow_ : find file dwim (other window)
        _fr_  : recent files

Dir     _dd_  : find dir                   _do_  : find dir (other window)

Search  _sa_  : ag                         _sg_  : grep
        _so_  : multi-occur

Cache   _cc_  : cache current file         _cC_  : clear cache
        _cx_  : remove known project       _cX_  : cleanup known projects

"
            ("bb"  projectile-switch-to-buffer)
            ("bi"  projectile-ibuffer)
            ("bk"  projectile-kill-buffers)
            ("bo"  projectile-switch-to-buffer-other-window)
            ("ff"  projectile-find-file)
            ("fw"  projectile-find-file-dwim)
            ("fd"  projectile-find-file-in-directory)
            ("fp"  projectile-find-file-in-known-projects)
            ("fof" projectile-find-file-other-window)
            ("fow" projectile-find-file-dwim-other-window)
            ("fr"  projectile-recentf)
            ("dd"  projectile-find-dir)
            ("do"  projectile-find-dir-other-window)
            ("sa"  projectile-ag)
            ("sg"  projectile-grep)
            ("so"  projectile-multi-occur)
            ("cc"  projectile-cache-current-file)
            ("cC"  projectile-invalidate-cache)
            ("cx"  projectile-remove-known-project)
            ("cX"  projectile-cleanup-known-projects)
            ("C"   projectile-compile-project "compile")
            ("p"   projectile-switch-project "switch project")
            ("q"   nil "quit" :color blue))
          (global-set-key (kbd "C-c P") 'my-hydra/projectile/body))
      (global-set-key (kbd "C-c P") 'projectile-commander)))

(use-package rainbow-delimiters
  :bind ("C-c r" . rainbow-delimiters-mode))

(use-package recentf
  :bind ("C-c F" . recentf-open-files)
  :init (recentf-mode t)
  :config
  (setq recentf-max-menu-items 10
        recentf-max-saved-items 50)
  (when (featurep 'evil)
    (evil-leader-set-key-normal "f" 'recentf-open-files)))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config (smex-initialize))

(use-package undo-tree
  :diminish undo-tree-mode
  :bind ("C-c u" . undo-tree-visualize)
  :init (global-undo-tree-mode)
  :config
  (when (featurep 'evil)
    (setq evil-want-fine-undo t)
    (evil-leader-set-key-normal "u" 'undo-tree-visualize)))

(provide 'init)
;;; init.el ends here
