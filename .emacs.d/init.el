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

;; scroll one line at a time when cursor moves past window top/bottom
(setq scroll-conservatively 101)

;; show column number in modeline
(setq column-number-mode t)

;; show matching parentheses without delay
(setq show-paren-delay 0)
(show-paren-mode t)

;; indent with soft tabs. Use C-q <TAB> for real tabs
(setq-default indent-tabs-mode nil)

;; guess default target dir for dired file ops using dired buffers in next window
(setq dired-dwim-target t)

;; remove unused GUI elements
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (and (not (display-graphic-p)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))

;; smooth scrolling in GUI (hold shift/control for 5 lines/full screen)
(if (display-graphic-p)
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control)))))

;; read-only comint-mode prompts
(setq comint-prompt-read-only t)

;; close term-mode and eshell-mode buffers on exit
(defadvice term-handle-exit (after term-kill-buffer-on-exit activate)
  "Kill term buffer on term session end."
  (kill-buffer))

;; set backup directory
(setq backup-directory-alist '((".*" . "~/.backup")))

;; store Customize settings in separate file if it exists
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; set GUI font
(defvar my-font (if (eq system-type 'darwin) "Menlo" "DejaVu Sans Mono"))
(defvar my-font-height (if (eq system-type 'darwin) 140 110))
(defvar my-font-weight (if (eq system-type 'darwin) 'light 'normal))
(defvar my-font-width 'normal)
(if (and (display-graphic-p)
         (and my-font (not (string= my-font "")))
         (x-list-fonts my-font))
  (set-face-attribute 'default nil
                      :family my-font
                      :height my-font-height
                      :weight my-font-weight
                      :width my-font-width))

;; use left Option key as Meta and preserve right Option key on Mac OS X
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
        mac-right-option-modifier nil))

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
  "Move selected frame within display by X% horizontally and Y% vertically."
  (my-move-frame (* x (/ (x-display-pixel-width) 100))
                 (* y (/ (x-display-pixel-height) 100))))

;; regenerate outdated bytecode
;; (setq load-prefer-newer t)

;; user packages in ~/.emacs.d/lisp
(defvar lisp-dir (expand-file-name "lisp" user-emacs-directory))
(unless (file-exists-p lisp-dir) (make-directory lisp-dir))
(add-to-list 'load-path lisp-dir)

;; third-party packages in ~/.emacs.d/site-lisp and its subdirectories
(defvar site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(unless (file-exists-p site-lisp-dir) (make-directory site-lisp-dir))
(add-to-list 'load-path site-lisp-dir)
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project) (add-to-list 'load-path project)))

;; load local pre-init file ~/.emacs.d/init-local-pre.el
(let ((local-f (expand-file-name "init-local-pre.el" user-emacs-directory)))
  (if (file-exists-p local-f) (load-file local-f)))

;; use package.el with given ELPA-compatible package repositories
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ;; ("MELPA"        . "https://melpa.org/packages/")
       )
      package-archive-priorities
      '(("GNU ELPA"     . 10)
        ("MELPA Stable" . 5)
        ;; ("MELPA"        . 0)
       ))

(package-initialize)

;; bootstrap use-package ( https://github.com/jwiegley/use-package )
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key)
  (setq use-package-always-ensure t))

;; copies env vars from shell - MELPA Stable
(when (eq system-type 'darwin)  ;; only for Mac OS X GUI mode
  (use-package exec-path-from-shell
    :init
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize))))

;; extensible vi layer for Emacs - MELPA Stable
(use-package evil
  :init
  ;; use C-z to toggle between Evil and Emacs bindings, C-x C-z to suspend
  (setq evil-want-C-u-scroll t ;; C-u goes half-page up like in Vim
        evil-insert-state-modes nil ;; clear Insert state modes
        evil-motion-state-modes nil ;; clear Motion state modes
        evil-default-state 'emacs) ;; use Emacs state as default
  (evil-mode t)
  :config
  (defvar evil-leader "<SPC>") ;; emulate Vim leader key in normal mode
  (defun evil-leader-set-key-normal (key fn)
    "Binds \"<evil-leader> KEY\" to interactively call FN in Evil normal mode."
    (define-key evil-normal-state-map (kbd (concat evil-leader key)) fn))
  (defun evil-leader-set-key-visual (key fn)
    "Binds \"<evil-leader> KEY\" to interactively call FN in Evil visual mode."
    (define-key evil-visual-state-map (kbd (concat evil-leader key)) fn))
  (evil-leader-set-key-normal "b" 'switch-to-buffer)
  (evil-leader-set-key-normal "d" 'dired)
  (evil-leader-set-key-normal "e" 'find-file)
  (evil-leader-set-key-normal "M" 'evil-show-marks)
  (evil-leader-set-key-normal "r" 'list-registers)
  (evil-leader-set-key-normal "w" 'whitespace-mode)
  (evil-leader-set-key-normal "y" (lambda () (interactive)
                                    (popup-menu 'yank-menu)))
  (evil-leader-set-key-normal "#" 'comment-line)
  (evil-leader-set-key-visual "#" 'comment-or-uncomment-region)
  ;; make tabs in insert mode work like Vim
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
  ;; useful bracket mappings like in vim-unimpaired
  (define-key evil-normal-state-map (kbd "[ e")
    (lambda (n) (interactive "p")
      (dotimes (_ n)
        (unless (eq (string-to-number (format-mode-line "%l")) 1)
          (progn (transpose-lines 1)
                 (forward-line -2))))))
  (define-key evil-normal-state-map (kbd "] e")
    (lambda (n) (interactive "p")
      (dotimes (_ n)
        (unless (eq (string-to-number (format-mode-line "%l"))
                    (line-number-at-pos (point-max)))
          (progn (forward-line 1)
                 (transpose-lines 1)
                 (forward-line -1))))))
  (define-key evil-normal-state-map (kbd "[ l") 'previous-error)
  (define-key evil-normal-state-map (kbd "] l") 'next-error)
  (define-key evil-normal-state-map (kbd "[ n") 'diff-hunk-prev)
  (define-key evil-normal-state-map (kbd "] n") 'diff-hunk-next))

;; mappings for adding, changing and deleting surrounding brackets
;; in evil-mode (emulates surround.vim by tpope) - MELPA Stable
(use-package evil-surround
  :after evil
  :init (global-evil-surround-mode 1))

;; framework for temporary or repeatable keybindings - MELPA Stable
(use-package hydra
  :config
  (defhydra my-hydra/buffer (:color amaranth :columns 5)
    "Buffer"
    ("p" previous-buffer "previous")
    ("n" next-buffer "next")
    ("R" revert-buffer "revert")
    ("B" bury-buffer "bury")
    ("U" unbury-buffer "unbury")
    ("s" save-buffer "save")
    ("S" save-some-buffers "save-all")
    ("k" kill-this-buffer "kill")
    ("K" kill-matching-buffers "kill-match")
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
    ("p" previous-error "previous")
    ("n" next-error "next")
    ("f" first-error "first")
    ("l" (condition-case nil (while t (next-error)) (user-error nil)) "last")
    ("q" nil "quit" :color blue))
  (defhydra my-hydra/frame (:color amaranth :columns 4)
    "Frame"
    ("p" (other-frame -1) "previous")
    ("n" other-frame "next")
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
    ("q" nil "quit" :color blue))
  (defhydra my-hydra/navigation (:color amaranth :columns 4)
    "Navigation"
    ("S-SPC" scroll-down "page-up")
    ("SPC" scroll-up "pg-down")
    ("<" scroll-right "pg-left")
    (">" scroll-left "pg-right")
    ("C-SPC" set-mark-command "set-mark")
    ("x" exchange-point-and-mark "xchg-mark")
    ("r SPC" point-to-register "pt-to-reg")
    ("rj" jump-to-register "jmp-to-reg")
    ("rm" bookmark-set "bmk-set")
    ("rb" bookmark-jump "bmk-jmp")
    ("gg" beginning-of-buffer "beg-buf")
    ("gG" end-of-buffer "end-buf")
    ("G" goto-line "goto-line")
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
    ("rs" query-replace "replace string")
    ("rr" query-replace-regexp "replace regexp")
    ("kg" kill-grep "kill-grep")
    ("q" nil "quit"))
  (defhydra my-hydra/window (:color amaranth :columns 4)
    "Window"
    ("n" next-multiframe-window "next")
    ("p" previous-multiframe-window "previous")
    ("v" split-window-right "split-v")
    ("s" split-window-below "split-h")
    ("h" windmove-left "left")
    ("j" windmove-down "down")
    ("k" windmove-up "up")
    ("l" windmove-right "right")
    ("H" (my-transpose-windows 'windmove-left) "transpose-l")
    ("J" (my-transpose-windows 'windmove-down) "transpose-d")
    ("K" (my-transpose-windows 'windmove-up) "transpose-u")
    ("L" (my-transpose-windows 'windmove-right) "transpose-r")
    ("-" shrink-window "shrink-v")
    ("+" enlarge-window "enlarge-v")
    ("<" shrink-window-horizontally "shrink-h")
    (">" enlarge-window-horizontally "enlarge-h")
    ("M" minimize-window "minimize")
    ("m" maximize-window "maximize")
    ("=" balance-windows "balance")
    ("_" balance-windows-area "balance-area")
    ("o" delete-other-windows "only")
    ("d" delete-window "delete")
    ("D" kill-buffer-and-window "delete-buf")
    ("q" nil "quit" :color blue))
  (defhydra my-hydra/zoom (:color amaranth)
    "Zoom"
    ("-" text-scale-decrease "out")
    ("+" text-scale-increase "in")
    ("0" (text-scale-adjust 0) "reset")
    ("q" nil "quit" :color blue))
  (global-set-key (kbd "C-c h D") 'my-hydra/desktop/body)
  (global-set-key (kbd "C-c h N") 'my-hydra/narrow/body)
  (global-set-key (kbd "C-c h s") 'my-hydra/search/body)
  (global-set-key (kbd "C-c h Z") 'my-hydra/zoom/body)
  (global-set-key (kbd "C-c h b") 'my-hydra/buffer/body)
  (global-set-key (kbd "C-c h e") 'my-hydra/error/body)
  (global-set-key (kbd "C-c h f") 'my-hydra/frame/body)
  (global-set-key (kbd "C-c h n") 'my-hydra/navigation/body)
  (global-set-key (kbd "C-c h w") 'my-hydra/window/body))

;; display available keybindings in popup - GNU ELPA
(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-compute-remaps t
        which-key-allow-multiple-replacements t)
  (which-key-mode 1)
  (global-set-key (kbd "C-c h W") 'which-key-show-top-level))

;; text completion framework - MELPA Stable
(use-package company
  :diminish company-mode
  :init
  (setq company-selection-wrap-around t
        company-dabbrev-downcase nil
        company-idle-delay 0.25
        company-minimum-prefix-length 2)
  (company-tng-configure-default) ;; Tab and Go behavior
  (add-hook 'after-init-hook 'global-company-mode))

;; Ediff - built-in
(use-package ediff
  :config
  (with-eval-after-load 'hydra
     (defhydra my-hydra/ediff (:color teal :hint nil)
       "
Ediff

Buffer   _b_ : 2-way       _B_ : 3-way

Files    _f_ : 2-way       _F_ : 3-way       _c_ : current

Region   _l_ : line-wise   _w_ : word-wise

Windows  _L_ : line-wise   _W_ : word-wise

"
      ("b" ediff-buffers)
      ("B" ediff-buffers3)
      ("f" ediff-files)
      ("F" ediff-files3)
      ("c" ediff-current-file)
      ("l" ediff-regions-linewise)
      ("w" ediff-regions-wordwise)
      ("L" ediff-windows-linewise)
      ("W" ediff-windows-wordwise)
      ("q" nil "quit" :color blue))
    (global-set-key (kbd "C-c h d") 'my-hydra/ediff/body)))

;; syntax checker, replaces Flymake - MELPA Stable
(use-package flycheck
  :diminish flycheck-mode
  :init (global-flycheck-mode)
  :config
  (with-eval-after-load 'hydra
    (defhydra my-hydra/flycheck (:color amaranth :columns 6)
      "Error"
      ("F" flycheck-error-list-set-filter "filter")
      ("p" flycheck-previous-error "previous")
      ("n" flycheck-next-error "next")
      ("f" flycheck-first-error "first")
      ("l" (condition-case nil (while t (flycheck-next-error))
             (user-error nil)) "last")
      ("L" (condition-case nil (quit-windows-on "*Flycheck errors*" t)
             (error (flycheck-list-errors))) "list")
      ("q" nil "quit" :color blue))
    ;; bind over my-hydra/error
    (define-key flycheck-mode-map (kbd "C-c h e") 'my-hydra/flycheck/body))
  (with-eval-after-load 'evil
    ;; bind over error navigation bracket mappings
    (define-key evil-normal-state-map (kbd "[ l") 'flycheck-previous-error)
    (define-key evil-normal-state-map (kbd "] l") 'flycheck-next-error)))

;; advanced buffer menu - built-in
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (with-eval-after-load 'evil
    (evil-leader-set-key-normal "B" 'ibuffer)))

;; interactively do things with buffers and files, use C-f to escape - built-in
(use-package ido
  :init
  (setq ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        ido-enable-flex-matching t
        ido-everywhere t
        ido-use-virtual-buffers t)
  (ido-mode t))

;; replaces stock completion with ido wherever possible - MELPA Stable
(use-package ido-completing-read+
  :init (ido-ubiquitous-mode t))

;; project interaction library - MELPA Stable
(use-package projectile
  :init (projectile-mode)
  :config
  (setq-default projectile-mode-line
                '(:eval (if (file-remote-p default-directory)
                            " Proj"
                          (format " Proj[%s]" (projectile-project-name)))))
  (setq projectile-switch-project-action 'projectile-commander)
  ;; define function to use for grepping in projectile, prefer ripgrep to grep
  (if (executable-find "rg")
      (progn
        (use-package projectile-ripgrep) ;; load ripgrep support
        (defalias 'my-projectile-search-fun 'projectile-ripgrep))
    (defalias 'my-projectile-search-fun 'projectile-grep))
  (with-eval-after-load 'hydra
    (defhydra my-hydra/projectile (:color teal :hint nil)
      "
Projectile: %(projectile-project-root)

Buffer  _bb_  : switch to buffer          _bi_  : ibuffer
        _bk_  : kill buffers              _bo_  : switch buffer (other window)

File    _ff_  : find file                 _fw_  : find file dwim
        _fd_  : find file in dir          _fp_  : find file in known projects
        _fof_ : find file (other window)  _fow_ : find file dwim (other window)
        _fr_  : recent files

Dir     _dd_  : find dir                  _do_  : find dir (other window)

Search  _sg_  : grep / ripgrep            _so_  : multi-occur
        _rs_  : replace string            _rr_  : replace regexp

Cache   _cc_  : cache current file        _cC_  : clear cache
        _cx_  : remove known project      _cX_  : cleanup known projects

"
      ("bb" projectile-switch-to-buffer)
      ("bi" projectile-ibuffer)
      ("bk" projectile-kill-buffers)
      ("bo" projectile-switch-to-buffer-other-window)
      ("ff" projectile-find-file)
      ("fw" projectile-find-file-dwim)
      ("fd" projectile-find-file-in-directory)
      ("fp" projectile-find-file-in-known-projects)
      ("fof" projectile-find-file-other-window)
      ("fow" projectile-find-file-dwim-other-window)
      ("fr" projectile-recentf)
      ("dd" projectile-find-dir)
      ("do" projectile-find-dir-other-window)
      ("sg" my-projectile-search-fun)
      ("so" projectile-multi-occur)
      ("rs" projectile-replace)
      ("rr" projectile-replace-regexp)
      ("cc" projectile-cache-current-file)
      ("cC" projectile-invalidate-cache)
      ("cx" projectile-remove-known-project)
      ("cX" projectile-cleanup-known-projects)
      ("C" projectile-compile-project "compile")
      ("p" projectile-switch-project "switch project")
      ("q" nil "quit" :color blue))
    (define-key projectile-mode-map (kbd "C-c h p") 'my-hydra/projectile/body)))

;; recently opened files - built-in
(use-package recentf
  :bind ("C-c F" . recentf-open-files)
  :init (recentf-mode t)
  :config
  (setq recentf-max-menu-items 10
        recentf-max-saved-items 50)
  (with-eval-after-load 'evil
    (evil-leader-set-key-normal "F" 'recentf-open-files)))

;; smart M-x enhancements - MELPA Stable
(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config (smex-initialize))

;; traverse undo history as a tree - GNU ELPA
(use-package undo-tree
  :diminish undo-tree-mode
  :bind ("C-c u" . undo-tree-visualize)
  :init (global-undo-tree-mode)
  :config
  (with-eval-after-load 'evil
    (setq evil-want-fine-undo t)
    (evil-leader-set-key-normal "u" 'undo-tree-visualize)))

;; Eldoc - built-in
(use-package eldoc
  :diminish eldoc-mode
  :init (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

;; Eshell - built-in
(use-package eshell
  :commands (eshell eshell-command)
  :init
  (require 'em-term)
  (require 'em-smart)
  (setq eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t
        eshell-where-to-jump 'begin)
  :config
  (add-to-list 'eshell-visual-commands "htop")
  (add-to-list 'eshell-visual-commands "lftp")
  (add-to-list 'eshell-visual-commands "ssh")
  (add-to-list 'eshell-visual-commands "vim")
  (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))
  (add-to-list 'eshell-visual-subcommands '("vagrant" "ssh")))

;; Git - MELPA Stable
(when (executable-find "git")
  (use-package magit
    :init (setq vc-handled-backends (delq 'Git vc-handled-backends))
    :config (global-set-key (kbd "C-c g g") 'magit-status))
  (use-package git-timemachine
    :after magit
    :config (global-set-key (kbd "C-c g t") 'git-timemachine)))

;; Vim Tagbar-like imenu extension - MELPA Stable
(use-package imenu-list
  :bind ("C-c i" . imenu-list-smart-toggle)
  :init (setq imenu-list-focus-after-activation t
              imenu-list-auto-resize t))

;; Org-mode - built-in
(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link))
  :config
  (require 'org-agenda)
  (setq org-agenda-start-on-weekday nil
        org-catch-invisible-edits 'error
        org-log-into-drawer t
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANCELED(c@/!)"))
        org-use-fast-todo-selection t
        org-use-speed-commands t
        org-startup-indented t)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (defhydra my-hydra/org-agenda (:color amaranth :hint nil)
    "
Org agenda

Headline    _ht_  : set status   _hk_  : kill         _hr_  : refile
            _hA_  : archive      _h:_  : set tags     _hp_  : set priority

Visit Entry _SPC_ : other window _TAB_ : & go to loc  _RET_ : & del other wins
            _o_   : link

Date        _ds_  : schedule     _dd_  : set deadline _dt_  : timestamp

View        _vd_  : day          _vw_  : week         _vm_  : month
            _vn_  : next span    _vp_  : prev span    _vr_  : reset

Filter      _ft_  : by tag       _fc_  : by category  _fh_  : by top headline
            _fx_  : by regex     _fd_  : reset

Clock       _ci_  : in           _co_  : out          _cq_  : cancel
            _cg_  : goto

Other       _gr_  : reload       _gd_  : go to date   _._   : go to today

"
    ("ht" org-agenda-todo)
    ("hk" org-agenda-kill)
    ("hr" org-agenda-refile)
    ("hA" org-agenda-archive-default)
    ("h:" org-agenda-set-tags)
    ("hp" org-agenda-priority)
    ("SPC" org-agenda-show-and-scroll-up)
    ("TAB" org-agenda-goto :color blue)
    ("RET" org-agenda-switch-to :color blue)
    ("o" link-hint-open-link :color blue)
    ("ds" org-agenda-schedule)
    ("dd" org-agenda-deadline)
    ("dt" org-agenda-date-prompt)
    ("vd" org-agenda-day-view)
    ("vw" org-agenda-week-view)
    ("vm" org-agenda-month-view)
    ("vn" org-agenda-later)
    ("vp" org-agenda-earlier)
    ("vr" org-agenda-reset-view)
    ("ft" org-agenda-filter-by-tag)
    ("fc" org-agenda-filter-by-category)
    ("fh" org-agenda-filter-by-top-headline)
    ("fx" org-agenda-filter-by-regexp)
    ("fd" org-agenda-filter-remove-all)
    ("ci" org-agenda-clock-in :color blue)
    ("co" org-agenda-clock-out)
    ("cq" org-agenda-clock-cancel)
    ("cg" org-agenda-clock-goto :color blue)
    ("gr" org-agenda-redo)
    ("gd" org-agenda-goto-date)
    ("." org-agenda-goto-today)
    ("q" nil "quit" :color blue))
  (define-key org-agenda-mode-map (kbd "C-c h m") 'my-hydra/org-agenda/body))

;; Visit large files without loading it entirely - MELPA Stable
(use-package vlf
  :config (require 'vlf-setup))

;; template systems, i.e. expandable snippets - GNU ELPA
(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode 1)
  :config
  ;; official snippets - MELPA Stable
  (use-package yasnippet-snippets)
  ;; create temporary snippets - MELPA Stable
  (use-package auto-yasnippet)
  ;; disable tab for snippet expansion to avoid conflicts with company-mode
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; use Ctrl-Shift-Space for snippet expansion
  (define-key yas-minor-mode-map (kbd "<C-S-spc>") #'yas-expand)
  (with-eval-after-load 'hydra
    (defhydra my-hydra/yasnippet (:color teal :columns 3)
      "YASnippet"
      ("SPC" yas-expand "expand") ;; expand snippet
      ("d" yas-describe-tables "describe") ;; describe snippets for current mode
      ("w" aya-create "create-auto") ;; store temp yasnippet
      ("y" aya-expand "expand-auto") ;; paste stored temp yasnippet
      ("?" (message "Current auto-yasnippet:\n%s" aya-current) "current-auto") ;; print stored temp yasnippet
      ("q" nil "quit"))
    (global-set-key (kbd "C-c h y") 'my-hydra/yasnippet/body))
  (define-key yas-minor-mode-map (kbd "C-S-SPC") #'yas-expand))

;; load local post-init file ~/.emacs.d/init-local-post.el
(let ((local-f (expand-file-name "init-local-post.el" user-emacs-directory)))
  (if (file-exists-p local-f) (load-file local-f)))

(provide 'init)
;;; init.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
