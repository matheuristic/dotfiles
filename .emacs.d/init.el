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

;; indent with soft tabs. Use C-q <TAB> to insert real tabs
(setq-default indent-tabs-mode nil)

;; simplify GUI
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (and (not (display-graphic-p)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))

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

;; use left Option key as Meta on Mac OS X
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier nil))

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
(setq load-prefer-newer t)

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

;; use package.el with given ELPA-compatible package repositories
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"     . 10)
        ("MELPA Stable" . 5)
        ("MELPA"        . 0)))

(package-initialize)

;; bootstrap use-package ( https://github.com/jwiegley/use-package )
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'diminish)
  (require 'bind-key)
  (setq use-package-always-ensure t))

;; extensible vi layer for Emacs
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
  (evil-leader-set-key-visual "#" 'comment-or-uncomment-region)
  ;; make tabs in insert mode work like Vim
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
  ;; useful bracket mappings like in vim-unimpaired
  (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "] b") 'next-buffer)
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

;; framework for creating temporary or repeatable keybindings
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
    ("p" ns-prev-frame "previous")
    ("n" ns-next-frame "next")
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
    ("," backward-sexp "bkwd-sexp")
    ("." forward-sexp "fwd-sexp")
    ("[" backward-list "bkwd-list")
    ("]" forward-list "fwd-list")
    ("u" up-list "up-list")
    ("d" down-list "down-list")
    ("U" beginning-of-defun "beg-defun")
    ("D" end-of-defun "end-defun")
    ("(" backward-sentence "bkwd-sntc")
    (")" forward-sentence "fwd-sntc")
    ("{" backward-paragraph "bkwd-par")
    ("}" forward-paragraph "fwd-par")
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
    ("q"  nil "quit"))
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
  (global-set-key (kbd "C-c b") 'my-hydra/buffer/body)
  (global-set-key (kbd "C-c d") 'my-hydra/desktop/body)
  (global-set-key (kbd "C-c e") 'my-hydra/error/body)
  (global-set-key (kbd "C-c f") 'my-hydra/frame/body)
  (global-set-key (kbd "C-c n") 'my-hydra/navigation/body)
  (global-set-key (kbd "C-c N") 'my-hydra/narrow/body)
  (global-set-key (kbd "C-c s") 'my-hydra/search/body)
  (global-set-key (kbd "C-c w") 'my-hydra/window/body)
  (global-set-key (kbd "C-c z") 'my-hydra/zoom/body))

;; text completion framework
(use-package company
  :diminish company-mode
  :init
  (setq company-selection-wrap-around t)
  (add-hook 'after-init-hook 'global-company-mode))

;; emulates surround.vim ( https://github.com/tpope/vim-surround )
(use-package evil-surround
  :after evil
  :init (global-evil-surround-mode 1))

;; copies environment variables from the shell
(use-package exec-path-from-shell
  :init
  ;; use only in Mac OS X GUI mode
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; highlight FIXME, TODO, BUG in comments
(use-package fic-mode
  :diminish fic-mode
  :config (add-hook 'prog-mode-hook 'fic-mode))

;; syntax checker (alternative to Flymake)
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
    (define-key flycheck-mode-map (kbd "C-c e") 'my-hydra/flycheck/body))
  (with-eval-after-load 'evil
    ;; bind over bracket mappings for error navigation
    (define-key evil-normal-state-map (kbd "[ l") 'flycheck-previous-error)
    (define-key evil-normal-state-map (kbd "] l") 'flycheck-next-error)))

;; color scheme
(use-package gruvbox-theme
  :config (load-theme 'gruvbox t))

;; advanced buffer menu
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (with-eval-after-load 'evil
    (evil-leader-set-key-normal "B" 'ibuffer)))

;; interactively do things with buffers and files
;; new files should be created without ido using C-x C-f C-f
(use-package ido
  :init
  (setq ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        ido-enable-flex-matching t
        ido-everywhere t
        ido-use-virtual-buffers t)
  (ido-mode t))

;; replaces stock completion with ido wherever possible
(use-package ido-completing-read+
  :init (ido-ubiquitous-mode t))

;; project interaction library
(use-package projectile
  :init (projectile-global-mode)
  :config
  (setq projectile-switch-project-action 'projectile-commander)
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

Search  _sg_  : grep                      _so_  : multi-occur
        _rs_  : replace string            _rr_  : replace regexp

Cache   _cc_  : cache current file        _cC_  : clear cache
        _cx_  : remove known project      _cX_  : cleanup known projects

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
      ("sg"  projectile-grep)
      ("so"  projectile-multi-occur)
      ("rs"  projectile-replace)
      ("rr"  projectile-replace-regexp)
      ("cc"  projectile-cache-current-file)
      ("cC"  projectile-invalidate-cache)
      ("cx"  projectile-remove-known-project)
      ("cX"  projectile-cleanup-known-projects)
      ("C"   projectile-compile-project "compile")
      ("p"   projectile-switch-project "switch project")
      ("q"   nil "quit" :color blue))
    (define-key projectile-mode-map (kbd "C-c P") 'my-hydra/projectile/body)))

;; colorize parentheses, brackets and braces according to depth
(use-package rainbow-delimiters
  :bind ("C-c r" . rainbow-delimiters-mode))

;; list recently opened files
(use-package recentf
  :bind ("C-c F" . recentf-open-files)
  :init (recentf-mode t)
  :config
  (setq recentf-max-menu-items 10
        recentf-max-saved-items 50)
  (with-eval-after-load 'evil
    (evil-leader-set-key-normal "F" 'recentf-open-files)))

;; smart M-x enhancements
(use-package smex
  ;; bind over executed-extended-command
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config (smex-initialize))

;; traverse undo history as a tree
(use-package undo-tree
  :diminish undo-tree-mode
  :bind ("C-c u" . undo-tree-visualize)
  :init (global-undo-tree-mode)
  :config
  (with-eval-after-load 'evil
    (setq evil-want-fine-undo t)
    (evil-leader-set-key-normal "u" 'undo-tree-visualize)))

;; CSV
(use-package csv-mode)

;; Eshell
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

;; Git
(when (executable-find "git")
  (use-package magit
    :bind ("C-c g" . magit-status)
    :config (setq vc-handled-backends (delq 'Git vc-handled-backends))))

;; Go
(when (executable-find "go")
  (use-package go-mode
    :commands go-mode
    :config
    (add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
    (if (executable-find "goimports")
        (setq gofmt-command "goimports")))
  ;; Go support for company
  (use-package company-go
    :config
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-go)))
  ;; Go guru, commands have prefix C-c C-o
  (if (executable-find "guru")
      (use-package go-guru
        :config
        (with-eval-after-load 'go-mode
          (add-hook 'go-mode-hook 'go-guru-hl-identifier-mode)))))

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode) ;; Github-flavored markdown
         ("\\.md\\'" . markdown-mode) ;; regular markdown
         ("\\.markdown\\'" . markdown-mode)))

;; Org-mode
(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link))
  :config
  (setq org-agenda-start-on-weekday nil)
  (setq org-catch-invisible-edits 'error)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANCELED(c@/!)")))
  (setq org-use-fast-todo-selection t)
  (setq org-use-speed-commands t))

;; Python
(when (executable-find "python")
  (use-package elpy
    :init (elpy-enable)
    :config
    (setq elpy-rpc-backend "jedi") ;; use jedi for Python autocompletion
    (with-eval-after-load 'flycheck ;; use FlyCheck for Python syntax checking
      (remove-hook 'elpy-modules 'elpy-module-flymake)
      (add-hook 'elpy-mode-hook 'flycheck-mode)))
  (if (executable-find "jupyter")
      ;; IPython notebook client
      (use-package ein
        :config
        ;; fancy prompts in IPython 5 or later don't work with Eshell
        (setq ein:console-args '("--simple-prompt"))
        (with-eval-after-load 'elpy
          (add-to-list 'python-shell-completion-native-disabled-interpreters
                       "jupyter")))))

;; YAML
(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; load local init file ~/.emacs.d/init-local.el
(let ((init-local-f (expand-file-name "init-local.el" user-emacs-directory)))
  (if (file-exists-p init-local-f) (load-file init-local-f)))

(provide 'init)
;;; init.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
