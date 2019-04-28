;;; init.el --- Emacs config file -*- lexical-binding: t; -*-

;; Author: matheuristic
;; URL: https://github.com/matheuristic/dotfiles

;;; Commentary:

;; Symlink or copy this file to ~/.emacs or ~/.emacs.d/init.el

;;; Code:

;; basic interface settings
(setq inhibit-startup-message t ;; suppress splash screen
      ring-bell-function 'ignore ;; turn off audible and visual bells
      scroll-conservatively 101 ;; scroll a line at a time at window edge
      column-number-mode t ;; show column number in modeline
      show-paren-delay 0) ;; no delay in show-paren-mode
(show-paren-mode t) ;; show matching parentheses

;; indent with soft tabs. Use C-q <TAB> for real tabs
(setq-default indent-tabs-mode nil)

;; remove unused interface elements
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (and (not (display-graphic-p)) (fboundp 'menu-bar-mode)) (menu-bar-mode -1))

;; smooth scrolling in GUI (hold shift for 5 lines or control for full screen)
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

;; use left Option key as Meta, preserve right Option key on Mac OS X
;; use right Command key as Hyper
(if (eq system-type 'darwin)
    (setq mac-option-modifier 'meta
          mac-right-option-modifier nil
          mac-right-command-modifier 'hyper))

(require 'cl-seq)
(defun my-yank-from-kill-ring ()
  "Yank from the kill ring into buffer at point or region.
Uses `completing-read' for selection, which is set by Ido, Ivy, etc."
  (interactive)
  (let ((to_insert (completing-read
                    "Yank : " (cl-delete-duplicates kill-ring :test #'equal))))
    ;; delete selected buffer region (if applicable)
    (if (and to_insert (region-active-p))
      (delete-region (region-beginning) (region-end)))
    ;; insert the selected entry from the kill ring
    (insert to_insert)))

(defun my-transpose-windows (selector)
  "Transpose buffers between current window and window after calling SELECTOR."
  (let ((from-win (selected-window))
        (from-buf (window-buffer)))
    (funcall selector)
    (set-window-buffer from-win (window-buffer))
    (set-window-buffer (selected-window) from-buf)))

(defun my-enlarge-frame (w h)
  "Enlarge width, height of selected frame by W, H lines (shrink if negative)."
  (let ((this-frame (selected-frame)))
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
(dolist (project (directory-files lisp-dir t "\\w+"))
  (if (file-directory-p project) (add-to-list 'load-path project)))

;; third-party packages in ~/.emacs.d/site-lisp and its subdirectories
(defvar site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(unless (file-exists-p site-lisp-dir) (make-directory site-lisp-dir))
(add-to-list 'load-path site-lisp-dir)
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (if (file-directory-p project) (add-to-list 'load-path project)))

;; use package.el with given ELPA-compatible package repositories
(require 'package)
(setq package-enable-at-startup nil
      package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"     . 10)
        ("MELPA Stable" . 5)
        ("MELPA"        . 0)))

(package-initialize)

;; bootstrap use-package, provides configuration macros - MELPA Stable
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; preload use-package and bind-key to reduce load time
(eval-when-compile
  (require 'use-package)
  (require 'bind-key)
  (setq use-package-always-ensure t))

;; copies env vars from shell - MELPA Stable
(if (eq system-type 'darwin)  ;; only for Mac OS X GUI mode
    (use-package exec-path-from-shell
      :init (if (memq window-system '(mac ns))
                (exec-path-from-shell-initialize))))

;; customize how mode names appear in the mode line - ELPA
(use-package delight)

;; framework for temporary or repeatable bindings - MELPA Stable
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
    ("b" switch-to-buffer "switch" :exit t)
    ("q" nil "quit" :exit t))
  (defhydra my-hydra/desktop (:color teal)
    "Desktop"
    ("c" desktop-clear "clear")
    ("s" desktop-save "save")
    ("r" desktop-read "read")
    ("R" desktop-revert "revert")
    ("d" desktop-change-dir "dir")
    ("q" nil "quit"))
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
    ("q" nil "quit" :exit t))
  (defhydra my-hydra/narrow (:color teal)
    "Narrow"
    ("n" narrow-to-region "region")
    ("p" narrow-to-page "page")
    ("d" narrow-to-defun "defun")
    ("w" widen "widen")
    ("q" nil "quit" :exit t))
  (defhydra my-hydra/navigation (:color amaranth :columns 4)
    "Navigation"
    ("h" backward-char "left")
    ("j" next-line "down")
    ("k" previous-line "up")
    ("l" forward-char "right")
    ("b" backward-word "wd-left")
    ("w" forward-word "wd-right")
    ("0" move-beginning-of-line "ln-begin")
    ("$" move-end-of-line "ln-end")
    ("(" backward-sentence "sent-left")
    (")" forward-sentence "sent-right")
    ("{" backward-paragraph "par-left")
    ("}" forward-paragraph "par-right")
    ("," backward-sexp "sexp-left")
    ("." forward-sexp "sexp-right")
    ("[" backward-list "list-left")
    ("]" forward-list "list-right")
    ("S-SPC" scroll-down "pg-up")
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
    ("G" end-of-buffer "end-buf")
    ("gG" goto-line "goto-line")
    ("q" nil "quit" :exit t))
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
    ("q" nil "quit" :exit t))
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
    ("q" nil "quit" :exit t))
  (defhydra my-hydra/zoom (:color amaranth)
    "Zoom"
    ("-" text-scale-decrease "out")
    ("+" text-scale-increase "in")
    ("0" (text-scale-adjust 0) "reset")
    ("q" nil "quit" :exit t))
  (global-set-key (kbd "H-D") 'my-hydra/desktop/body)
  (global-set-key (kbd "H-N") 'my-hydra/narrow/body)
  (global-set-key (kbd "H-s") 'my-hydra/search/body)
  (global-set-key (kbd "H-Z") 'my-hydra/zoom/body)
  (global-set-key (kbd "H-b") 'my-hydra/buffer/body)
  (global-set-key (kbd "H-f") 'my-hydra/frame/body)
  (global-set-key (kbd "H-n") 'my-hydra/navigation/body)
  (global-set-key (kbd "H-w") 'my-hydra/window/body))

;; alternative interface for M-x - MELPA Stable
(use-package amx
  :bind ("M-X" . amx-major-mode-commands)
  :init (amx-mode))

;; text completion framework - MELPA Stable
(use-package company
  :delight company-mode
  :config
  (setq company-dabbrev-downcase nil
        company-idle-delay 0.25
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t  ;; use M-<number> to directly choose completion
        company-tooltip-align-annotations t)
  (company-tng-configure-default) ;; Tab and Go behavior
  (add-hook 'after-init-hook 'global-company-mode))

;; Dired - built-in
(use-package dired
  :ensure nil ;; dired does not have updated packages in ELPA/MELPA
  :config
  (setq dired-dwim-target t ;; use neighboring dired buffer as default target dir
                dired-listing-switches "-alhvF" ;; more readable file listings
                dired-recursive-copies 'always ;; always copy recursively
                dired-recursive-deletes 'always) ;; always delete recursively
  (add-hook 'dired-mode-hook 'auto-revert-mode)) ;; auto-refresh on file change

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
      ("q" nil "quit" :exit t))
    (global-set-key (kbd "H-d") 'my-hydra/ediff/body)))

;; Eldoc - built-in
(use-package eldoc
  :delight eldoc-mode
  :init (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

;; Eshell - built-in
(use-package eshell
  :commands (eshell eshell-command)
  :init
  (require 'em-term)
  (require 'em-smart)
  :config
  (setq eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t
        eshell-where-to-jump 'begin)
  (add-to-list 'eshell-visual-commands '("htop" "lftp" "ssh" "vim"))
  (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"
                                            "vagrant" "ssh")))

;; typing any left bracket auto-inserts matching right bracket - built-in
;; (use-package elec-pair
;;   :config (dolist (mode-hook '(prog-mode-hook org-mode-hook markdown-mode-hook))
;;             (add-hook mode-hook 'electric-pair-mode)))

;; increase selected region by semantic units - MELPA Stable
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; manage window configs, bindings prefixed by C-c C-w  - MELPA Stable
(use-package eyebrowse
  :delight eyebrowse-mode
  :init (eyebrowse-mode t)
  :config (setq eyebrowse-new-workspace t))

;; syntax checker, alternative to Flymake - MELPA Stable
;; (use-package flycheck
;;   :delight flycheck-mode
;;   :init (global-flycheck-mode)
;;   :config
;;   (with-eval-after-load 'hydra
;;     (defhydra my-hydra/flycheck (:color amaranth :columns 6)
;;       "Error"
;;       ("F" flycheck-error-list-set-filter "filter")
;;       ("p" flycheck-previous-error "previous")
;;       ("n" flycheck-next-error "next")
;;       ("f" flycheck-first-error "first")
;;       ("l" (condition-case nil (while t (flycheck-next-error))
;;              (user-error nil)) "last")
;;       ("L" (condition-case nil (quit-windows-on "*Flycheck errors*" t)
;;              (error (flycheck-list-errors))) "list")
;;       ("q" nil "quit" :exit t))
;;     ;; bind over my-hydra/error
;;     (define-key flycheck-mode-map (kbd "H-e") 'my-hydra/flycheck/body)))

;; syntax checker, use C-h . to show error on current line - built-in
(use-package flymake
  :config
  (use-package flymake-diagnostic-at-point
    :config
    (setq flymake-diagnostic-at-point-error-prefix "» ")
    (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))
  (add-hook 'emacs-lisp-mode-hook '(lambda () (flymake-mode)))
  (defun my-toggle-flymake-diagnostics ()
    "Toggles flymake diagnostics window for current buffer."
    (interactive)
    (if flymake-mode
        (let* ((buf-name (buffer-name (current-buffer)))
               (flymake-winds (condition-case nil
                                  (get-buffer-window-list
                                   (concat "*Flymake diagnostics for "
                                           buf-name
                                           "*"))
                                (error nil))))
          (if flymake-winds
              (dolist (wind flymake-winds) (quit-window nil wind))
            (flymake-show-diagnostics-buffer)))))
  (with-eval-after-load 'hydra
    (defhydra my-hydra/flymake (:color amaranth)
      "Error"
      ("p" flymake-goto-prev-error "previous")
      ("n" flymake-goto-next-error "next")
      ("L" my-toggle-flymake-diagnostics "list")
      ("q" nil "quit" :exit t))
    (define-key flymake-mode-map (kbd "H-e") 'my-hydra/flymake/body)))

;; code folding package -- built-in
(use-package hideshow
  :delight hs-minor-mode
  :config (add-hook 'prog-mode-hook 'hs-minor-mode))

;; advanced buffer menu - built-in
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (use-package ibuffer-vc ;; group buffers by VC project in ibuffer
    :after ibuffer))
    ;; :config (add-hook 'ibuffer-hook
    ;;                   (lambda ()
    ;;                     (ibuffer-vc-set-filter-groups-by-vc-root)
    ;;                     (unless (eq ibuffer-sorting-mode 'alphabetic)
    ;;                       (ibuffer-do-sort-by-alphabetic))))))

;; interactively do things with buffers and files, use C-f to escape - built-in
(use-package ido
  :init (ido-mode t)
  :bind ("H-y" . my-yank-from-kill-ring)
  :config
  (setq ido-create-new-buffer 'always
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        ido-enable-flex-matching t
        ido-enable-tramp-completion nil
        ido-everywhere t
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t)
  ;; do not make suggestions when naming new file
  (when (boundp 'ido-minor-mode-map-entry)
    (define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil))
  ;; replace stock completion with ido wherever possible - MELPA Stable
  (use-package ido-completing-read+
    :init (ido-ubiquitous-mode t))
  ;; use ido for commands using `completing-read-multiple' - MELPA Stable
  (use-package crm-custom
    :init (crm-custom-mode 1)))

;; Vim Tagbar-like imenu extension - MELPA Stable
;; (use-package imenu-list
;;   :bind ("H-i" . imenu-list-smart-toggle)
;;   :config (setq imenu-list-focus-after-activation t
;;                 imenu-list-auto-resize t))

;; Git - MELPA Stable (all packages)
(when (executable-find "git")
  (use-package magit
    :bind ("H-g g" . magit-status)
    :config
    (setq auto-revert-check-vc-info t)
    (with-eval-after-load 'ido-completing-read+
       (setq magit-completing-read-function 'magit-ido-completing-read)))
  (use-package git-timemachine
    :after magit
    :bind ("H-g t" . git-timemachine)))

;; multiple cursors - MELPA Stable
(use-package multiple-cursors
  :config
  (setq mc/always-run-for-all nil
        mc/always-repeat-command nil
        mc/insert-numbers-default 1)
  (with-eval-after-load 'hydra
    (defhydra my-hydra/multiple-cursors (:color amaranth :columns 3)
      "Multiple-cursors"
      ("l" mc/edit-lines "edit-lines")
      ("a" mc/mark-all-like-this "mark-all-like")
      ("<mouse-1>" mc/add-cursor-on-click "mark-click")
      ("p" mc/mark-previous-like-this "mark-prev")
      ("P" mc/skip-to-previous-like-this "skip-prev")
      ("M-p" mc/unmark-previous-like-this "unmark-prev")
      ("n" mc/mark-next-like-this "mark-next")
      ("N" mc/skip-to-next-like-this "skip-next")
      ("M-n" mc/unmark-next-like-this "unmark-next")
      ("0" mc/insert-numbers "insert-numbers" :exit t)
      ("A" mc/insert-letters "insert-letters" :exit t)
      ("q" nil "quit" :exit t))
    (global-set-key (kbd "H-M") 'my-hydra/multiple-cursors/body)))

;; Org-mode - built-in
(use-package org
  :bind (("H-a" . org-agenda)
         ("H-l" . org-store-link))
  :config
  (require 'org-agenda)
  (setq org-agenda-start-on-weekday nil
        org-catch-invisible-edits 'error
        org-hide-emphasis-markers t
        org-log-into-drawer t
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
                            (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANCELED(c@/!)"))
        org-use-fast-todo-selection t
        org-use-speed-commands t
        org-startup-indented t)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'variable-pitch-mode)
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
    ("TAB" org-agenda-goto :exit t)
    ("RET" org-agenda-switch-to :color blue)
    ("o" link-hint-open-link :exit t)
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
    ("ci" org-agenda-clock-in :exit t)
    ("co" org-agenda-clock-out)
    ("cq" org-agenda-clock-cancel)
    ("cg" org-agenda-clock-goto :exit t)
    ("gr" org-agenda-redo)
    ("gd" org-agenda-goto-date)
    ("." org-agenda-goto-today)
    ("q" nil "quit" :exit t))
  (define-key org-agenda-mode-map (kbd "H-m") 'my-hydra/org-agenda/body))

;; project interaction library - MELPA Stable
(use-package projectile
  :delight projectile-mode '(:eval (concat " [" (projectile-project-name) "]"))
  :init (projectile-mode)
  :config
  (setq projectile-switch-project-action 'projectile-commander)
  ;; use ripgrep for grepping in projectile, if available
  (if (executable-find "rg")
      (progn
        (use-package projectile-ripgrep)
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
      ("q" nil "quit" :exit t))
    (define-key projectile-mode-map (kbd "H-p") 'my-hydra/projectile/body)))

;; recently opened files - built-in
(use-package recentf
  :bind ("H-R" . recentf-open-files)
  :init (recentf-mode t)
  :config (setq recentf-max-menu-items 10
                recentf-max-saved-items 50))

;; traverse undo history as a tree, default binding is C-x u - GNU ELPA
(use-package undo-tree
  :delight undo-tree-mode
  :init (global-undo-tree-mode)
  :config (setq undo-tree-visualizer-relative-timestamps nil))

;; visit large files without loading it entirely - MELPA Stable
(use-package vlf
  :config (require 'vlf-setup))

;; display available bindings in popup - GNU ELPA
(use-package which-key
  :delight which-key-mode
  :bind ("H-W" . which-key-show-top-level)
  :init (which-key-mode 1)
  :config (setq which-key-compute-remaps t
                which-key-allow-multiple-replacements t))

;; template systems, i.e. expandable snippets - GNU ELPA
(use-package yasnippet
  :delight yas-minor-mode
  :bind (:map yas-minor-mode-map
              ("<tab>" . nil) ;; disable default tab binding to ...
              ("TAB" . nil) ;; ... avoid conflict with company-mode tng
              ("C-S-SPC" . #'yas-expand))
  :init (yas-global-mode 1)
  :config
  ;; official snippets - MELPA Stable
  (use-package yasnippet-snippets)
  ;; allow creation of temporary snippets - MELPA Stable
  (use-package auto-yasnippet)
  (with-eval-after-load 'hydra
    (defhydra my-hydra/yasnippet (:color teal :columns 3)
      "YASnippet"
      ("SPC" yas-expand "expand") ;; expand snippet
      ("d" yas-describe-tables "describe") ;; describe snippets for current mode
      ("w" aya-create "create-auto") ;; store temp yasnippet
      ("y" aya-expand "expand-auto") ;; paste temp yasnippet
      ("?" (message "Current auto-yasnippet:\n%s" aya-current) "current-auto") ;; show temp yasnippet
      ("q" nil "quit"))
    (global-set-key (kbd "H-Y") 'my-hydra/yasnippet/body)))

;; load local post-init file ~/.emacs.d/init-local.el
(let ((local-f (expand-file-name "init-local.el" user-emacs-directory)))
  (if (file-exists-p local-f) (load-file local-f)))

(provide 'init)
;;; init.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
