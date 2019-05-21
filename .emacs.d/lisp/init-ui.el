;;; init-ui.el --- Emacs config UI layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Configure user interface

;;; Code:

;; Helper functions

(require 'cl-seq)

(with-eval-after-load 'ibuffer
  (defvar ibuffer-saved-filter-groups
          '(("default"
             ("Org" (or (mode . org-mode) (mode . org-agenda-mode)))
             ("Dired" (mode . dired-mode))
             ("Elisp" (mode . emacs-lisp-mode))
             ("Python" (mode . python-mode))
             ("Magit" (name . "\*magit"))
             ("Emacs" (name . "^\\*"))))))

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

;; Configure user interface

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
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (and (not (display-graphic-p)) (fboundp 'menu-bar-mode)) (menu-bar-mode -1))

;; smooth scrolling in GUI (hold shift for 5 lines or control for full screen)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control)))))

;; left-right scrolling
(if (eq system-type 'darwin)
    (progn (global-set-key [wheel-right] (lambda () (interactive) (scroll-left 1)))
           (global-set-key [wheel-left] (lambda () (interactive) (scroll-right 1))))
  (progn (global-set-key [wheel-right] 'scroll-right)
         (global-set-key [wheel-left] 'scroll-left)))

;; use left Option key as Meta, preserve right Option key on Mac OS X
;; use right Command key as Hyper
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
        mac-right-option-modifier nil
        mac-right-command-modifier 'hyper))

;; framework for temporary or repeatable bindings
(require 'init-ui-hydra)

;; alternative interface for M-x
(use-package amx
  :bind ("M-X" . amx-major-mode-commands)
  :init (amx-mode))

;; text completion framework
(use-package company
  :defer 1
  :delight company-mode
  :config
  (setq company-dabbrev-downcase nil
        company-idle-delay 0.25
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t  ;; use M-<number> to directly choose completion
        company-tooltip-align-annotations t)
  (company-tng-configure-default) ;; Tab and Go behavior
  (add-hook 'prog-mode-hook 'company-mode))

;; Ediff
(use-package ediff
  :ensure nil ;; built-in
  :config
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
  (global-set-key (kbd "H-E") 'my-hydra/ediff/body))

;; Eldoc
(use-package eldoc
  :ensure nil ;; built-in
  :delight eldoc-mode
  :init (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

;; increase selected region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; manage window configs, bindings prefixed by C-c C-w (default)
(use-package eyebrowse
  :defer 2
  :delight eyebrowse-mode
  :init
  (setq eyebrowse-keymap-prefix (kbd "H-W") ;; change prefix binding
        eyebrowse-new-workspace t)
  (eyebrowse-mode t))

;; code folding package
(use-package hideshow
  :ensure nil ;; built-in
  :delight hs-minor-mode
  :config (add-hook 'prog-mode-hook 'hs-minor-mode))

;; highlight line
(use-package hl-line
  :ensure nil ;; built-in
  :commands hl-line-mode
  :defer t)

;; advanced buffer menu
(use-package ibuffer
  :ensure nil ;; built-in
  :commands ibuffer
  :hook (ibuffer-mode . (lambda () (progn
                                      (ibuffer-auto-mode 1)
                                      (when ibuffer-saved-filter-groups
                                        (ibuffer-switch-to-saved-filter-groups (car (car ibuffer-saved-filter-groups)))))))
  :bind ("C-x C-b" . ibuffer)
  :config
  (use-package ibuffer-vc ;; group buffers by VC project in ibuffer
    :after ibuffer))

;; interactively do things with buffers and files, use C-f to escape
(use-package ido
  :ensure nil ;; built-in
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
  ;; replace stock completion with ido wherever possible
  (use-package ido-completing-read+
    :init (ido-ubiquitous-mode t))
  ;; use ido for commands using `completing-read-multiple'
  (use-package crm-custom
    :init (crm-custom-mode 1)))

;; Vim Tagbar-like imenu extension
;; (use-package imenu-list
;;   :bind ("H-i" . imenu-list-smart-toggle)
;;   :config (setq imenu-list-focus-after-activation t
;;                 imenu-list-auto-resize t))

;; multiple cursors
(use-package multiple-cursors
  :defer 2
  :config
  (setq mc/always-run-for-all nil
        mc/always-repeat-command nil
        mc/insert-numbers-default 1)
  (defhydra my-hydra/multiple-cursors (:color amaranth :columns 3)
    "Multiple-cursors"
    ("l" mc/edit-lines "edit-lines" :exit t)
    ("a" mc/mark-all-like-this "mark-all-like" :exit t)
    ("s" mc/mark-all-in-region-regexp "mark-regex-rgn" :exit t)
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
  (global-set-key (kbd "H-M") 'my-hydra/multiple-cursors/body))

;; recently opened files
(use-package recentf
  :ensure nil ;; built-in
  :commands recentf-open-files
  :bind ("H-r" . recentf-open-files)
  :config
  (setq recentf-max-menu-items 10
        recentf-max-saved-items 50)
  (recentf-mode t))

;; traverse undo history as a tree, default binding is C-x u
(use-package undo-tree
  :defer 1
  :delight undo-tree-mode
  :config
  (setq undo-tree-visualizer-relative-timestamps nil)
  (global-undo-tree-mode))

;; display available bindings in popup
(use-package which-key
  :delight which-key-mode
  :bind ("C-H-w" . which-key-show-top-level)
  :init (which-key-mode 1)
  :config (setq which-key-compute-remaps t
                which-key-allow-multiple-replacements t))

;; visualize and cleanup whitespace
(use-package whitespace
  :ensure nil ;; built-in
  :defer 1
  :config
  (defhydra my-hydra/whitespace (:color teal :columns 3)
    "Whitespace"
    ("w" whitespace-mode "show-whitespace" :exit nil)
    ("n" whitespace-newline-mode "show-newline" :exit nil)
    ("c" whitespace-cleanup "cleanup")
    ("r" whitespace-report "report")
    ("q" nil "quit"))
  (global-set-key (kbd "H-M-w") 'my-hydra/whitespace/body))

;; expandable snippet template system
(use-package yasnippet
  :delight yas-minor-mode
  :defer 2
  :bind (:map yas-minor-mode-map
              ("<tab>" . nil) ;; disable default tab binding to ...
              ("TAB" . nil) ;; ... avoid conflict with company-mode tng
              ("C-S-SPC" . #'yas-expand))
  :config
  ;; official snippets
  (use-package yasnippet-snippets)
  ;; allow creation of temporary snippets
  (use-package auto-yasnippet)
  (defhydra my-hydra/yasnippet (:color teal :columns 3)
    "YASnippet"
    ("SPC" yas-expand "expand") ;; expand snippet
    ("d" yas-describe-tables "describe") ;; snippets for current mode
    ("w" aya-create "create-auto") ;; store temp yasnippet
    ("y" aya-expand "expand-auto") ;; paste temp yasnippet
    ("?" (message "Current auto-yasnippet:\n%s" aya-current) "current-auto") ;; show temp yasnippet
    ("q" nil "quit"))
  (global-set-key (kbd "H-Y") 'my-hydra/yasnippet/body)
  (yas-global-mode 1))

(require 'init-ui-color)

(require 'init-ui-font)

(require 'init-ui-modeline)

(provide 'init-ui)

;;; init-ui.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
