;;; init-ui.el --- Emacs config UI layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up user interface

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
    ;; delete selected buffer region, if applicable
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

(when (eq system-type 'darwin) ;; on Mac OS X,
  (setq mac-option-modifier 'meta ;; use left Option as Meta,
        mac-right-option-modifier nil ;; preserve right Option,
        mac-right-command-modifier 'hyper)) ;; and use right Command as Hyper

;; framework for temporary or repeatable bindings
(require 'init-ui-hydra)

;; alternative interface for M-x
(use-package amx
  :bind ("M-X" . amx-major-mode-commands)
  :init (amx-mode))

;; text completion framework
(use-package company
  :defer t
  :delight company-mode
  :init (with-eval-after-load 'prog-mode
          (add-hook 'prog-mode-hook 'company-mode))
  :config
  (setq company-dabbrev-downcase nil
        company-idle-delay 0.25
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t  ;; use M-<num> to directly choose completion
        company-tooltip-align-annotations t)
  (company-tng-configure-default)) ;; Tab and Go behavior

;; Ediff
(use-package ediff
  :ensure nil ;; built-in
  :commands ediff-setup-keymap
  :init
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
  :hook (emacs-lisp-mode . eldoc-mode))

;; increase selected region by semantic units
(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

;; manage window configs, bindings prefixed by C-c C-w (default)
(use-package eyebrowse
  :defer 1
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
  :commands hl-line-mode)

;; advanced buffer menu
(use-package ibuffer
  :ensure nil ;; built-in
  :commands ibuffer
  :hook (ibuffer-mode . (lambda () (progn ;; default to first saved group
                                     (ibuffer-auto-mode 1)
                                     (when ibuffer-saved-filter-groups
                                       (ibuffer-switch-to-saved-filter-groups
                                         (car (car ibuffer-saved-filter-groups)))))))
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-expert t ;; skip extraneous confirm messages
        ibuffer-show-empty-filter-groups nil)
  ;; build VC project ibuffer filter groups
  (use-package ibuffer-vc
    :after ibuffer
    :config (define-key ibuffer-mode-map (kbd "/ V") 'ibuffer-vc-set-filter-groups-by-vc-root))
  ;; adapted hydra definitions from https://github.com/abo-abo/hydra/wiki/Ibuffer
  (defhydra my-hydra/ibuffer (:color amaranth :columns 3)
    "ibuffer"
    ;; navigation
    ("n" ibuffer-forward-line "next")
    ("p" ibuffer-backward-line "prev")
    ("RET" (condition-case nil
               (progn (ibuffer-toggle-filter-group) (my-hydra/ibuffer/body))
             (error (ibuffer-visit-buffer))) "open" :exit t)
    ;; mark
    ("m" ibuffer-mark-forward "mark")
    ("u" ibuffer-unmark-forward "unmark")
    ("*" my-hydra/ibuffer-mark/body "→ mark" :exit t)
    ;; actions
    ("S" ibuffer-do-save "save")
    ("D" ibuffer-do-delete "delete")
    ("a" my-hydra/ibuffer-action/body "→ action" :exit t)
    ;; view
    ("g" ibuffer-update "refresh")
    ("s" my-hydra/ibuffer-sort/body "→ sort" :exit t)
    ("/" my-hydra/ibuffer-filter/body "→ filter" :exit t)
    ;; other
    ("o" ibuffer-visit-buffer-other-window "open-other" :exit t)
    ("q" nil "quit" :exit t))
  (defhydra my-hydra/ibuffer-mark (:color teal
                                   :columns 5
                                   :after-exit (my-hydra/ibuffer/body))
    "ibuffer → mark"
    ("*" ibuffer-unmark-all "unmark all")
    ("M" ibuffer-mark-by-mode "mode")
    ("m" ibuffer-mark-modified-buffers "modified")
    ("u" ibuffer-mark-unsaved-buffers "unsaved")
    ("s" ibuffer-mark-special-buffers "special")
    ("r" ibuffer-mark-read-only-buffers "read-only")
    ("/" ibuffer-mark-dired-buffers "dired")
    ("e" ibuffer-mark-dissociated-buffers "dissociated")
    ("h" ibuffer-mark-help-buffers "help")
    ("z" ibuffer-mark-compressed-file-buffers "compressed")
    ("q" nil "←"))
  (defhydra my-hydra/ibuffer-action (:color teal
                                     :columns 3
                                     :after-exit (if (eq major-mode 'ibuffer-mode)
                                                   (my-hydra/ibuffer/body)))
    "ibuffer → action"
    ("A" ibuffer-do-view "view")
    ("E" ibuffer-do-eval "eval")
    ("F" ibuffer-do-shell-command-file "shell-command-file")
    ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
    ("H" ibuffer-do-view-other-frame "view-other-frame")
    ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
    ("M" ibuffer-do-toggle-modified "toggle-modified")
    ("O" ibuffer-do-occur "occur")
    ("P" ibuffer-do-print "print")
    ("Q" ibuffer-do-query-replace "query-replace")
    ("R" ibuffer-do-rename-uniquely "rename-uniquely")
    ("T" ibuffer-do-toggle-read-only "toggle-read-only")
    ("U" ibuffer-do-replace-regexp "replace-regexp")
    ("V" ibuffer-do-revert "revert")
    ("W" ibuffer-do-view-and-eval "view-and-eval")
    ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
    ("q" nil "←"))
  (defhydra my-hydra/ibuffer-sort (:color amaranth :columns 5)
    "ibuffer → sort"
    ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
    ("f" ibuffer-do-sort-by-filename/process "filename")
    ("m" ibuffer-do-sort-by-major-mode "mode")
    ("s" ibuffer-do-sort-by-size "size")
    ("v" ibuffer-do-sort-by-recency "recency")
    ("i" ibuffer-invert-sorting "invert")
    ("q" my-hydra/ibuffer/body "←" :exit t))
  (defhydra my-hydra/ibuffer-filter (:color amaranth :columns 5)
    "ibuffer → filter"
    ("c" ibuffer-filter-by-content "content")
    ("e" ibuffer-filter-by-predicate "predicate")
    ("f" ibuffer-filter-by-filename "filename")
    ("m" ibuffer-filter-by-used-mode "mode")
    ("M" ibuffer-filter-by-derived-mode "derived mode")
    ("n" ibuffer-filter-by-name "name")
    (">" ibuffer-filter-by-size-gt "size-gt")
    ("<" ibuffer-filter-by-size-lt "size-lt")
    ("&" ibuffer-and-filter "and")
    ("|" ibuffer-or-filter "or")
    ("V" ibuffer-vc-set-filter-groups-by-vc-root "vc-groups")
    ("R" ibuffer-switch-to-saved-filter-groups "saved-groups")
    ("/" ibuffer-filter-disable "disable")
    ("q" my-hydra/ibuffer/body "←" :exit t))
  (define-key ibuffer-mode-map (kbd "H-m") 'my-hydra/ibuffer/body))

;; interactively do things with buffers and files, use C-f to escape
(use-package ido
  :ensure nil ;; built-in
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
  (ido-mode t) ;; enable ido-mode globally
  ;; don't make suggestions when naming new file
  (when (boundp 'ido-minor-mode-map-entry)
    (define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil))
  ;; replace stock completion with ido wherever possible
  (use-package ido-completing-read+
    :after ido
    :config (ido-ubiquitous-mode t))
  ;; use ido for commands using `completing-read-multiple'
  (use-package crm-custom
    :after ido
    :config (crm-custom-mode 1)))

;; multiple cursors
(use-package multiple-cursors
  :defer 1
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
  (with-eval-after-load 'multiple-cursors
    (global-set-key (kbd "H-M") 'my-hydra/multiple-cursors/body)))

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
  :init
  (setq which-key-compute-remaps t
        which-key-allow-multiple-replacements t)
  (which-key-mode 1))

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
  :defer 1
  :config
  ;; official snippets
  (use-package yasnippet-snippets)
  ;; allow creation of temporary snippets
  (use-package auto-yasnippet)
  (defhydra my-hydra/yasnippet (:color teal :columns 4)
    "YASnippet"
    ("SPC" yas-expand "expand") ;; expand snippet
    ("d" yas-describe-tables "describe") ;; snippets for current mode
    ("s" yas-insert-snippet "insert") ;; insert snippet
    ("n" yas-new-snippet "new") ;; create new snippet
    ("v" yas-visit-snippet-file "visit-snippet") ;; visit snippet file
    ("w" aya-create "create-auto") ;; store temp yasnippet
    ("y" aya-expand "expand-auto") ;; paste temp yasnippet
    ("?" (message "Current auto-yasnippet:\n%s" aya-current) "current-auto") ;; show temp yasnippet
    ("q" nil "quit"))
  (define-key yas-minor-mode-map (kbd "H-Y") 'my-hydra/yasnippet/body)
  ;; remove default yas-minor-map bindings, add new one for snippet expansion
  (require 'bind-key)
  (unbind-key "\C-c&" yas-minor-mode-map) ;; removing prefix bindings also ...
  (unbind-key "\C-c" yas-minor-mode-map) ;; ... removes the bindings using them
  (unbind-key "<tab>" yas-minor-mode-map) ;; remove tab bindings to avoid ...
  (unbind-key "TAB" yas-minor-mode-map) ;; ... conflict with company-mode tng
  (bind-key "C-S-SPC" #'yas-expand yas-minor-mode-map)
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
