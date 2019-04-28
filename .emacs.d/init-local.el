;;; init-local.el --- Sample Emacs local post-initialization config file -*- lexical-binding: t; -*-

;; Author: matheuristic
;; URL: https://github.com/matheuristic/dotfiles

;;; Commentary:

;; This file contains optional packages that are language-specific or UI
;; elements for specific machines (e.g. Macs with no physical ESC key)
;; Symlink or copy this file to ~/.emacs.d/init-local.el

;; TODO test org-mind-map package
;; TODO test org-brain package

;;; Code:

;; USER INTERFACE

;; color scheme - MELPA Stable
(use-package eink-theme
  :config
  (load-theme 'eink t)
  ;; make fringe bitmaps visible (default is to make them invisible)
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default))
  ;; make comment delimiters match comment
  (set-face-attribute 'font-lock-comment-delimiter-face nil
                      :foreground (face-foreground 'font-lock-comment-face)
                      :background (face-background 'font-lock-comment-face)
                      :inherit 'font-lock-comment-face)
  ;; make strings distinct
  (set-face-attribute 'font-lock-string-face nil
                      :foreground "#606060"))
;; (use-package gruvbox-theme :config (load-theme 'gruvbox t))
;; (use-package poet-theme :config (load-theme 'poet t))

;; sidebar file explorer using a tree layout - MELPA Stable
;; (use-package treemacs
;;   :bind ("H-T" . treemacs)
;;   :defer 0.5
;;   :config
;;   ;; projectile integration for treemacs - MELPA Stable
;;   (use-package treemacs-projectile
;;     :after projectile))


;; NON-LANGUAGE-SPECIFIC

;; porcelain for conda, use this or virtualenvwrapper - MELPA Stable
;; (use-package conda
;;   :init (setq conda-anaconda-home "~/miniconda3") ;; conda root directory
;;   :config
;;   (conda-env-initialize-eshell) ;; eshell support
;;   ;; (conda-env-initialize-interactive-shells) ;; interactive shell support
;;   ;; (conda-env-autoactivate-mode t) ;; auto-activation using proj environment.yml
;;   (setq-default mode-line-format
;;                 (add-to-list 'mode-line-format
;;                              '(:eval (if conda-env-current-name
;;                                          (format " {%s}"
;;                                                  (truncate-string-to-width
;;                                                   conda-env-current-name
;;                                                   15 nil nil "…"))
;;                                        ""))
;;                              t)) ;; add current conda env to mode-line, if any
;;   (with-eval-after-load 'hydra
;;     (defhydra my-hydra/conda (:color teal :columns 4)
;;       "
;; conda %s(if conda-env-current-name (concat \"[\" conda-env-current-name \"]\") \"\")"
;;       ("a" conda-env-activate "activate")
;;       ("d" conda-env-deactivate "deactivate")
;;       ("l" conda-env-list "list")
;;       ("q" nil "quit"))
;;     (global-set-key (kbd "H-C") 'my-hydra/conda/body)))

;; front-end for interacting with debug servers, use this or realgud - MELPA Stable
;; (use-package dap-mode
;;   :pin "MELPA"
;;   :after lsp-mode
;;   :config
;;   (dap-mode 1)
;;   (dap-ui-mode 1))

;; Language Server Protocol - MELPA
(use-package lsp-mode
  :pin "MELPA"
  :config
  ;; change nil to 't to enable logging of packets between emacs and the LS
  ;; this is valuable for debugging communication with the MS Python Language
  ;; Server and comparing this with what vs.code is doing
  ;; (setq lsp-print-io nil)
  (setq lsp-eldoc-enable-hover nil ;; don't have eldoc display hover info
        lsp-eldoc-enable-signature-help nil ;; don't have eldoc display signature help
        lsp-prefer-flymake t) ;; set to nil to prefer flycheck to flymake
  ;; lsp-ui enables pop-up documentation boxes and sidebar info
  (use-package lsp-ui
    :pin "MELPA"
    :commands lsp-ui-mode
    :config
    (setq lsp-ui-doc-enable t
          lsp-ui-doc-header t
          lsp-ui-doc-include-signature t
          lsp-ui-doc-max-height 20
          lsp-ui-doc-max-width 50
          lsp-ui-imenu-enable nil
          lsp-ui-peek-always-show t
          lsp-ui-sideline-enable t
          lsp-ui-sideline-ignore-duplicate t
          lsp-ui-sideline-show-hover nil)
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
    (define-key lsp-ui-mode-map (kbd "C-c i") 'lsp-ui-imenu))
  ;; install LSP company backend for LSP-driven completion
  (use-package company-lsp
    :pin "MELPA"
    :commands company-lsp
    :config (setq company-lsp-cache-candidates t))
  (with-eval-after-load 'hydra
    (defhydra my-hydra/lsp (:color teal :hint nil)
      "
Language Server Protocol

Buffer  _f_   : format          _m_   : imenu           _x_   : execute action

Server  _M-r_ : restart         _S_   : shutdown        _M-s_ : describe session

Symbol  _d_   : declaration     _D_   : definition      _R_   : references
        _i_   : implementation  _t_   : type            _o_   : documentation
        _r_   : rename

"
      ("f" lsp-format-buffer)
      ("m" lsp-ui-imenu)
      ("x" lsp-execute-code-action)
      ("M-r" lsp-restart-workspace)
      ("S" lsp-shutdown-workspace)
      ("M-s" lsp-describe-session)
      ("d" lsp-find-declaration)
      ("D" lsp-ui-peek-find-definitions)
      ("R" lsp-ui-peek-find-references)
      ("i" lsp-ui-peek-find-implementation)
      ("t" lsp-find-type-definition)
      ("o" lsp-describe-thing-at-point)
      ("r" lsp-rename)
      ("q" nil "quit" :color blue))
    (define-key lsp-mode-map (kbd "H-L") 'my-hydra/lsp/body)))

;; front-end for interacting with external debuggers, use this or dap-mode - MELPA Stable
;; (use-package realgud)

;; virtualenv tool, use this or conda - MELPA
(use-package virtualenvwrapper
  ;; enable the MELPA repository and uncomment below if the version of
  ;; virtualenvwrapper.el in MELPA Stable is too old for emacs-traad
  :pin "MELPA"
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  ;; set virtualenv storage dir if it differs from default ~/.virtualenvs
  (setq venv-location "~/miniconda3/envs")  ;; miniconda3
  ;; display currently active virtualenv on the mode line
  (setq-default mode-line-format
                (add-to-list 'mode-line-format
                             '(:eval (if venv-current-name
                                         (format " «%s»"
                                                 (truncate-string-to-width
                                                  venv-current-name
                                                  15 nil nil "…"))
                                       ""))
                             t)) ;; add current conda env to mode-line, if any
  (with-eval-after-load 'hydra
    (defhydra my-hydra/virtualenv (:color teal :columns 4)
      "virtualenv"
      ("w" venv-workon "workon")
      ("d" venv-deactivate "deactivate")
      ("m" venv-mkvirtualenv-using "make")
      ("r" venv-rmvirtualenv "remove")
      ("l" venv-lsvirtualenv "list")
      ("g" venv-cdvirtualenv "cd")
      ("c" venv-cpvirtualenv "cp")
      ("q" nil "quit"))
    (global-set-key (kbd "H-v") 'my-hydra/virtualenv/body)))


;; LANGUAGE-SPECIFIC

;; CSV - GNU ELPA
(use-package csv-mode
  :commands csv-mode
  :config
  (defhydra my-hydra/csv-mode (:color teal :columns 4)
    "CSV mode"
    ("s" csv-sort-fields "sort")
    ("r" csv-sort-numeric-fields "numsort")
    ("k" csv-kill-fields "cut")
    ("y" csv-yank-fields "copy")
    ("a" csv-align-fields "align")
    ("u" csv-unalign-fields "unalign")
    ("t" csv-transpose "transpose")
    ("q" nil "quit" :color blue))
  (define-key csv-mode-map (kbd "H-m") 'my-hydra/csv-mode/body))

;; Dockerfile - MELPA Stable
(use-package dockerfile-mode
  :commands dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; Emacs Speaks Statistics (languages: R, S, SAS, Stata, Julia) - MELPA Stable
(use-package ess
  :mode (("\\.R$" . R-mode)
         ("\\.jl$" . julia-mode))
  :commands (R-mode
             julia-mode
             ess-eval-function
             ess-eval-line
             ess-eval-buffer
             ess-switch-to-ESS))

;; Go support - MELPA Stable (all packages)
(when (executable-find "go")
  (use-package go-mode
    :commands go-mode
    :init
    (add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
    (if (executable-find "goimports")
        (setq gofmt-command "goimports")))
  (use-package company-go
    :after go-mode
    :config (with-eval-after-load 'company
              (add-to-list 'company-backends 'company-go)))
  ;; Go guru, commands have prefix C-c C-o
  (if (executable-find "guru")
      (use-package go-guru
        :after go-mode
        :init (with-eval-after-load 'go-mode
                (add-hook 'go-mode-hook 'go-guru-hl-identifier-mode)))))

;; JSON - GNU ELPA
(use-package json-mode
  :commands json-mode)

;; Markdown - MELPA Stable
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (use-package markdown-toc)  ;; Markdown table of contents
  :config
  (defhydra my-hydra/markdown-mode (:color teal :hint nil)
    "
Markdown mode

Formatting  _b_ : bold      _i_ : italic    _c_ : code      _p_ : pre-formatted
            _B_ : blockquote

Headings    _h_ : automatic _1_.._4_ : h1..h4

Move        _H_ : promote   _L_ : demote    _J_ : move down _K_ : move up

Other       _l_ : link      _u_ : uri       _f_ : footnote  _w_ : wiki-link
            _T_ : table of contents

"
    ("b" markdown-insert-bold)
    ("i" markdown-insert-italic)
    ("c" markdown-insert-code)
    ("p" markdown-insert-pre)
    ("B" markdown-insert-blockquote)
    ("h" markdown-insert-header-dwim)
    ("1" markdown-insert-header-atx-1)
    ("2" markdown-insert-header-atx-2)
    ("3" markdown-insert-header-atx-3)
    ("4" markdown-insert-header-atx-4)
    ("H" markdown-promote :color red)
    ("L" markdown-demote :color red)
    ("J" markdown-move-down :color red)
    ("K" markdown-move-up :color red)
    ("l" markdown-insert-link)
    ("u" markdown-insert-uri)
    ("f" markdown-insert-footnote)
    ("w" markdown-insert-wiki-link)
    ("T" markdown-toc-generate-toc)
    ("q" nil "quit" :color blue))
  (define-key markdown-mode-map (kbd "H-m") 'my-hydra/markdown-mode/body)
  (define-key gfm-mode-map (kbd "H-m") 'my-hydra/markdown-mode/body))

;; Python - MELPA Stable (all packages)
(when (executable-find "python")
  ;; use IPython for shell when available
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt -i")) ;; fancy prompts don't work in Eshell
  ;; configure flymake for Python
  (if (not (featurep 'flycheck))
      (when (load "flymake" t)
        (defun flymake-pylint-init ()
          (let* ((temp-file (flymake-init-create-temp-buffer-copy
                              'flymake-create-temp-inplace))
                 (local-file (file-relative-name
                               temp-file
                               (file-name-directory buffer-file-name))))
            (list "epylint" (list local-file))))
        (add-to-list 'flymake-allowed-file-name-masks
                     '("\\.py\\'" flymake-pylint-init)))
      ;; Set as a minor mode for Python
      (add-hook 'python-mode-hook '(lambda () (flymake-mode))))
  ;; virtualenv tool
  (use-package virtualenvwrapper
    ;; enable the MELPA repository and uncomment below if the version of
    ;; virtualenvwrapper.el in MELPA Stable is too old for emacs-traad
    :pin "MELPA"
    :config
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell)
    ;; set virtualenv storage dir if it differs from default ~/.virtualenvs
    (setq venv-location "~/miniconda3/envs")  ;; miniconda3
    ;; display currently active virtualenv on the mode line
    (setq-default mode-line-format
                  (add-to-list 'mode-line-format
                               '(:eval (if venv-current-name
                                           (format " «%s»"
                                                   (truncate-string-to-width
                                                    venv-current-name
                                                    15 nil nil "…"))
                                         ""))
                               t)) ;; add current conda env to mode-line, if any
    (with-eval-after-load 'hydra
      (defhydra my-hydra/virtualenv (:color teal :columns 4)
        "virtualenv"
        ("w" venv-workon "workon")
        ("d" venv-deactivate "deactivate")
        ("m" venv-mkvirtualenv-using "make")
        ("r" venv-rmvirtualenv "remove")
        ("l" venv-lsvirtualenv "list")
        ("g" venv-cdvirtualenv "cd")
        ("c" venv-cpvirtualenv "cp")
        ("q" nil "quit"))
      (global-set-key (kbd "H-v") 'my-hydra/virtualenv/body)))
  ;; START
  ;; install lsp-mode support for MS Python Language Server
  ;; option 1 ;; deprecated
  ;; (use-package ms-python
  ;;   :pin "MELPA"
  ;;   :config (add-hook 'python-mode-hook #'lsp))
  ;; option 2 ;; current
  (use-package lsp-python-ms
    :load-path "lisp"
    :config
    (setq lsp-python-ms-executable "~/Packages/Microsoft/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer")
    (add-hook 'python-mode-hook #'lsp))
  ;; END
  )

;; YAML support - MELPA Stable
(use-package yaml-mode
  :commands yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))


;; GRAPHICAL USER INTERFACE

(when (display-graphic-p)
  ;; A. customize typography

  ;; helper functions
  (require 'cl-extra)
  (defun my-font-exists (font-name)
    "Returns FONT-NAME if that font exists on the system and `nil` otherwise"
    (if (x-list-fonts font-name) font-name))
  (defun my-set-font (face family &optional height weight width)
    "Sets font for FACE to FAMILY at the given HEIGHT, WEIGHT and WIDTH"
    (set-face-attribute face nil
                        :family family
                        :height (or height 110)
                        :weight (or weight 'normal)
                        :width (or width 'normal)))
  ;; set default font priority
  (let* ((my-font-priority-list '("IBM Plex Mono"
                                  "Iosevka Slab"
                                  "Consolas"
                                  "Menlo"
                                  "DejaVu Sans Mono"))
         (my-font (cl-some #'my-font-exists my-font-priority-list))
         (is-darwin (eq system-type 'darwin)))
    (when my-font
      (my-set-font 'default my-font (if is-darwin 150 110) nil nil)
      (my-set-font 'mode-line my-font (if is-darwin 120 90) nil nil)
      (my-set-font 'mode-line-inactive my-font (if is-darwin 120 90) nil nil)))
  ;; set variable pitch font
  (let* ((my-font-priority-list '("IBM Plex Sans"
                                  "Helvetica"
                                  "DejaVu Sans"))
         (my-font (cl-some #'my-font-exists my-font-priority-list))
         (is-darwin (eq system-type 'darwin)))
    (when my-font
      (my-set-font 'variable-pitch my-font (if is-darwin 150 110) nil nil)))
  ;; enable ligatures, only works on Emacs Mac Port by Mitsuharu
  (if (fboundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode))

  ;; B. customize mode-line

  ;; mode line customizations - MELPA Stable (all packages)
  ;; minor-mode menu for mode line (right-click or call minions-minor-mode-menu)
  (use-package minions
    :init (minions-mode 1)
    :config (setq minions-direct '(projectile-mode
                                   flymake-mode
                                   overwrite-mode)))
  ;; display elements of the mode line in tabs and ribbons
  (use-package moody
    :config
    ;; uncomment below if using official Emacs for OSX
    (setq x-underline-at-descent-line t)
    (moody-replace-mode-line-buffer-identification)
    (moody-replace-vc-mode)
    ;; modify slant fn if using official Emacs for Mac OS X build to fix colors
    (if (and (eq system-type 'darwin)
             (eq window-system 'ns))
        (setq moody-slant-function 'moody-slant-apple-rgb))))

(provide 'init-local)
;;; init-local.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
