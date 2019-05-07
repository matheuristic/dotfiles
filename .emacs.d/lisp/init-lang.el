;;; init-lang.el --- Emacs config language layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up support for various programming languages and their tooling

;;; Code:

;;;;
;; Custom variables
;;;;

(defgroup init-lang-el nil
  "Language-specific settings."
  :group 'convenience)

(defcustom init-lang-enable-list '("csv"
                                   "docker"
                                   "json"
                                   "julia"
                                   "markdown"
                                   "python"
                                   "r"
                                   "yaml")
  "List of languages for which to enable support."
  :type '(repeat string)
  :group 'init-lang-el)

;;;;
;; Multi-language tools
;;;;

;; Language Server Protocol
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
    (define-key lsp-mode-map (kbd "H-l") 'my-hydra/lsp/body)))

;; front-end for interacting with debug servers
;; (use-package dap-mode
;;   :pin "MELPA"
;;   :after lsp-mode
;;   :config
;;   (dap-mode 1)
;;   (dap-ui-mode 1))

;;;;
;; Specific languages
;;;;

;; CSV
(when (member "csv" init-lang-enable-list)
  (use-package csv-mode
    :commands csv-mode
    :config
    (with-eval-after-load 'hydra
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
      (define-key csv-mode-map (kbd "H-m") 'my-hydra/csv-mode/body))))

;; Dockerfile
(when (member "docker" init-lang-enable-list)
  (use-package dockerfile-mode
    :commands dockerfile-mode
    :config
    (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))))

;; Emacs Speaks Statistics
;; has built-in flymake support (requires the R lintr pkg be installed)
(when (or (member "julia" init-lang-enable-list)
          (member "r" init-lang-enable-list))
  (use-package ess
    :mode (("\\.R$" . R-mode)
           ("\\.jl$" . julia-mode))
    :commands (R-mode
               julia-mode
               ess-eval-function
               ess-eval-line
               ess-eval-buffer
               ess-switch-to-ESS)))

;; JSON - GNU ELPA
(when (member "json" init-lang-enable-list)
  (use-package json-mode
    :commands json-mode))

;; Markdown - MELPA Stable
(when (member "markdown" init-lang-enable-list)
  (use-package markdown-mode
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (use-package markdown-toc)  ;; Markdown table of contents
    :config
    (with-eval-after-load 'hydra
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
      (define-key gfm-mode-map (kbd "H-m") 'my-hydra/markdown-mode/body))))

;; Python
(when (member "python" init-lang-enable-list)
  (require 'init-lang-python))

;; YAML - MELPA Stable
(when (member "yaml" init-lang-enable-list)
  (use-package yaml-mode
    :commands yaml-mode
    :mode ("\\.ya?ml\\'" . yaml-mode)))

(provide 'init-lang)

;;; init-lang.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
