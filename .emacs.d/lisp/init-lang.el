;;; init-lang.el --- Emacs config language layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up support for various programming languages and their tooling

;;; Code:

(require 'init-ui-hydra)

(defgroup init-lang-el nil
  "Language-specific settings."
  :group 'convenience)

(defcustom init-lang-enable-list '("csv" "docker" "json" "julia" "markdown"
                                   "python" "r" "yaml")
  "List of languages for which to enable support."
  :type '(repeat string)
  :group 'init-lang-el)

;; Language Server Protocol
(use-package lsp-mode
  :pin "MELPA"
  :defer 1
  :config
  ;; change nil to 't to enable logging of packets between emacs and the LS
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
    (defun my-lsp-ui-doc-mode-toggle ()
      "Toggles `lsp-ui-doc-mode'."
      (interactive)
      (if lsp-ui-doc-mode
          (progn
            (lsp-ui-doc-mode -1)
            (lsp-ui-doc--hide-frame))
        (lsp-ui-doc-mode 1))))
  ;; install LSP company backend for LSP-driven completion
  (use-package company-lsp
    :pin "MELPA"
    :commands company-lsp
    :config (setq company-lsp-cache-candidates t))
  (defhydra my-hydra/lsp (:color teal :hint nil)
    "
Language Server Protocol

Buffer  _f_   : format          _m_   : imenu           _x_   : execute action

Server  _M-r_ : restart         _S_   : shutdown        _M-s_ : describe session

Symbol  _d_   : declaration     _D_   : definition      _R_   : references
        _i_   : implementation  _t_   : type            _o_   : documentation
        _r_   : rename

Other   _C-d_ : toggle docs

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
    ("C-d" my-lsp-ui-doc-mode-toggle)
    ("q" nil "quit" :color blue))
  (define-key lsp-mode-map (kbd "H-l") 'my-hydra/lsp/body))

;; front-end for interacting with debug servers
(use-package dap-mode
  :pin "MELPA"
  :after lsp-mode
  :config
  (require 'dap-hydra)
  (require 'dap-ui)
  (dap-mode 1)
  (dap-ui-mode 1)
  (defhydra my-hydra/dap (:color teal :columns 4)
    "Debug Adapter Protocol"
    ;; session management
    ("dd" dap-debug "debug")
    ("dl" dap-debug-last "debug-last")
    ("dr" dap-debug-recent "debug-recent")
    ("A" dap-delete-all-sessions "del-all-sessions")
    ;; windows
    ("wo" dap-go-to-output-buffer "output-buf")
    ("wb" dap-ui-breakpoints "breakpoints")
    ("wl" dap-ui-locals "locals")
    ("ws" dap-ui-sessions "sessions")
    ;; repl
    ("'" dap-ui-repl "repl")
    ;; dap-mode operations
    ("." dap-hydra "dap-hydra")
    ("q" nil "quit"))
  (define-key dap-mode-map (kbd "H-D") 'my-hydra/dap/body))

;; CSV
(when (member "csv" init-lang-enable-list)
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
    (define-key csv-mode-map (kbd "H-m") 'my-hydra/csv-mode/body)))

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

;; JSON
(when (member "json" init-lang-enable-list)
  (use-package json-mode
    :commands json-mode))

;; Markdown
(when (member "markdown" init-lang-enable-list)
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
      ("q" nil "quit" :color blue)
    (define-key markdown-mode-map (kbd "H-m") 'my-hydra/markdown-mode/body)
    (define-key gfm-mode-map (kbd "H-m") 'my-hydra/markdown-mode/body))))

;; Python
(when (member "python" init-lang-enable-list)
  (require 'init-lang-python))

;; YAML
(when (member "yaml" init-lang-enable-list)
  (use-package yaml-mode
    :commands yaml-mode
    :mode ("\\.ya?ml\\'" . yaml-mode)))

;; support for interfacing with Jupyter kernels
;; when using virtualenvs, make sure the activated virtualenv has jupyter
;; installed before running `jupyter-run-repl'
;; load this package last, since org-mode babel support requires core language
;; support be loaded first
;; after `ob-jupyter' is loaded, in org-mode use "C-c h" to call the
;; `jupyter-org-hydra' hydra
;; to execute a source block in org-mode asynchronously, set the `:async'
;; parameter to `yes'
;; #+BEGIN_SRC jupyter-python :session py :async yes
;;   x = 'foo'
;;   y = 'bar'
;;   x + ' ' + y
;; #+END_SRC
(use-package jupyter
  :defer t
  :init
  (defun my-jupyter-status-string ()
    "Returns string showing jupyter cmd availability, if ob-jupyter is initialized, and if a REPL is associated with the current buffer."
    (let ((cmd-available (executable-find "jupyter"))
          (pkg-initialized (featurep 'ob-jupyter))
          (repl-associated (bound-and-true-p jupyter-repl-interaction-mode)))
      (mapconcat 'identity
                 (list (concat "cmd=" (if cmd-available "✔" "✖"))
                       (concat "org=" (if pkg-initialized "✔" "✖"))
                       (concat "repl=" (if repl-associated "✔" "✖")))
                 " ")))
  (defhydra my-hydra/jupyter (:color teal :hint nil)
    "
Jupyter (%s(my-jupyter-status-string))

REPL   _R_   : run         _C_   : connect     _A_   : assoc-buf   _C-s_ : scratch-buf

Kernel _C-r_ : restart     _C-i_ : interrupt

Eval   _C-c_ : line/region _C-b_ : buffer      _M-x_ : defun       _M-:_ : string

Other  _O_   : org-init    _o_   : org-menu    _M-i_ : inspect

"
    ("R" jupyter-run-repl)
    ("C" jupyter-connect-repl)
    ("A" jupyter-repl-associate-buffer)
    ("C-s" jupyter-repl-scratch-buffer)
    ("C-r" jupyter-repl-restart-kernel)
    ("C-i" jupyter-repl-interrupt-kernel)
    ("C-c" jupyter-eval-line-or-region)
    ("C-b" jupyter-eval-buffer)
    ("M-x" jupyter-eval-defun)
    ("M-:" jupyter-eval-string)
    ("O" (progn (require 'ob-jupyter) (message "Org support initialized.")))
    ("o" (if (featurep 'ob-jupyter) (jupyter-org-hydra/body) (message "Org support not yet initialized. Initialize with \"(require 'ob-jupyter)\".")))
    ("M-i" jupyter-inspect-at-point)
    ("q" nil "quit"))
  (defhydra my-hydra/jupyter-repl (:color teal)
    "Jupyter REPL"
    ("C-s" jupyter-repl-scratch-buffer "scratch-buffer")
    ("C-r" jupyter-repl-restart-kernel "restart-kernel")
    ("C-i" jupyter-repl-interrupt-kernel "interrupt-kernel")
    ("q" nil "quit"))
  (global-set-key (kbd "H-j") 'my-hydra/jupyter/body))

(provide 'init-lang)

;;; init-lang.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
