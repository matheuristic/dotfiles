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

(defcustom init-lang-python-ms-executable nil
  "Path to Microsoft Python Language Server executable."
  :type 'string
  :group 'init-lang-el)

(defcustom init-lang-venv-dir "~/miniconda3/envs"
  "Path to conda or virtualenv environments directory."
  :type 'string
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
    (define-key lsp-mode-map (kbd "H-L") 'my-hydra/lsp/body)))

;; front-end for interacting with debug servers
;; (use-package dap-mode
;;   :pin "MELPA"
;;   :after lsp-mode
;;   :config
;;   (dap-mode 1)
;;   (dap-ui-mode 1))


;; TODO - replace with conda after it gets fixed
;; virtualenv tool
(use-package virtualenvwrapper
  ;; enable the MELPA repository and uncomment below if the version of
  ;; virtualenvwrapper.el in MELPA Stable is too old for emacs-traad
  :pin "MELPA"
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  ;; set virtualenv storage dir if it differs from default ~/.virtualenvs
  (setq venv-location init-lang-venv-dir)
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

;;;;
;; Specific languages
;;;;

;; CSV
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
    (define-key csv-mode-map (kbd "H-m") 'my-hydra/csv-mode/body)))

;; Dockerfile
(use-package dockerfile-mode
  :commands dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; Emacs Speaks Statistics
;; has built-in flymake support (requires the R lintr pkg be installed)
(use-package ess
  :mode (("\\.R$" . R-mode)
         ("\\.jl$" . julia-mode))
  :commands (R-mode
             julia-mode
             ess-eval-function
             ess-eval-line
             ess-eval-buffer
             ess-switch-to-ESS))

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
    (define-key gfm-mode-map (kbd "H-m") 'my-hydra/markdown-mode/body)))

;; Python
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython" ;; use IPython for REPL
        python-shell-interpreter-args "--simple-prompt -i")) ;; fancy prompts don't work in Eshell
(if (not (featurep 'flycheck)) ;; configure flymake for Python if not using flycheck
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
  (add-hook 'python-mode-hook '(lambda () (flymake-mode))))
(when init-lang-python-ms-executable
  (use-package lsp-python-ms ;; use Microsoft Python Language Server
    ;; :load-path "site-lisp"
    :pin "MELPA"
    :after lsp-mode
    :config
    (setq lsp-python-ms-executable init-lang-python-ms-executable)
    (add-hook 'python-mode-hook #'lsp)))

;; YAML - MELPA Stable
(use-package yaml-mode
  :commands yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

(provide 'init-lang)

;;; init-lang.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
