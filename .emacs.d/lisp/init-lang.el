;;; init-lang.el --- Emacs config language layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up support for various programming languages and their tooling

;;; Code:

(require 'init-ui-hydra)

(defgroup init-lang-el nil
  "Language-specific settings."
  :group 'convenience)

(defcustom init-lang-enable-list '("bibtex" "csv" "docker" "json" "julia"
                                   "markdown" "python" "r" "yaml")
  "List of languages for which to enable support."
  :type '(repeat string)
  :group 'init-lang-el)

;; Language Server Protocol
(use-package lsp-mode
  :pin "MELPA"
  :defer t
  :hook (prog-mode . (lambda () (require 'lsp-mode)))
  :bind (:map lsp-mode-map
         ("H-l" . my-hydra/lsp/body))
  :config
  (setq lsp-print-io nil ;; change nil to t to enable logging of packets between emacs and the LS
        lsp-eldoc-enable-hover nil ;; don't have eldoc display hover info
        lsp-eldoc-enable-signature-help nil ;; don't have eldoc display signature help
        lsp-prefer-flymake t) ;; set to nil to prefer flycheck to flymake
  ;; lsp-ui enables pop-up documentation boxes and sidebar info
  (use-package lsp-ui
    :pin "MELPA"
    :commands lsp-ui-mode
    :bind (:map lsp-ui-mode-map
           ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
           ([remap xref-find-references] . lsp-ui-peek-find-references))
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
    :init (setq company-lsp-cache-candidates t))
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
    ("q" nil "quit" :color blue)))

;; front-end for interacting with debug servers
(use-package dap-mode
  :pin "MELPA"
  :after lsp-mode
  :bind (:map dap-mode-map
         ("H-D" . my-hydra/dap/body))
  :init
  (require 'dap-hydra)
  (require 'dap-ui)
  :config
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
    ("q" nil "quit")))

;; CSV
(when (member "csv" init-lang-enable-list)
  (use-package csv-mode
    :commands csv-mode
    :bind (:map csv-mode-map
           ("H-m" . my-hydra/csv-mode/body))
    :config (defhydra my-hydra/csv-mode (:color teal :columns 4)
              "CSV mode"
              ("s" csv-sort-fields "sort")
              ("r" csv-sort-numeric-fields "numsort")
              ("k" csv-kill-fields "cut")
              ("y" csv-yank-fields "copy")
              ("a" csv-align-fields "align")
              ("u" csv-unalign-fields "unalign")
              ("t" csv-transpose "transpose")
              ("q" nil "quit" :color blue))))

;; Dockerfile
(when (member "docker" init-lang-enable-list)
  (use-package dockerfile-mode
    :commands dockerfile-mode
    :config (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))))

;; BibTeX
;;
;; this setup supports exporting Org to PDF with BibTeX bibliographies via
;; xelatex and biber, so they will need to be installed on the system
;;
;; Org documents should include the LaTeX headers for bibliographies via
;; "#+LATEX_HEADER:" structural markup elements and "\printbibliography"
;; should be added at the desired location for the bibliography (typically
;; at the end of an article or book chapter or before the index)
;;
;; Org references to bibliography entries can be inserted by calling
;; `ebib-insert-citation' while in org-mode or by pressing `i' when on an entry
;; in ebib
;;
;; to export references from Org to LaTeX, ebib needs to be opened with the
;; bibliographies for the references that appear in the document
;;
;; use "::" in the Org-link description to separate the pre-reference text,
;; pre-note and post-note elements (all optional) for the LaTeX export,
;; i.e. "[[ebib:key][Pre-reference text::Pre-note::Post-note]]"
;; will export to "Pre-reference text\cite[Pre-note][Post-note]{key}"
;;
;; example:
;; ---
;; ...
;; #+LATEX_HEADER: \usepackage[backend=biber]{biblatex}
;; #+LATEX_HEADER: \addbibresource{path/to/bibtex_file.bib}
;; ...
;; [[ebib:some_ebib_entry_key]]
;; [[ebib:some_ebib_entry_key][Preamble]
;; [[ebib:some_ebib_entry_key][Preamble::::Post-note]
;; [[ebib:some_ebib_entry_key][Preamble::Pre-note::Post-note]]
;; [[ebib:incognito_1970][::see::pg. 99]]
;; ...
;; \printbibliography
;; ...
;; ---
(when (member "bibtex" init-lang-enable-list)
  ;; BibTeX reference manager
  (use-package ebib
    :commands ebib
    :bind ("H-B" . ebib)
    :config
    (with-eval-after-load 'org
      (require 'org-ebib)
      ;; compile LaTeX to PDF using xelatex and biber (for bibliographies)
      (setq org-latex-pdf-process '("xelatex -interaction nonstopmode -output-directory %o %f"
                                    "biber %b"
                                    "xelatex -interaction nonstopmode -output-directory %o %f"
                                    "xelatex -interaction nonstopmode -output-directory %o %f"))
      (defun my-org-ebib-export (path desc format)
        "Export an ebib link. See `org-link-parameters' for details about PATH, DESC and FORMAT."
        (let* ((my-desc (or desc ""))
               (desc-parts (split-string my-desc "::"))
               (desc-name (car desc-parts))
               (desc-pre-note (or (nth 1 desc-parts) ""))
               (desc-post-note (mapconcat 'identity (nthcdr 2 desc-parts) "::")))
          (cond
            ((eq format 'html)
             (if desc
                 (format "(%s<cite>%s</cite>%s)"
                         (if (string= "" desc-pre-note) "" (concat desc-pre-note " "))
                         (if (string= "" desc-name) path desc-name)
                         (if (string= "" desc-post-note) "" (concat ", " desc-post-note)))
               (format "(<cite>%s</cite>)" path)))
            ((eq format 'latex)
             (if desc
                 (format "%s\\cite[%s][%s]{%s}" desc-name desc-pre-note desc-post-note path)
               (format "\\cite{%s}" path))))))
      (org-link-set-parameters "ebib" :export 'my-org-ebib-export)
      (bind-key "H-i" 'ebib-insert-citation org-mode-map))))

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
    :bind (:map markdown-mode-map
           ("H-m" . my-hydra/markdown-mode/body)
           :map gfm-mode-map
           ("H-m" . my-hydra/markdown-mode/body))
    :config
    ;; Markdown table of contents
    (use-package markdown-toc)
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
      ("q" nil "quit" :color blue))))

;; Python
(when (member "python" init-lang-enable-list)
  (require 'init-lang-python))

;; YAML
(when (member "yaml" init-lang-enable-list)
  (use-package yaml-mode
    :commands yaml-mode
    :mode ("\\.ya?ml\\'" . yaml-mode)))

;; support for interfacing with Jupyter kernels
;;
;; when using virtualenvs, make sure the activated virtualenv has jupyter
;; installed before running `jupyter-run-repl'
;;
;; load `ob-jupyter' last, since org-mode babel support requires core language
;; support be loaded first
;;
;; after `ob-jupyter' is loaded, "C-c h" in org-mode calls `jupyter-org-hydra'
;; to execute a source block in org-mode asynchronously, set the `:async'
;; parameter to `yes'
;;
;; example:
;; ---
;; #+BEGIN_SRC jupyter-python :session py :async yes
;;   x = 'foo'
;;   y = 'bar'
;;   x + ' ' + y
;; #+END_SRC
;; ---
(use-package jupyter
  :defer t
  :bind ("H-j" . my-hydra/jupyter/body)
  :init
  (defun my-jupyter-status-string ()
    "Returns string showing availability of jupyter cmd, if `ob-jupyter' is initialized, and if a REPL is associated with the current buffer."
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
    ("O" (progn
           (require 'ob-jupyter)
           (message "Org support initialized.")))
    ("o" (if (featurep 'ob-jupyter)
             (jupyter-org-hydra/body)
           (message "Org support not yet initialized. Initialize with \"(require 'ob-jupyter)\".")))
    ("M-i" jupyter-inspect-at-point)
    ("q" nil "quit")))

(provide 'init-lang)

;;; init-lang.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
