;;; init-lang.el --- Emacs config language layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up programming language support

;;; Code:

(require 'init-ui-hydra)

(defgroup init-lang-el nil
  "Language-specific settings."
  :group 'convenience)

(defcustom init-lang-enable-list '("bibtex" "clojure" "csv" "docker" "json"
                                   "julia" "lisp" "markdown" "plantuml"
                                   "python" "r" "scheme" "yaml")
  "List of languages for which to enable support."
  :type '(repeat string)
  :group 'init-lang-el)

;; Language Server Protocol
(use-package lsp-mode
  :pin "MELPA"
  :defer t
  :hook (prog-mode . (lambda () (require 'lsp-mode)))
  :bind (:map lsp-mode-map
         ("C-c s-l" . my-hydra/lsp/body))
  :config
  (setq lsp-print-io nil ;; disable logging of packets between emacs and the LS
        lsp-eldoc-enable-hover nil ;; don't have eldoc display hover info
        lsp-eldoc-enable-signature-help t ;; display signature help in minibuffer
        lsp-eldoc-prefer-signature-help t ;; prefer displaying signature help to hover
        lsp-eldoc-render-all nil ;; don't show all returned from document/onHover, only symbol info
        lsp-enable-on-type-formatting nil ;; don't have the LS automatically format the document when typing
        lsp-prefer-flymake (if (featurep 'flycheck) nil t)) ;; prefer flycheck for syntax checking if it's loaded, otherwise prefer flymake
  ;; company backend for LSP-driven completion
  (use-package company-lsp
    :pin "MELPA"
    :commands company-lsp
    :init (setq company-lsp-cache-candidates t))
  (defhydra my-hydra/lsp (:color teal :hint nil)
    "
Language Server

Server  _R_   : (re)start        _S_   : shutdown         _M-l_ : IO log

Symbol  _o_   : describe         _fd_  : find declaration _fD_  : find defn
        _fi_  : find implementn  _fr_  : find references  _ft_  : find type defn
        _h_   : highlight        _M-r_ : rename

Lens    _lm_  : lsp-lens-mode    _ls_  : show lenses      _lh_  : hide lenses

Other   _FB_  : format buffer    _FR_  : format region    _X_   : execute action
        _O_   : organize imports _M-s_ : describe session

"
    ;; Server
    ("R" (condition-case nil (lsp-restart-workspace) (error (lsp))))
    ("S" lsp-shutdown-workspace)
    ("M-l" lsp-switch-to-io-log-buffer)
    ;; Symbol at point
    ("o" lsp-describe-thing-at-point)
    ("fd" lsp-find-declaration)
    ("fD" lsp-find-definition)
    ("fi" lsp-find-implementation)
    ("fr" lsp-find-references)
    ("ft" lsp-find-type-definition)
    ("h" lsp-document-highlight)
    ("M-r" lsp-rename)
    ;; CodeLens (Microsoft Language Servers)
    ("lm" lsp-lens-mode)
    ("ls" lsp-lens-show)
    ("lh" lsp-lens-hide)
    ;; Other
    ("FB" lsp-format-buffer)
    ("FR" lsp-format-region)
    ("X" lsp-execute-code-action)
    ("O" lsp-organize-imports)
    ("M-s" lsp-describe-session)
    ("q" nil "quit")))

;; front-end for interacting with debug servers
(use-package dap-mode
  :pin "MELPA"
  :commands dap-mode
  :after lsp-mode
  :bind (:map dap-mode-map
         ("C-c s-d b" . my-hydra/dap/body))
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
           ("C-c s-m" . my-hydra/csv-mode/body))
    :config (defhydra my-hydra/csv-mode (:color teal :columns 4)
              "CSV mode"
              ("s" csv-sort-fields "sort")
              ("r" csv-sort-numeric-fields "numsort")
              ("k" csv-kill-fields "cut")
              ("y" csv-yank-fields "copy")
              ("a" csv-align-fields "align")
              ("u" csv-unalign-fields "unalign")
              ("t" csv-transpose "transpose")
              ("q" nil "quit"))))

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
;; Org references to bibliography entries can be inserted by pressing `i' when
;; on an entry in ebib or by calling `ebib-insert-citation' in org-mode
;;
;; to export references from Org to LaTeX, ebib needs to be opened with the
;; bibliographies for the references that appear in the document
;;
;; use "::" in the Org link description to separate the preamble text,
;; pre-note and post-note elements (all optional) for export to LaTeX,
;; i.e. "[[ebib:key][Preamble text::Pre-note::Post-note]]"
;; will export to "Preamble text\cite[Pre-note][Post-note]{key}"
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
    :bind ("C-c s-b e" . ebib)
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
                 (format "%s\\cite%s%s{%s}"
                         (concat desc-name " ")
                         (if (string= "" desc-pre-note) "" (format "[%s]" desc-pre-note))
                         (if (string= "" desc-post-note) "" (format "[%s]" desc-post-note))
                         path)
               (format "\\cite{%s}" path))))))
      (org-link-set-parameters "ebib" :export 'my-org-ebib-export)
      (bind-key "C-c s-b i" 'ebib-insert-citation org-mode-map))))

;; Clojure
(when (member "clojure" init-lang-enable-list)
  (require 'init-lang-clojure))

;; Emacs Speaks Statistics
;; has built-in flymake support (requires R lintr be installed)
(when (or (member "julia" init-lang-enable-list)
          (member "r" init-lang-enable-list))
  (use-package ess
    :pin "MELPA"
    :mode (("\\.R$" . R-mode)
           ("\\.jl$" . julia-mode))
    :commands (R-mode julia-mode ess-switch-to-ESS)
    :bind (:map ess-mode-map
           ("C-c s-m" . my-hydra/ess/body))
    :init (setq ess-eval-visibly 'nowait
                ess-default-style 'RStudio)
    :config
    (defhydra my-hydra/ess (:color teal :hint nil)
      "
Emacs Speaks Statistics

Session     _N_   : new     _R_   : request _s_   : switch  _C-q_ : quit

Eval        _l_   : line    _f_   : func    _r_   : region  _b_   : buffer

Workspace   _D_   : chdir   _d_   : R dired

Help        _h_   : object  _H_   : browser _A_   : apropos

"
      ;; session
      ("N" (lambda () (interactive)
             (cond ((string= ess-dialect "R") (R))
                   ((string= ess-dialect "julia") (julia))
                   (t (message "Unsupported dialect")))))
      ("R" ess-request-a-process)
      ("s" ess-switch-to-ESS)
      ("C-q" ess-quit)
      ;; eval
      ("l" ess-eval-line)
      ("f" ess-eval-function)
      ("r" ess-eval-region)
      ("b" ess-eval-buffer)
      ;; workspace
      ("D" ess-change-directory)
      ("d" ess-rdired)
      ;; help
      ("h" ess-display-help-on-object)
      ("H" ess-display-help-in-browser)
      ("A" ess-display-help-apropos)
      ("q" nil "quit"))))

;; JSON
(when (member "json" init-lang-enable-list)
  (use-package json-mode
    :commands json-mode))

;; LISP
(when (member "lisp" init-lang-enable-list)
  (require 'init-lang-lisp))

;; Markdown
(when (member "markdown" init-lang-enable-list)
  (use-package markdown-mode
    :pin "MELPA"
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode)
           ("\\.Rmd\\'" . markdown-mode)) ; R markdown
    :bind (:map markdown-mode-map
           ("C-c s-m" . my-hydra/markdown-mode/body)
           :map gfm-mode-map
           ("C-c s-m" . my-hydra/markdown-mode/body))
    :config
    ;; Markdown table of contents
    (use-package markdown-toc
      :pin "MELPA")
    ;; Github-flavored Markdown/Org preview using grip
    ;; requires Python grip package be installed
    (use-package grip-mode
      :bind (:map markdown-mode-command-map ("g" . grip-mode)))
    ;; render mathematical expressions in HTML preview
    (setq markdown-xhtml-header-content
          (concat "<script type=\"text/x-mathjax-config\">"
                  "MathJax.Hub.Config({"
                  "  tex2jax: {"
                  "    inlineMath: [ ['$','$'], [\"\\(\",\"\\)\"] ],"
                  "    processEscapes: true"
                  "  }"
                  "});"
                  "</script>"
                  "<script type=\"text/javascript\" async"
                  "        src=\"https://cdnjs.cloudflare.com/ajax/libs/"
                  "mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML\">"
                  "</script>"))
    ;; syntax highlighting in fenced code blocks
    (setq markdown-fontify-code-blocks-natively t)
    (defhydra my-hydra/markdown-mode (:color teal :hint nil)
      "
Markdown mode

Keymaps     _c_ : commands  _s_ : styles

Outline     _n_ : next      _p_ : prev      _f_ : fwd-level _b_ : bwd-level
            _←_ : promote   _→_ : demote    _↓_ : move-down _↑_ : move-up

Shift-Rgn   _<_ : left      _>_ : right

Toggle      _E_ : math      _F_ : code-font _I_ : images    _L_ : url
            _M_ : markup

Other       _d_ : do        _o_ : follow    _'_ : edit code block
            _t_/_C-t_ : insert/remove table of contents

"
      ;; keymaps
      ("c" (lambda () (interactive) (setq unread-command-events (listify-key-sequence "\C-c\C-c"))))
      ("s" (lambda () (interactive) (setq unread-command-events (listify-key-sequence "\C-c\C-s"))))
      ;; outline
      ("n" markdown-outline-next :color red)
      ("p" markdown-outline-previous :color red)
      ("f" markdown-outline-next-same-level :color red)
      ("b" markdown-outline-previous-same-level :color red)
      ("<left>" markdown-promote :color red)
      ("<right>" markdown-demote :color red)
      ("<down>" markdown-move-down :color red)
      ("<up>" markdown-move-up :color red)
      ;; shift region
      ("<" markdown-outdent-region :color red)
      (">" markdown-indent-region :color red)
      ;; toggle user interface
      ("E" markdown-toggle-math)
      ("F" markdown-toggle-fontify-code-blocks-natively)
      ("I" markdown-toggle-inline-images)
      ("L" markdown-toggle-url-hiding)
      ("M" markdown-toggle-markup-hiding)
      ;; other
      ("d" markdown-do)
      ("o" markdown-follow-thing-at-point)
      ("'" markdown-edit-code-block)
      ("t" markdown-toc-generate-or-refresh-toc)
      ("C-t" markdown-toc-delete-toc)
      ;; quit
      ("q" nil "quit"))))

;; Pandoc wrapper for converting between document formats
;; Use C-c / to access pandoc options and settings
(use-package pandoc-mode
  :commands (pandoc-mode pandoc-load-default-settings)
  :hook ((pandoc-mode . pandoc-load-default-settings)
         (markdown-mode . pandoc-mode)))

;; PlantUML
;;
;; graphviz and java needs to be installed on the system
;;
;; the plantuml.jar file needs to be downloaded to `plantuml-jar-path' (the
;; default value is "~/plantuml.jar") which can be done manually or by using
;; "M-x plantuml-download-jar"
;;
;; there is also Org source block support (edit a block with "C-c '" and
;; evaluate with "C-c C-c"), for example:
;; ---
;; ...
;; #+BEGIN_SRC plantuml :file diagram.png
;;   @startuml
;;   [*] --> State1
;;   State1 --> [*]
;;   State1 : a line description
;;   State1 : another line description
;;   State1 --> State2
;;   State2 --> [*]
;;   @enduml
;; #+END_SRC
;; ...
;; ---
(when (member "plantuml" init-lang-enable-list)
  (use-package plantuml-mode
    :defer t
    :mode ("\\.p\\(lant\\)?uml" . plantuml-mode)
    :init (setq plantuml-default-exec-mode 'jar)
    :config (with-eval-after-load 'org
              (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
              (setq org-plantuml-jar-path plantuml-jar-path)
              (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))))

;; Python
(when (member "python" init-lang-enable-list)
  (require 'init-lang-python))

;; Scheme
(when (member "scheme" init-lang-enable-list)
  (require 'init-lang-scheme))

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
  :bind ("C-c s-j" . my-hydra/jupyter/body)
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
