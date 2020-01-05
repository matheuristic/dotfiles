;;; init-lang-clojure.el --- Emacs config Clojure language layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Configure Clojure tooling

;;; Code:

(require 'init-ui-hydra)

;; Basic support
(use-package clojure-mode
  :defer t
  :hook ((clojure-mode . paredit-mode)
         (clojure-mode . rainbow-delimiters-mode)
         (clojure-mode . subword-mode)))

;; IDE
(use-package cider
  :after clojure-mode
  :bind (:map clojure-mode-map
         ("C-c s-m" . my-hydra/cider/body))
  :hook ((cider-mode . eldoc-mode)
         (cider-repl-mode . eldoc-mode)
         (cider-repl-mode . paredit-mode)
         (cider-repl-mode . rainbow-delimiters-mode))
  :config
  (setq nrepl-log-messages t)
  ;; Hydras, adapted from https://github.com/clojure-emacs/cider-hydra
  (defhydra my-hydra/cider (:color teal :columns 3)
    "CIDER"
    ;; Start a REPL and connect
    ("j" cider-jack-in-clj "jack-in-clj")
    ("s" cider-jack-in-cljs "jack-in-cljs")
    ("b" cider-jack-in-clj&cljs "jack-in-clj&cljs")
    ;; Sub-hydras
    ("d" my-hydra/cider-doc/body "→ Documentation")
    ("e" my-hydra/cider-eval/body "→ Evaluation")
    ("T" my-hydra/cider-test/body "→ Test")
    ("D" my-hydra/cider-debug/body "→ Debug")
    ("r" my-hydra/cider-repl/body "→ REPL")
    ("q" nil "quit"))
  (defhydra my-hydra/cider-doc (:color teal :columns 4)
    "CIDER → Documentation"
    ;; CiderDoc
    ("d" cider-doc "cider-docs")
    ;; JavaDoc
    ("j" cider-javadoc "java-docs-web")
    ;; Apropos
    ("a" cider-apropos "search-symbols")
    ("s" cider-apropos-select "select-symbols")
    ("A" cider-apropos-documentation "search-docs")
    ("e" cider-apropos-documentation-select "select-docs")
    ;; ClojureDocs
    ("r" cider-clojuredocs "clojure-docs")
    ("h" cider-clojuredocs-web "clojure-docs-web")
    ("q" my-hydra/cider/body "←"))
  (defhydra my-hydra/cider-eval (:color teal :columns 3)
    "CIDER → Eval"
    ;; Load
    ("k" cider-load-buffer "load-buffer")
    ("l" cider-load-file "load-file")
    ("p" cider-load-all-project-ns "load-all-proj-ns")
    ;; Eval
    ("r" cider-eval-region "eval-region")
    ("n" cider-eval-ns-form "eval-ns-form")
    ("e" cider-eval-last-sexp "eval-last-sexp")
    ("P" cider-pprint-eval-last-sexp "eval-last-sexp-pp")
    ("w" cider-eval-last-sexp-and-replace "eval-last-sexp-replace")
    ("E" cider-eval-last-sexp-to-repl "eval-last-sexp-to-repl")
    ("d" cider-eval-defun-at-point "eval-defun-at-point")
    ("f" cider-pprint-eval-defun-at-point "eval-defun-at-point-pp")
    (":" cider-read-and-eval "read-and-eval")
    ;; Inspect
    ("i" cider-inspect "inspect")
    ;; Macroexpand
    ("m" cider-macroexpand-1 "macroexpand-1")
    ("M" cider-macroexpand-all "macroexpand-all")
    ("q" my-hydra/cider/body "←"))
  (defhydra my-hydra/cider-test (:color teal :columns 4)
    "CIDER → Test"
    ("t" cider-test-run-test "run")
    ("l" cider-test-run-loaded-tests "run-loaded")
    ("p" cider-test-run-project-tests "run-project")
    ("n" cider-test-run-ns-tests "run-ns")
    ("r" cider-test-rerun-failed-tests "rerun-failed")
    ("s" cider-test-show-report "show-report")
    ("q" my-hydra/cider/body "←"))
  (defhydra my-hydra/cider-debug (:color teal :columns 4)
    "CIDER → Debug"
    ("x" (lambda () (interactive) (cider-eval-defun-at-point t)) "eval-defun-at-pt")
    ("v" cider-toggle-trace-var "toggle-var-trace")
    ("n" cider-toggle-trace-ns "toggle-ns-trace")
    ("q" my-hydra/cider/body "←"))
  (defhydra my-hydra/cider-repl (:color teal :columns 4)
    "CIDER → REPL"
    ("d" cider-display-connection-info "disp-conn-info")
    ("r" cider-rotate-default-connection "rot-default-conn")
    ;; Input
    ("z" cider-switch-to-repl-buffer "switch-to-repl")
    ("n" cider-repl-set-ns "set-repl-ns")
    ("p" cider-insert-last-sexp-in-repl "ins-last-sexp-in-repl")
    ("x" cider-refresh "refresh")
    ;; Output
    ("o" cider-find-and-clear-repl-output "clear-repl-output")
    ("O" (lambda () (interactive) (cider-find-and-clear-repl-output t)) "clear-repl-all")
    ;; Interrupt/quit
    ("b" cider-interrupt "interrupt")
    ("Q" cider-quit "quit-cider")
    ("q" my-hydra/cider/body "←")))

;; Linting, requires clj-kondo be installed on the system
(when (executable-find "clj-kondo")
  (if (not (featurep 'flycheck))
      ;; Flymake, adapted from https://github.com/turbo-cafe/flymake-kondor
      ;; requires 'flymake-quickdef
      (with-eval-after-load 'flymake-quickdef
        (flymake-quickdef-backend flymake-clj-kondo-backend
          :pre-let ((clj-kondo-exec (executable-find "clj-kondo")))
          :pre-check (unless clj-kondo-exec (error "Cannot find clj-kondo executable"))
          :write-type 'pipe
          :proc-form (list clj-kondo-exec "--lint" "-")
          :search-regexp "^.+:\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\([[:alpha:]]+\\): \\(.+\\)$"
          :prep-diagnostic
          (let* ((lnum (string-to-number (match-string 1)))
                 (lcol (string-to-number (match-string 2)))
                 (severity (match-string 3))
                 (msg (match-string 4))
                 (pos (flymake-diag-region fmqd-source lnum lcol))
                 (beg (car pos))
                 (end (cdr pos))
                 (type (cond
                        ((string= severity "error") :error)
                        ((string= severity "warning") :warning)
                        ((string= severity "info") :note)
                        (t :note))))
            (list fmqd-source beg end type msg)))
        (defun flymake-clj-kondo-setup ()
          "Enable clj-kondo backend for Flymake."
          (add-hook 'flymake-diagnostic-functions #'flymake-clj-kondo-backend nil t))
        (add-hook 'clojure-mode-hook 'flymake-clj-kondo-setup)
        (add-hook 'clojure-mode-hook 'flymake-mode))
    ;; Flycheck
    (progn
      (use-package flycheck-clj-kondo)
      (with-eval-after-load 'clojure-mode
        (require 'flycheck-clj-kondo)))))

(provide 'init-lang-clojure)

;;; init-lang-clojure.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
