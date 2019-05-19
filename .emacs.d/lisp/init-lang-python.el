;;; init-lang-python.el --- Emacs config Python language layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up Python tooling

;;; Code:

(require 'init-ui-hydra)

(defgroup init-lang-python-el nil
  "Language-specific settings."
  :group 'convenience)

(defcustom init-lang-python-ms-executable nil
  "Path to Microsoft Python Language Server executable."
  :type 'string
  :group 'init-lang-el)

(defcustom init-lang-python-venv-dir "~/miniconda3/envs"
  "Path to virtualenv (or conda) environments directory."
  :type 'string
  :group 'init-lang-el)

;; enable evaluation of Python in Org-mode code blocks
(use-package ob-python
  :ensure nil)

;; use IPython for REPL
;; fall back to simple prompts since eshell does not support fancy prompts
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"))

;; configure flymake for Python if not using flycheck
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
  (add-hook 'python-mode-hook '(lambda () (flymake-mode))))

;; lsp-mode support for Microsoft Python Language Server
(when init-lang-python-ms-executable
  (use-package lsp-python-ms
    ;; :load-path "site-lisp"
    :pin "MELPA"
    :after lsp-mode
    :config
    (setq lsp-python-ms-executable init-lang-python-ms-executable)
    (add-hook 'python-mode-hook #'lsp)
    (with-eval-after-load 'dap-mode
      (require 'dap-python))))

;; virtualenv tool
(use-package virtualenvwrapper
  ;; enable the MELPA repository and uncomment below if the version of
  ;; virtualenvwrapper.el in MELPA Stable is too old for emacs-traad
  :pin "MELPA"
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  ;; set virtualenv storage dir if it differs from default ~/.virtualenvs
  (setq venv-location init-lang-python-venv-dir)
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
  (global-set-key (kbd "H-v") 'my-hydra/virtualenv/body))

(provide 'init-lang-python)

;;; init-lang-python.el ends here
