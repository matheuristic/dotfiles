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

(defcustom init-lang-python-conda-dir "~/miniconda3/"
  "Path to conda directory, must end with slash character."
  :type 'string
  :group 'init-lang-el)

;; enable evaluation of Python in Org-mode code blocks
(use-package ob-python
  :ensure nil ;; built-in
  :defer t)

;; configure flymake for Python if not using flycheck
(if (not (featurep 'flycheck))
    (with-eval-after-load 'flymake
      (defun flymake-pylint-init ()
        (let* ((temp-file (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))
               (local-file (file-relative-name
                            temp-file
                            (file-name-directory buffer-file-name))))
          (list "epylint" (list local-file))))
      (add-to-list 'flymake-allowed-file-name-masks
                   '("\\.py\\'" flymake-pylint-init)))
  (add-hook 'python-mode-hook '(lambda () (flycheck-mode))))

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

;; manage python envs, prefer conda to virtualenvwrapper
(if (executable-find "conda")
    (use-package conda
      :pin "MELPA"
      :init (setq conda-anaconda-home init-lang-python-conda-dir)
      :config
      (conda-env-initialize-interactive-shells)
      (conda-env-initialize-eshell)
      ;; display currently active conda env on the mode line
      (setq-default mode-line-format
                    (add-to-list 'mode-line-format
                                 '(:eval (if conda-env-current-name
                                             (format " «%s»"
                                                     (truncate-string-to-width
                                                      conda-env-current-name
                                                      15 nil nil "…"))
                                           ""))
                                 t)) ;; add current conda env to mode-line, if any
      (defhydra my-hydra/conda (:color teal :columns 4)
        "conda"
        ("a" conda-env-activate "activate")
        ("d" conda-env-deactivate "deactivate")
        ("l" conda-env-list "list")
        ("q" nil "quit"))
      (global-set-key (kbd "C-c s-v e") 'my-hydra/conda/body))
  (use-package virtualenvwrapper
    :pin "MELPA"
    :config
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell)
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
    (global-set-key (kbd "C-c s-v e") 'my-hydra/virtualenv/body)))

(provide 'init-lang-python)

;;; init-lang-python.el ends here
