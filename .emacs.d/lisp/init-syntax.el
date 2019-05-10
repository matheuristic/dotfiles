;;; init-syntax.el --- Emacs config syntax checking layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up syntax checker

;;; Code:

(require 'init-ui-hydra)

(defgroup init-syntax-el nil
  "Syntax checker."
  :group 'convenience)

(defcustom init-syntax-use-flycheck nil
  "Whether to use flycheck instead of built-in flymake."
  :type 'boolean
  :group 'init-syntax-el)

(if init-syntax-use-flycheck
    ;; use flycheck if specified ...
    (use-package flycheck
      :delight flycheck-mode
      :init (global-flycheck-mode)
      :config
      (defhydra my-hydra/flycheck (:color amaranth :columns 6)
        "Error"
        ("F" flycheck-error-list-set-filter "filter")
        ("p" flycheck-previous-error "previous")
        ("n" flycheck-next-error "next")
        ("f" flycheck-first-error "first")
        ("l" (condition-case nil (while t (flycheck-next-error))
               (user-error nil)) "last")
        ("L" (condition-case nil (quit-windows-on "*Flycheck errors*" t)
               (error (flycheck-list-errors))) "list")
        ("q" nil "quit" :exit t))
      ;; bind over my-hydra/error
      (define-key flycheck-mode-map (kbd "H-e") 'my-hydra/flycheck/body))
  ;; ... otherwise use built-in flymake
  (use-package flymake  ;; NOTE use C-h . to show error on current line
    :config
    (use-package flymake-diagnostic-at-point
      :config
      (setq flymake-diagnostic-at-point-error-prefix "» ")
      (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))
    (add-hook 'emacs-lisp-mode-hook 'flymake-mode)
    (defun my-toggle-flymake-diagnostics ()
      "Toggles flymake diagnostics window for current buffer."
      (interactive)
      (if flymake-mode
          (let* ((buf-name (buffer-name (current-buffer)))
                 (flymake-winds (condition-case nil
                                    (get-buffer-window-list
                                     (concat "*Flymake diagnostics for "
                                             buf-name
                                             "*"))
                                  (error nil))))
            (if flymake-winds
                (dolist (wind flymake-winds) (quit-window nil wind))
              (flymake-show-diagnostics-buffer)))))
    (defhydra my-hydra/flymake (:color amaranth)
      "Error"
      ("p" flymake-goto-prev-error "previous")
      ("n" flymake-goto-next-error "next")
      ("L" my-toggle-flymake-diagnostics "list")
      ("q" nil "quit" :exit t))
    (define-key flymake-mode-map (kbd "H-e") 'my-hydra/flymake/body)
    (with-eval-after-load 'minions
      (add-to-list 'minions-direct 'flymake-mode))))

(provide 'init-syntax)

;;; init-syntax.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
