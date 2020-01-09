;;; init-syntax.el --- Emacs config syntax checking layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Configure syntax checker (flycheck/flymake)

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
    (use-package flycheck
      :delight flycheck-mode
      :bind (:map flycheck-mode-map
             ("C-c s-e e" . my-hydra/flycheck/body))
      :init (global-flycheck-mode)
      :config (defhydra my-hydra/flycheck (:color amaranth :columns 6)
                "Error"
                ("F" flycheck-error-list-set-filter "filter")
                ("p" flycheck-previous-error "previous")
                ("n" flycheck-next-error "next")
                ("<" flycheck-first-error "first")
                (">" (condition-case nil (while t (flycheck-next-error))
                       (user-error nil)) "last")
                ("l" (condition-case nil (quit-windows-on "*Flycheck errors*" t)
                       (error (flycheck-list-errors))) "list")
                ("q" nil "quit" :exit t)))
  (use-package flymake ;; use "C-h ." to show error at point in minibuffer
    :ensure nil ;; built-in
    :bind (:map flymake-mode-map
           ("C-c s-e e" . my-hydra/flymake/body))
    :config
    (use-package flymake-quickdef) ;; macro for quick Flymake backend defns
    (use-package flymake-diagnostic-at-point
      :hook ((emacs-lisp-mode . flymake-mode)
             (flymake-mode . flymake-diagnostic-at-point-mode))
      :config (setq flymake-diagnostic-at-point-error-prefix "» "))
    ;; Compress Flymake mode-line name
    (defun my-flymake-modeline-filter (ret)
      "Filter function for `flymake--mode-line-format`."
      (setf (seq-elt (car ret) 1) " FlyM")
      ret)
    (advice-add #'flymake--mode-line-format
                :filter-return #'my-flymake-modeline-filter)
    (defun my-toggle-flymake-diagnostics ()
      "Toggles flymake diagnostics window for current buffer."
      (interactive)
      (if flymake-mode
          (let* ((buf-name (buffer-name (current-buffer)))
                 (flymake-winds (condition-case nil
                                    (get-buffer-window-list
                                     (concat "*Flymake diagnostics for " buf-name "*"))
                                  (error nil))))
            (if flymake-winds
                (dolist (wind flymake-winds) (quit-window nil wind))
              (flymake-show-diagnostics-buffer)))))
    (defhydra my-hydra/flymake (:color amaranth)
      "Error"
      ("p" flymake-goto-prev-error "previous")
      ("n" flymake-goto-next-error "next")
      ("l" my-toggle-flymake-diagnostics "list")
      ("q" nil "quit" :exit t))
    (with-eval-after-load 'minions
      (add-to-list 'minions-direct 'flymake-mode))))

(provide 'init-syntax)

;;; init-syntax.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
