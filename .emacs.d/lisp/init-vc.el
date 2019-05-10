;;; init-vc.el --- Emacs config version control layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up version control tooling

;; Only set up for Git currently

;;; Code:

(when (executable-find "git")
  ;; Git porcelain
  (use-package magit
    :bind ("H-g g" . magit-status)
    :config
    (setq auto-revert-check-vc-info t)
    (with-eval-after-load 'ido-completing-read+
      (setq magit-completing-read-function 'magit-ido-completing-read)))
  
  ;; Browse historic versions of Git-controlled files
  (use-package git-timemachine
    ;; :after magit
    :bind ("H-g t" . git-timemachine)))

(provide 'init-vc)

;;; init-vc.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
