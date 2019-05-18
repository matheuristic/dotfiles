;;; init-vc.el --- Emacs config version control layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up version control tooling
;; * Git

;;; Code:

(require 'init-ui-hydra)

(use-package smerge-mode
  :ensure nil
  :hook (find-file . (lambda ()
                       (save-excursion
                         (goto-char (point-min))
                         (when (re-search-forward "^<<<<<<< " nil t)
                           (smerge-mode 1)))))
  :config
  (defhydra my-hydra/smerge (:color pink :hint nil :post (smerge-auto-leave))
    "
Smerge

Move   _n_   : next          _p_ : prev

Keep   _b_   : base          _u_   : upper         _l_   : lower
       _a_   : all           _RET_ : current

Diff   _<_   : upper/base    _=_   : upper/lower   _>_   : base/lower
       _R_   : refine        _E_   : ediff

Other  _C_   : combine       _r_   : resolve       _k_   : kill current

"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("q" nil "quit" :color blue))
  (define-key smerge-mode-map (kbd "H-m") 'my-hydra/smerge/body))

(when (executable-find "git")
  ;; Git porcelain
  (use-package magit
    :after smerge-mode
    :bind ("H-g s" . magit-status)
    :hook (magit-diff-visit-file . (lambda ()
                                     (when smerge-mode (my-hydra/smerge/body))))
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
