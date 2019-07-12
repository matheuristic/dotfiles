;;; init-proj.el --- Emacs config project interaction layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Configure tools for project interaction

;;; Code:

(require 'init-org)
(require 'init-ui-hydra)

;; project interaction library
(use-package projectile
  :delight projectile-mode '(:eval (concat " [" (projectile-project-name) "]"))
  :bind (:map projectile-mode-map
         ("C-c s-p" . projectile-command-map) ;; prefix binding for projectile cmds
         ("C-c s-P" . my-hydra/projectile/body))
  :init
  (setq projectile-create-missing-test-files t ;; create test files if none is found when toggling btw implementation and test
        projectile-switch-project-action 'projectile-commander
        projectile-use-git-grep t) ;; use git grep when in a Git project to skip backup, object and other non-tracked files
  (projectile-mode)
  :config
  ;; use ripgrep to grep in projectile, if available
  (if (executable-find "rg")
      (progn
        (use-package projectile-ripgrep)
        (defalias 'my-projectile-search-fun 'projectile-ripgrep))
    (defalias 'my-projectile-search-fun 'projectile-grep))
  (defhydra my-hydra/projectile (:color teal :hint nil)
    "
Projectile: %(projectile-project-root)

Buffer  _bb_  : switch to buffer          _bi_  : ibuffer
        _bk_  : kill buffers              _bo_  : switch buffer (other window)

File    _ff_  : find file                 _fw_  : find file dwim
        _fd_  : find file in dir          _fp_  : find file in known projects
        _fof_ : find file (other window)  _fow_ : find file dwim (other window)
        _fr_  : recent files

Dir     _dd_  : find dir                  _do_  : find dir (other window)

Search  _sg_  : grep / ripgrep            _so_  : multi-occur
        _rs_  : replace string            _rr_  : replace regexp

Cache   _cc_  : cache current file        _cC_  : clear cache
        _cx_  : remove known project      _cX_  : cleanup known projects

"
    ("bb" projectile-switch-to-buffer)
    ("bi" projectile-ibuffer)
    ("bk" projectile-kill-buffers)
    ("bo" projectile-switch-to-buffer-other-window)
    ("ff" projectile-find-file)
    ("fw" projectile-find-file-dwim)
    ("fd" projectile-find-file-in-directory)
    ("fp" projectile-find-file-in-known-projects)
    ("fof" projectile-find-file-other-window)
    ("fow" projectile-find-file-dwim-other-window)
    ("fr" projectile-recentf)
    ("dd" projectile-find-dir)
    ("do" projectile-find-dir-other-window)
    ("sg" my-projectile-search-fun)
    ("so" projectile-multi-occur)
    ("rs" projectile-replace)
    ("rr" projectile-replace-regexp)
    ("cc" projectile-cache-current-file)
    ("cC" projectile-invalidate-cache)
    ("cx" projectile-remove-known-project)
    ("cX" projectile-cleanup-known-projects)
    ("C" projectile-compile-project "compile")
    ("p" projectile-switch-project "switch project")
    ("q" nil "quit" :exit t)))

;; support org-mode TODOs for projectile projects
;; project TODOs are reflected in the Org agenda
;; use org-capture to store TODOs for the current buffer's project
(use-package org-projectile
  :pin "MELPA"
  :after (org projectile)
  :config
  (setq org-projectile-projects-file "~/org/projects.org"
        org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (push (org-projectile-project-todo-entry) org-capture-templates))

(provide 'init-proj)

;;; init-proj.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
