;;; init-local-post.el --- Sample Emacs local post-initialization config file -*- lexical-binding: t; -*-

;; Author: matheuristic
;; URL: https://github.com/matheuristic/dotfiles

;;; Commentary:

;; This file contains optional packages that are language-specific or UI
;; elements for specific machines (e.g. Macs with no physical ESC key)
;; Symlink or copy this file to ~/.emacs.d/init-local-post.el

;;; Code:

;;;;
;; USER INTERFACE
;;;;

;; return to normal mode in evil with custom key seq - MELPA Stable
(use-package evil-escape
  :after evil
  :diminish evil-escape-mode
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "jk"))

;; powerline - MELPA Stable
(use-package powerline
  :config
  (setq powerline-default-separator 'utf-8)
  (powerline-center-evil-theme))

;; virtualenv activation in Emacs - MELPA Stable
(use-package pyvenv
  :init
  (setenv "WORKON_HOME" "~/miniconda3/envs")
  (pyvenv-mode 1)
  (pyvenv-tracking-mode 1))

;;;;
;; LANGUAGE-SPECIFIC
;;;;

;; CSV - GNU ELPA
(use-package csv-mode
  :commands csv-mode
  :config
  (defhydra my-hydra/csv-mode (:color teal :columns 4)
    "CSV mode"
    ("s" csv-sort-fields "sort")
    ("r" csv-sort-numeric-fields "numsort")
    ("k" csv-kill-fields "cut")
    ("y" csv-yank-fields "copy")
    ("a" csv-align-fields "align")
    ("u" csv-unalign-fields "unalign")
    ("t" csv-transpose "transpose")
    ("q" nil "quit" :color blue))
  (define-key csv-mode-map (kbd "C-c h M") 'my-hydra/csv-mode/body))

;; Dockerfile - MELPA Stable
(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; Go - MELPA Stable (all packages)
(when (executable-find "go")
  (use-package go-mode
    :commands go-mode
    :init
    (add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
    (if (executable-find "goimports")
        (setq gofmt-command "goimports")))
  (use-package company-go
    :after go-mode
    :config
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-go)))
  ;; Go guru, commands have prefix C-c C-o
  (if (executable-find "guru")
      (use-package go-guru
        :after go-mode
        :init
        (with-eval-after-load 'go-mode
          (add-hook 'go-mode-hook 'go-guru-hl-identifier-mode)))))

;; Python - MELPA Stable (all packages)
(when (executable-find "python")
  (use-package anaconda-mode
    ;; requires python jedi be installed
    :commands anaconda-mode
    :diminish anaconda-mode
    :init
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))
  (use-package company-anaconda
    :after anaconda-mode
    :config
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-anaconda)))
  ;; Jupyter notebook client
  ;; Add (setq my-load-ein t) to init-local-pre.el to enable
  (if (and (bound-and-true-p my-load-ein)
           (executable-find "jupyter"))
      (use-package ein
        :commands ein:notebooklist-open
        :config
        ;; IPython fancy prompts don't work in Eshell
        (setq ein:console-args '("--simple-prompt"))
        (with-eval-after-load 'anaconda-mode
          (add-to-list 'python-shell-completion-native-disabled-interpreters
                       "jupyter"))
        (with-eval-after-load 'hydra
          (defhydra my-hydra/ein (:color amaranth :hint nil)
            "
Emacs IPython Notebook mode

Cell       _j_/_k_       : next/prev        _J_/_K_       : move down/up
           _m_         : merge with prev  _o_/_O_       : insert below/above
           _y_/_p_/_d_     : copy/paste/del   _s_         : split at point
           _u_         : change type      _'_         : edit contents
           _RET_       : run              _M-RET_     : run in-place

Worksheet  _h_/_l_       : prev/next        _H_/_L_       : move prev/next
           _1_.._9_      : first..last      _+_/_-_       : new/delete

Notebook   _C-s_/_C-w_   : save/rename      _C-#_       : close

Other      _t_         : toggle output    _C-l_/_C-L_   : clear cell/all output
           _C-x_       : show traceback   _C-r_/_C-z_   : restart/stop kernel
           _C-/_       : open scratch     _C-o_       : open console

"
            ("j" ein:worksheet-goto-next-input)
            ("k" ein:worksheet-goto-prev-input)
            ("J" ein:worksheet-move-cell-down)
            ("K" ein:worksheet-move-cell-up)
            ("m" ein:worksheet-merge-cell)
            ("o" ein:worksheet-insert-cell-below)
            ("O" ein:worksheet-insert-cell-above)
            ("y" ein:worksheet-copy-cell)
            ("p" ein:worksheet-yank-cell)
            ("d" ein:worksheet-kill-cell)
            ("s" ein:worksheet-split-cell-at-point)
            ("u" ein:worksheet-change-cell-type)
            ("'" ein:edit-cell-contents :color blue)
            ("RET" ein:worksheet-execute-cell-and-goto-next)
            ("M-RET" ein:worksheet-execute-cell)
            ("h" ein:notebook-worksheet-open-prev-or-last)
            ("l" ein:notebook-worksheet-open-next-or-first)
            ("H" ein:notebook-worksheet-move-prev)
            ("L" ein:notebook-worksheet-move-next)
            ("1" ein:notebook-worksheet-open-1th)
            ("2" ein:notebook-worksheet-open-2th)
            ("3" ein:notebook-worksheet-open-3th)
            ("4" ein:notebook-worksheet-open-4th)
            ("5" ein:notebook-worksheet-open-5th)
            ("6" ein:notebook-worksheet-open-6th)
            ("7" ein:notebook-worksheet-open-7th)
            ("8" ein:notebook-worksheet-open-8th)
            ("9" ein:notebook-worksheet-open-last)
            ("+" ein:notebook-worksheet-insert-next)
            ("-" ein:notebook-worksheet-delete)
            ("C-s" ein:notebook-save-notebook-command :color blue)
            ("C-w" ein:notebook-rename-command :color blue)
            ("C-#" ein:notebook-close :color blue)
            ("t" ein:worksheet-toggle-output)
            ("C-l" ein:worksheet-clear-output)
            ("C-L" ein:worksheet-clear-all-output)
            ("C-x" ein:tb-show)
            ("C-r" ein:notebook-restart-kernel-command)
            ("C-z" ein:notebook-kernel-interrupt-command)
            ("C-/" ein:notebook-scratchsheet-open :color blue)
            ("C-o" ein:console-open :color blue)
            ("q" nil "quit" :color blue))
          (with-eval-after-load 'ein-notebooklist
            (define-key ein:notebook-mode-map (kbd "C-c h M")
              'my-hydra/ein/body))))))

;; YAML - MELPA Stable
(use-package yaml-mode
  :commands yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

(provide 'init-local-post)
;;; init-local-post.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
