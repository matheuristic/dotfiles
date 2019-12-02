;;; init-dired.el --- Emacs configuration Dired layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up Dired

;;; Code:

(require 'init-ui-hydra)

;; ls-lisp
(use-package ls-lisp
  :ensure nil ;; built-in
  :config (setq ls-lisp-use-insert-directory-program nil ;; don't use system ls
                ls-lisp-dirs-first t)) ;; list directories first

;; Dired
(use-package dired
  :ensure nil ;; built-in
  :commands dired
  :bind (:map dired-mode-map
         ("C-c s-m" . my-hydra/dired/body))
  :config
  (require 'dired-x)
  (require 'dired-aux)
  (setq dired-dwim-target t ;; use neighboring dired buffer as default target dir
        dired-listing-switches "-alhvFG" ;; more readable file listings
        dired-omit-files (concat dired-omit-files "\\|^\\..+$") ;; omit dot files in dired-omit-mode
        dired-recursive-copies 'always ;; always copy recursively
        dired-recursive-deletes 'always) ;; always delete recursively
  (add-hook 'dired-mode-hook 'auto-revert-mode) ;; auto-refresh on file change
  ;; adapted from https://www.reddit.com/r/emacs/comments/jh1me/keeping_large_dired_buffers_tidy/
  (defun my-dired-kill-and-next-subdir ()
    "Kill current subdir in dired, and jump back to its parent dir."
    (interactive)
    (let* ((subdir-name (directory-file-name (dired-current-directory)))
           (parent-dir  (file-name-directory subdir-name))
           (search-term (concat " "
                                (file-name-base subdir-name)
                                (file-name-extension subdir-name t))))
      (dired-kill-subdir)
      (dired-goto-subdir parent-dir)
      (search-forward search-term)))
  (use-package dired-filter
    :bind (:map dired-mode-map
           ("/" . dired-filter-map))
    :hook (dired-mode . dired-filter-mode)
    :init (setq-default dired-filter-stack nil))
  (defhydra my-hydra/dired (:color pink :columns 4)
    "Dired"
    ("RET" (progn (dired-find-file) (when (eq major-mode 'dired-mode) (my-hydra/dired/body))) "open" :exit t)
    ("{" find-name-dired "find-name" :exit t)
    ("}" find-grep-dired "find-grep" :exit t)
    ("/" my-hydra/dired-filter/body "→ filter" :exit t)
    ("(" dired-hide-details-mode "toggle-details")
    (")" dired-omit-mode "toggle-omit")
    ("+" dired-create-directory "mkdir")
    ("=" dired-diff "diff" :exit t)
    ("_" dired-show-file-type "show-file-type")
    ("?" dired-summary "help")
    ("A" dired-do-find-regexp "find-regex" :exit t)
    ("C" dired-do-copy "copy")
    ("c" dired-do-compress-to "compress-to")
    ("D" dired-do-delete "delete")
    ("E" dired-mark-extension "mark-ext")
    ("F" dired-do-find-marked-files "find-marked" :exit t)
    ("G" dired-do-chgrp "chgrp")
    ("g" revert-buffer "refresh")
    ("i" dired-maybe-insert-subdir "insert-subdir")
    ("K" my-dired-kill-and-next-subdir "kill-subdir")
    ("l" dired-do-redisplay "redisplay")
    ("M" dired-do-chmod "chmod")
    ("m" dired-mark "mark")
    ("O" dired-display-file "display")
    ("o" dired-find-file-other-window "find-file-o" :exit t)
    ("Q" dired-do-find-regexp-and-replace "find-regex-sub" :exit t)
    ("R" dired-do-rename "rename")
    ("S" dired-do-symlink "symlink")
    ("s" dired-sort-toggle-or-edit "date-sort")
    ("T" dired-do-touch "touch")
    ("t" dired-toggle-marks "toggle-marks")
    ("U" dired-unmark-all-marks "unmark-all")
    ("u" dired-unmark "unmark")
    ("v" dired-view-file "view-file" :exit t) ;; open file in view-mode
    ("Y" dired-do-relsymlink "symlink-to-dir")
    ("Z" dired-do-compress "compress")
    ("q" nil "quit" :exit t))
  (defhydra my-hydra/dired-filter (:color pink :columns 4)
    "Dired → filter"
    ("n" dired-filter-by-name "by-name")
    ("r" dired-filter-by-regex "by-regex")
    ("." dired-filter-by-extension "by-ext")
    ("h" dired-filter-by-dot-files "by-hidden")
    ("o" dired-filter-by-omit "by-omit")
    ("g" dired-filter-by-garbage "by-garbage")
    ("e" dired-filter-by-predicate "by-pred")
    ("f" dired-filter-by-file "by-file")
    ("d" dired-filter-by-directory "by-dir")
    ("m" dired-filter-by-mode "by-mode")
    ("s" dired-filter-by-symlink "by-symlink")
    ("x" dired-filter-by-executable "by-exe")
    ("ig" dired-filter-by-git-ignored "by-git-ign")
    ("|" dired-filter-or "or")
    ("!" dired-filter-negate "negate")
    ("*" dired-filter-decompose "decompose")
    ("TAB" dired-filter-transpose "transpose")
    ("p" dired-filter-pop "pop")
    ("/" dired-filter-pop-all "reset")
    ("S" dired-filter-save-filters "save")
    ("D" dired-filter-delete-saved-filters "del")
    ("A" dired-filter-add-saved-filters "add")
    ("L" dired-filter-load-saved-filters "load")
    ("q" nil "quit" :exit t)))

(provide 'init-dired)

;;; init-dired.el ends here
