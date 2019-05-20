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
  (defhydra my-hydra/dired (:color pink :columns 4)
    "Dired"
    ("(" dired-hide-details-mode "toggle-details")
    (")" dired-omit-mode "toggle-omit")
    ("+" dired-create-directory "mkdir")
    ("=" dired-diff "diff" :exit t)
    ("_" dired-show-file-type "show-file-type")
    ("?" dired-summary "help")
    ("A" dired-do-find-regexp "find-regex")
    ("C" dired-do-copy "copy")
    ("c" dired-do-compress-to "compress-to")
    ("D" dired-do-delete "delete")
    ("E" dired-mark-extension "mark-ext")
    ("F" dired-do-find-marked-files "find-marked")
    ("G" dired-do-chgrp "chgrp")
    ("g" revert-buffer "refresh")
    ("i" dired-maybe-insert-subdir "insert-subdir")
    ("K" my-dired-kill-and-next-subdir "kill-subdir")
    ("l" dired-do-redisplay "redisplay")
    ("M" dired-do-chmod "chmod")
    ("m" dired-mark "mark")
    ("O" dired-display-file "display")
    ("o" dired-find-file-other-window "find-file-o")
    ("Q" dired-do-find-regexp-and-replace "find-regex-sub")
    ("R" dired-do-rename "rename")
    ("S" dired-do-symlink "symlink")
    ("s" dired-sort-toggle-or-edit "date-sort")
    ("T" dired-do-touch "touch")
    ("t" dired-toggle-marks "toggle-marks")
    ("U" dired-unmark-all-marks "unmark-all")
    ("u" dired-unmark "unmark")
    ("v" dired-view-file "view-file" :exit t) ;; q -> exit, s -> search, = -> get linum
    ("Y" dired-do-relsymlink "symlink-to-dir")
    ("Z" dired-do-compress "compress")
    ("q" nil "quit" :exit t))
  (define-key dired-mode-map (kbd "H-m") 'my-hydra/dired/body))

(provide 'init-dired)

;;; init-dired.el ends here
