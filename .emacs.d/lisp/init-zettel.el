;;; init-zettel.el --- Emacs Zettelkasten layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Configure Zettelkasten workflow, see https://zettelkasten.de/

;;; Code:

(require 'init-org)

;; set deft customization variables to default values if not yet set
(defvar deft-auto-save-interval 0) ;; disable auto-saving deft buffers
(defvar deft-default-extension "org")
(defvar deft-directory (concat (or org-directory (file-truename "~/org"))
                               "/zetteldeft"))
(defvar deft-extensions '("org"))
(defvar deft-recursive t)

;; Emacs mode for quick browsing, filtering and editing dirs of text files
(use-package deft
  :defer t)

;; Zettelkasten system built on deft, https://github.com/EFLS/zetteldeft
(use-package zetteldeft
  :pin "MELPA"
  :defer t
  :bind ("C-c s-d f" . (lambda () (interactive)
                         (require 'zetteldeft)
                         (my-hydra/zetteldeft/body)))
  :config
  (with-eval-after-load 'hydra
    (defhydra my-hydra/zetteldeft (:color teal :columns 3)
      "zetteldeft"
      ("d" deft "deft")
      ("D" zetteldeft-deft-new-search "deft-new-search")
      ("R" deft-refresh "deft-refresh")
      ("s" zetteldeft-search-at-point "search-at-pt")
      ("c" zetteldeft-search-current-id "search-cur-id")
      ("f" zetteldeft-follow-link "follow-link")
      ("F" zetteldeft-avy-file-search-ace-window "file-search")
      ("l" zetteldeft-avy-link-search "link-search")
      ("t" zetteldeft-avy-tag-search "tag-search")
      ("T" zetteldeft-tag-buffer "tag-buffer")
      ("i" zetteldeft-find-file-id-insert "find-file-id-insert")
      ("I" zetteldeft-find-file-full-title-insert "find-file-title-insert")
      ("o" zetteldeft-find-file "find-file")
      ("n" zetteldeft-new-file "new-file")
      ("N" zetteldeft-new-file-and-link "new-file-and-link")
      ("r" zetteldeft-file-rename "file-rename")
      ("x" zetteldeft-count-words "count-words")
      ("q" nil "quit"))))

(provide 'init-zettel)

;;; init-zettel.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:

