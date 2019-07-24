;;; init-deft.el --- Emacs deft layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up deft package for viewing, recording and searching notes quickly

;;; Code:

(require 'init-org)

;; assign deft default customization variable values if not already set
(defvar deft-auto-save-interval 0) ;; disable auto-saving buffers opened by deft
(defvar deft-default-extension "org")
(defvar deft-directory "~/org")
(defvar deft-extensions '("org"))
(defvar deft-file-naming-rules '((case-fn . downcase)
                                 (noslash . "-")
                                 (nospace . "-")))
(defvar deft-recursive t)
(defvar deft-use-filename-as-title nil)
(defvar deft-use-filter-string-for-filename t)

(use-package deft
  :commands deft
  :bind ("C-c s-d f" . deft))

(provide 'init-deft)

;;; init-deft.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
