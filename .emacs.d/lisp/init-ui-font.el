;;; init-ui-font.el --- Emacs config font layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up typography

;;; Code:

(defgroup init-ui-font-el nil
  "Font settings."
  :group 'convenience)

(defcustom init-ui-font-default-list '("Consolas"
                                       "Menlo"
                                       "DejaVu Sans Mono")
  "List of fonts to use for the default face."
  :type '(repeat string)
  :group 'init-ui-font-el)

(defcustom init-ui-font-variable-pitch-list '("Constantia"
                                              "Hoefler Text"
                                              "DejaVu Serif")
  "List of fonts to use for the default face."
  :type '(repeat string)
  :group 'init-ui-font-el)

;; Typography, GUI-only
(when (display-graphic-p)
  ;; helper functions
  (require 'cl-extra)
  
  (defun my-font-exists (font-name)
    "Returns FONT-NAME if that font exists on the system and `nil` otherwise"
    (if (x-list-fonts font-name) font-name))
  
  (defun my-set-font (face family &optional height weight width)
    "Sets font for FACE to FAMILY at the given HEIGHT, WEIGHT and WIDTH"
    (set-face-attribute face nil
                        :family family
                        :height (or height 110)
                        :weight (or weight 'normal)
                        :width (or width 'normal)))

  ;; set default font priority
  (let* ((my-font-priority-list init-ui-font-default-list)
         (my-font (cl-some #'my-font-exists my-font-priority-list))
         (is-darwin (eq system-type 'darwin)))
    (when my-font
      (my-set-font 'default my-font (if is-darwin 150 110) nil nil)
      (my-set-font 'mode-line my-font (if is-darwin 120 90) nil nil)
      (my-set-font 'mode-line-inactive my-font (if is-darwin 120 90) nil nil)))

  ;; set variable pitch font
  (let* ((my-font-priority-list init-ui-font-variable-pitch-list)
         (my-font (cl-some #'my-font-exists my-font-priority-list))
         (is-darwin (eq system-type 'darwin)))
    (when my-font
      (my-set-font 'variable-pitch my-font (if is-darwin 150 110) nil nil)))

  ;; enable ligatures, only works on Emacs Mac Port by Mitsuharu
  (if (fboundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode))
)

(provide 'init-ui-font)

;;; init-ui-font.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
