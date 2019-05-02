;;; init-org.el --- Emacs configuration Org-mode layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up Org-mode

;;; Code:

;; Org-mode - built-in
(use-package org
  :init
  (defhydra my-hydra/org-global (:color teal)
    "Org"
    ("a" org-agenda "agenda")
    ("c" org-capture "capture")
    ("b" org-switchb "switch buffer")
    ("l" org-store-link "store link")
    ("q" nil "quit"))
  (global-set-key (kbd "H-o") 'my-hydra/org-global/body)
  :config
  (require 'org-agenda)
  (setq org-agenda-start-on-weekday nil
        org-catch-invisible-edits 'error
        org-confirm-babel-evaluate t
        org-edit-src-content-indentation 2
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-hide-emphasis-markers t
        org-log-into-drawer t
        org-pretty-entities t
        org-return-follows-link t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
                            (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANX(c@/!)"))
        org-use-fast-todo-selection t
        org-use-speed-commands t
        org-startup-indented t)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (defhydra my-hydra/org-agenda (:color amaranth :hint nil)
    "
Org agenda

Headline    _ht_  : set status   _hk_  : kill         _hr_  : refile
            _hA_  : archive      _h:_  : set tags     _hp_  : set priority

Visit Entry _SPC_ : other window _TAB_ : & go to loc  _RET_ : & del other wins
            _o_   : link

Date        _ds_  : schedule     _dd_  : set deadline _dt_  : timestamp

View        _vd_  : day          _vw_  : week         _vm_  : month
            _vn_  : next span    _vp_  : prev span    _vr_  : reset

Filter      _ft_  : by tag       _fc_  : by category  _fh_  : by top headline
            _fx_  : by regex     _fd_  : reset

Clock       _ci_  : in           _co_  : out          _cq_  : cancel
            _cg_  : goto

Other       _gr_  : reload       _gd_  : go to date   _._   : go to today

"
    ("ht" org-agenda-todo)
    ("hk" org-agenda-kill)
    ("hr" org-agenda-refile)
    ("hA" org-agenda-archive-default)
    ("h:" org-agenda-set-tags)
    ("hp" org-agenda-priority)
    ("SPC" org-agenda-show-and-scroll-up)
    ("TAB" org-agenda-goto :exit t)
    ("RET" org-agenda-switch-to :color blue)
    ("o" link-hint-open-link :exit t)
    ("ds" org-agenda-schedule)
    ("dd" org-agenda-deadline)
    ("dt" org-agenda-date-prompt)
    ("vd" org-agenda-day-view)
    ("vw" org-agenda-week-view)
    ("vm" org-agenda-month-view)
    ("vn" org-agenda-later)
    ("vp" org-agenda-earlier)
    ("vr" org-agenda-reset-view)
    ("ft" org-agenda-filter-by-tag)
    ("fc" org-agenda-filter-by-category)
    ("fh" org-agenda-filter-by-top-headline)
    ("fx" org-agenda-filter-by-regexp)
    ("fd" org-agenda-filter-remove-all)
    ("ci" org-agenda-clock-in :exit t)
    ("co" org-agenda-clock-out)
    ("cq" org-agenda-clock-cancel)
    ("cg" org-agenda-clock-goto :exit t)
    ("gr" org-agenda-redo)
    ("gd" org-agenda-goto-date)
    ("." org-agenda-goto-today)
    ("q" nil "quit" :exit t))
  (define-key org-agenda-mode-map (kbd "H-m") 'my-hydra/org-agenda/body))

(use-package org-bullets
  :pin "MELPA"
  :after org
  :config
  ;; (setq org-bullets-bullet-list '("◉" "○" "✸" "✿" "◆" "◇"))
  (add-hook 'org-mode-hook 'org-bullets-mode))

;; use variable pitch font in Org-mode for graphical Emacs
(when (display-graphic-p)
  (with-eval-after-load 'init-ui-font
    (add-hook 'org-mode-hook #'variable-pitch-mode)
    (set-face-attribute 'org-document-title nil
                        :height 1.5)
    (set-face-attribute 'org-document-info nil
                        :height 1.2
                        :slant 'italic)
    (set-face-attribute 'org-document-info-keyword nil
                        :height 0.8)
    (mapc (lambda (face)
            (set-face-attribute face nil :inherit 'fixed-pitch))
          (list 'org-block
                'org-block-begin-line
                'org-block-end-line
                'org-code
                'org-link
                'org-table
                'org-verbatim
                'org-meta-line))))

(provide 'init-org)

;;; init-org.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
