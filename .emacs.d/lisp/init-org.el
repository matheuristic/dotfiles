;;; init-org.el --- Emacs configuration Org-mode layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up Org-mode

;;; Code:

(require 'init-ui-hydra)

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
  :hook (org-mode . visual-line-mode)
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
        org-hide-leading-stars t
        org-log-into-drawer t
        org-pretty-entities t
        org-return-follows-link t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
                            (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANX(c@/!)"))
        org-use-fast-todo-selection t
        org-use-speed-commands t
        org-startup-indented nil)
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
  (defhydra my-hydra/org-mode (:color amaranth :columns 3)
    "Org-mode"
    ("ns" org-narrow-to-subtree "narrow-subtree")
    ("nb" org-narrow-to-block "narrow-block")
    ("nw" widen "widen")
    ("<tab>" org-global-cycle "cycle-visibility")
    ("i" org-toggle-inline-images "toggle-imgs")
    ("I" org-indent-mode "org-indent-mode")
    ("o" org-occur "org-occur" :exit t)
    ("R" org-refile "org-refile" :exit t)
    ("S" org-sort "sort" :exit t)
    ("q" nil "quit" :exit t))
  (define-key org-agenda-mode-map (kbd "H-m") 'my-hydra/org-agenda/body)
  (define-key org-mode-map (kbd "H-m") 'my-hydra/org-mode/body)
  ;; use variable pitch font in Org-mode for graphical Emacs (looks better)
  (when (display-graphic-p)
    (with-eval-after-load 'init-ui-font
      (require 'org-mouse) ;; Org-mode mouse support
      (add-hook 'org-mode-hook #'variable-pitch-mode) ;; variable-pitch font
      (add-hook 'org-mode-hook (lambda ()
        (setq cursor-type 'bar ;; bar cursor looks better with variable pitch fonts
              line-spacing 0.1)))
      (set-face-attribute 'org-block nil :inherit 'fixed-pitch :background "#FFFFE0")
      (set-face-attribute 'org-block-begin-line nil :inherit 'fixed-pitch :foreground "#555555" :background "#E2E1D5")
      (set-face-attribute 'org-block-end-line nil :inherit 'fixed-pitch :foreground "#555555" :background "#E2E1D5")
      (set-face-attribute 'org-date nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-document-info nil :height 1.2 :slant 'italic)
      (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-document-info-keyword nil :inherit '(shadow fixed-pitch))
      (set-face-attribute 'org-document-title nil :height 1.5)
      (set-face-attribute 'org-link nil :foreground "royal blue" :underline t)
      (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
      (set-face-attribute 'org-property-value nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
      (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-tag nil :inherit '(shadow fixed-pitch) :weight 'bold :height 0.8)
      (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
      ;; properly indent by using fixed-pitch font
      (require 'org-indent) ;; make sure org-indent face is defined
      (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch)))))

;; UTF-8 bullets for org-mode
(use-package org-bullets
  :pin "MELPA"
  :after org
  :hook (org-mode . org-bullets-mode))

(provide 'init-org)

;;; init-org.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
