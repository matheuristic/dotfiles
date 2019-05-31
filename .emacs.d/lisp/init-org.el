;;; init-org.el --- Emacs configuration Org-mode layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up Org-mode

;;; Code:

(require 'init-ui-hydra)

(defvar org-agenda-files '("~/org/inbox.org"
                           "~/org/gtd.org"
                           "~/org/tickler.org"))
(defvar org-capture-templates '(("t" "Todo [inbox]" entry (file "~/org/inbox.org") "* TODO %i%?")
                                ("T" "Tickler" entry (file "~/org/tickler.org") "* %i%? \n %U")))
(defvar org-refile-targets '((nil . (:maxlevel . 9)) ;; current buffer
                             (org-agenda-files . (:maxlevel . 3)))) ;; files for agenda display
(defvar org-refile-use-outline-path 'file) ;; allows refile to top level
(defvar org-tag-alist '((:startgroup) ;; tags within the same group are mutually exclusive
                        ("@home" . ?H) ("@work" . ?W)
                        (:endgroup)
                        (:startgroup)
                        ("easy" . ?e) ("medium" . ?m) ("hard" . ?h)
                        (:endgroup)
                        ("urgent" . ?u)))

;; Org-mode
;;
;; see http://doc.norang.ca/org-mode.html for a good example config
;;
;; escaping Org examples: using "C-c '" to edit Org source blocks in a separate
;; buffer will automatically escape the Org markup on return
(use-package org
  :hook (org-mode . visual-line-mode)
  :bind (("H-o" . my-hydra/org-global/body)
         :map org-agenda-mode-map
         ("H-m" . my-hydra/org-agenda/body)
         :map org-mode-map
         ("H-m" . my-hydra/org-mode/body))
  :init (defhydra my-hydra/org-global (:color teal)
          "Org"
          ("a" org-agenda "agenda")
          ("c" org-capture "capture")
          ("b" org-switchb "switch buffer")
          ("l" org-store-link "store link")
          ("q" nil "quit"))
  :config
  (require 'org-agenda)
  (setq org-agenda-start-on-weekday nil
        org-catch-invisible-edits 'error
        org-confirm-babel-evaluate nil ;; don't confirm before evaluating code blocks in Org documents
        org-edit-src-content-indentation 2
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-highlight-latex-and-related '(latex script entities) ;; highlight LaTeX fragments with the `org-highlight-latex-and-related' face
        org-log-into-drawer t
        org-outline-path-complete-in-steps nil
        org-pretty-entities t
        org-refile-allow-creating-parent-nodes 'confirm
        org-return-follows-link t
        org-src-fontify-natively t
        org-src-preserve-indentation t ;; preserve src code block indentation on export and when switching btw org buffer and edit buffer
        org-src-strip-leading-and-trailing-blank-lines t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window ;; reuse Org file window for editing source blocks when using "C-c '"
        ;; Diagram of possible state transitions for a given task
        ;;     -------------------------
        ;;     |                       |
        ;;     |                       v
        ;; -> TODO....... -> NEXT -> DONE ----->
        ;;    | ^  |  | ^    | ^      ^     |
        ;;    v |  |  v |    v |      |     |
        ;;   HOLD  |  WAIT...... ------     |
        ;;     |   |  | (note records what  |
        ;;     v   v  v  it is waiting for) |
        ;;     CANX.... ---------------------
        ;;     (note records why it was cancelled)
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
                            (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANX(c@/!)"))
        org-treat-S-cursor-todo-selection-as-state-change nil
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
            _sd_  : hide done

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
    ("sd" (lambda () (interactive)
            (progn (setq org-agenda-skip-scheduled-if-done
                         (if org-agenda-skip-scheduled-if-done nil t))
                   (org-agenda-redo-all t))))
    ("q" nil "quit" :exit t))
  (defhydra my-hydra/org-mode (:color amaranth :columns 3)
    "Org-mode"
    ("ns" org-narrow-to-subtree "narrow-subtree")
    ("nb" org-narrow-to-block "narrow-block")
    ("nw" widen "widen")
    ("<tab>" org-global-cycle "cycle-visibility")
    ("i" org-toggle-inline-images "toggle-imgs")
    ("I" org-indent-mode "org-indent-mode")
    ("s" org-sort "sort" :exit t)
    ("o" org-occur "org-occur" :exit t)
    ("r" org-refile "org-refile" :exit t)
    ("t" org-todo "org-todo" :exit t)
    ("a" org-archive-subtree-default :exit t)
    ("q" nil "quit" :exit t))
  ;; use variable pitch font in Org-mode for graphical Emacs, which looks better
  (when (display-graphic-p)
    (with-eval-after-load 'init-ui-font
      (require 'org-mouse) ;; Org-mode mouse support
      (add-hook 'org-mode-hook #'variable-pitch-mode) ;; variable-pitch font
      (add-hook 'org-mode-hook (lambda () (setq line-spacing 0.1)))
      (set-face-attribute 'org-block nil :inherit 'fixed-pitch :background "#FFFFE0")
      (set-face-attribute 'org-block-begin-line nil :inherit 'fixed-pitch :foreground "#555555" :background "#E2E1D5")
      (set-face-attribute 'org-block-end-line nil :inherit 'fixed-pitch :foreground "#555555" :background "#E2E1D5")
      (set-face-attribute 'org-date nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-document-info nil :height 1.2 :slant 'italic)
      (set-face-attribute 'org-done nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-document-info-keyword nil :inherit '(shadow fixed-pitch))
      (set-face-attribute 'org-document-title nil :height 1.5)
      (set-face-attribute 'org-latex-and-related nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-link nil :foreground "royal blue" :underline t)
      (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
      (set-face-attribute 'org-property-value nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
      (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-tag nil :inherit '(shadow fixed-pitch) :weight 'bold :height 0.8)
      (set-face-attribute 'org-todo nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
      ;; properly indent by using fixed-pitch font
      (require 'org-indent) ;; make sure org-indent face is defined
      (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch)))))

;; UTF-8 bullets for org-mode
(use-package org-bullets
  :pin "MELPA"
  :after org
  :hook (org-mode . org-bullets-mode))

;; Gantt charts via LaTeX
;;
;; assumes org-gantt package is present on the system; to install the package,
;; clone the repo https://github.com/swillner/org-gantt in ~/.emacs.d/site-lisp
;;
;; to use, in a Org document create an org-gantt-chart dynamic block, which
;; can be limited to a given Org subtree using the :ID: property, populate it
;; using "C-c C-x C-u" and export the Org document to LaTeX as a PDF file
;;
;; the subtree tasks should have either
;; 1. scheduled ("C-c C-s") and deadline ("C-c C-d") dates, or
;; 2. an :Effort: property
;;
;; task downstream dependencies are specified by a :LINKED-TO: <id_list>
;; property to link to tasks with the given :ID: property, or by using the
;; order of the child tasks if a task has an :ORDERED: property with value t
;;
;; to hide the subtree generating the Gantt chart in the LaTeX output, give the
;; subtree a COMMENT state and make sure the dynamic block appears outside the
;; commented subtree (in fact, outside any commented subtree)
;;
;; Example:
;; ---
;; ...
;; #+LATEX_HEADER: \usepackage{pgfgantt}
;; #+LATEX_HEADER: \usepackage{pdflscape}
;; ...
;; * Task Specification
;; Description of Project.
;; Since the subtree below has the ~COMMENT~ state, it is not exported.
;; To also export it, remove the ~COMMENT~ state before exporting.
;; ** COMMENT Project
;;    :PROPERTIES:
;;    :ID:       project
;;    :END:
;; *** Task 1
;;     SCHEDULED: <2015-05-25 Mon> DEADLINE: <2015-05-28 Thu>
;;     :PROPERTIES:
;;     :LINKED-TO: task2,task4
;;     :END:
;; *** Task 2
;;     :PROPERTIES:
;;     :ID:       task2
;;     :Effort:   2d
;;     :END:
;; *** Task 3
;;     :PROPERTIES:
;;     :ID:       task3
;;     :ORDERED:  t
;;     :Effort:   7d
;;     :END:
;; **** Task 3.1
;;      :PROPERTIES:
;;      :Effort:   3d
;;      :END:
;; **** Task 3.2
;;      :PROPERTIES:
;;      :Effort:   4d
;;      :END:
;; *** Task 4
;;     :PROPERTIES:
;;     :ID:       task4
;;     :Effort:   5d
;;     :LINKED-TO: task3
;;     :END:
;;
;; * Gantt chart
;; Press "~C-c C-x C-u~" to populate the dynamic block below before exporting
;; to LaTeX. The Gantt chart below is wrapped in the ~landscape~ environment
;; to make it appear on its own landscape page.
;; #+LATEX: \begin{landscape}
;; #+BEGIN: org-gantt-chart :id "project-deadlines-schedules"
;; #+END
;; #+LATEX: \end{landscape}
;; ...
;; ---
(use-package org-gantt
  :load-path "site-lisp/org-gantt"
  :ensure nil
  :after org)

;; export Org documents to reveal.js presentations
;; https://gitlab.com/oer/org-re-reveal
(use-package org-re-reveal
  :pin "MELPA"
  :after org
  :init (setq org-re-reveal-note-key-char nil
              org-re-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.8.0/"))

;; export Org documents to Markdown
(use-package ox-md
  :ensure nil
  :after org) ;; built-in to Org

(provide 'init-org)

;;; init-org.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
