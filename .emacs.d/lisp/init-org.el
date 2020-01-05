;;; init-org.el --- Emacs configuration Org-mode layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up Org-mode

;;; Code:

(require 'init-ui-hydra)

(defvar org-directory (file-name-as-directory (file-truename "~/org")))

(defvar my-org-agenda-inbox (concat org-directory "inbox.org"))
(defvar org-agenda-files (cons my-org-agenda-inbox
                               (mapcar '(lambda (x) (concat org-directory x))
                                       '("gtd.org"
                                         "tickler.org"))))
(defvar org-capture-templates '(("t" "Todo" entry (file my-org-agenda-inbox)
                                 "* TODO %i%?\n%U")
                                ("r" "Respond" entry (file my-org-agenda-inbox)
                                 "* NEXT Respond to %i%?\n%U")
                                ("i" "Interrupt Task" entry (file my-org-agenda-inbox)
                                 "* NEXT %i%?\n%U"
                                 :jump-to-captured t :clock-in t :clock-resume t)
                                ("n" "Note" entry (file my-org-agenda-inbox)
                                 "* %i%? :note:\n%U")
                                ("s" "Someday" entry (file my-org-agenda-inbox)
                                 "* %i%? :someday:\n%U")
                                ("l" "Link" entry (file my-org-agenda-inbox)
                                 "* %a%?\n%U")
                                ("y" "Paste" entry (file my-org-agenda-inbox)
                                 "* %?\n%U\n%c")))
(defvar org-refile-targets '((nil . (:maxlevel . 9)) ;; current buffer
                             (org-agenda-files . (:maxlevel . 3)))) ;; files for agenda display
(defvar org-refile-use-outline-path 'file) ;; allows refile to top level
(defvar org-tag-alist '((:startgroup) ;; tags in the same group are mutually exclusive
                        ("@home" . ?H) ("@work" . ?W)
                        (:endgroup)
                        (:startgroup)
                        ("easy" . ?e) ("medium" . ?m) ("hard" . ?h)
                        (:endgroup)
                        ("note" . ?n)
                        ("someday" . ?s)
                        ("urgent" . ?u)))
(defvar org-journal-dir (concat org-directory "journal/")) ;; default directory for org journals

;; Org-mode
;;
;; see http://doc.norang.ca/org-mode.html for a good example config
;;
;; using "C-c '" to edit Org source blocks in a separate buffer will
;; automatically escape the Org markup on return
(use-package org
  :hook (org-mode . visual-line-mode)
  :bind (("C-c s-o" . my-hydra/org-global/body)
         :map org-agenda-mode-map
         ("C-c s-m" . my-hydra/org-agenda/body)
         :map org-mode-map
         ("C-c s-m" . my-hydra/org-mode/body))
  :init (defhydra my-hydra/org-global (:color teal)
          "Org"
          ("a" org-agenda "agenda")
          ("c" org-capture "capture")
          ("b" org-switchb "switch buffer")
          ("l" org-store-link "store link")
          ("q" nil "quit"))
  :config
  (require 'org-agenda)
  (setq org-adapt-indentation nil ;; don't auto indent node text when promoting or demoting nodes
        org-agenda-restore-windows-after-quit t
        org-agenda-start-on-weekday nil
        org-agenda-window-setup 'only-window ;; full-frame Agenda view
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
        org-startup-indented nil
        ;; Diagram of possible task state transitions
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
        org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "|" "DONE(d!)")
                            (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANX(c@/!)"))
        org-treat-S-cursor-todo-selection-as-state-change nil
        org-use-fast-todo-selection t
        org-use-speed-commands t)
  ;; add custom agenda commands that only show undated tasks in list view
  (dolist (my-custom-cmd
           '(("N" "Three-day agenda and undated TODO entries"
               ((agenda "" ((org-agenda-span 3)))
                (alltodo "" ((org-agenda-todo-ignore-with-date t)
                             (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up category-keep alpha-up))))))
             ("u" "Undated TODO entries"
              (alltodo "" ((org-agenda-todo-ignore-with-date t)
                           (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up category-keep alpha-up)))))))
    (add-to-list 'org-agenda-custom-commands my-custom-cmd))
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
    ("RET" org-agenda-switch-to :exit t)
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
    ("M-s" org-narrow-to-subtree "narrow-subtree")
    ("M-b" org-narrow-to-block "narrow-block")
    ("M-w" widen "widen")
    ("i" org-toggle-inline-images "toggle-images")
    ("I" org-indent-mode "toggle-indent")
    ("P" org-toggle-pretty-entities "toggle-prettify")
    ("<tab>" org-cycle "cycle")
    ("<S-tab>" org-global-cycle "global-cycle")
    ("/" org-sparse-tree "sparse-tree")
    ("c" org-remove-occur-highlights "occur-clear")
    ("p" (lambda (n) (interactive "p") (if org-occur-highlights (previous-error n) (org-previous-visible-heading n))) "previous")
    ("n" (lambda (n) (interactive "p") (if org-occur-highlights (next-error n) (org-next-visible-heading n))) "next")
    ("g" org-goto "goto" :exit t)
    ("s" org-sort "sort" :exit t)
    ("o" org-occur "occur" :exit t)
    ("r" org-refile "refile" :exit t)
    ("t" org-todo "state" :exit t)
    (":" org-set-tags-command "tags" :exit t)
    ("," org-priority "priority" :exit t)
    ("D" org-insert-drawer "drawer" :exit t)
    ("P" org-set-property "property" :exit t)
    ("N" org-add-note "note" :exit t)
    ("F" org-footnote-action "footnote" :exit t)
    ("a" org-archive-subtree-default "archive" :exit t)
    ("<" org-insert-structure-template "structure" :exit t)
    ("'" org-edit-special "edit-special" :exit t)
    ("e" my-hydra/org-mode-emphasize/body "→ emphasize" :exit t)
    ("q" nil "quit" :exit t))
  (defhydra my-hydra/org-mode-emphasize (:color teal :columns 4)
    "Org-mode → emphasize"
    ("b" (org-emphasize ?*) "bold")
    ("i" (org-emphasize ?/) "italic")
    ("u" (org-emphasize ?_) "underline")
    ("s" (org-emphasize ?+) "strike-through")
    ("c" (org-emphasize ?~) "code")
    ("v" (org-emphasize ?=) "verbatim")
    ("q" my-hydra/org-mode/body "←"))
  ;; use variable pitch font for Org-mode in graphical Emacs
  (when (display-graphic-p)
    (with-eval-after-load 'init-ui-font
      (require 'org-mouse) ;; Org-mode mouse support
      (add-hook 'org-mode-hook #'variable-pitch-mode) ;; enable var-pitch font
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
      (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))))
  ;; maximize org-capture buffer
  (defun my-org-capture-setup (&rest args)
    "Save window configuration prior to `org-capture'."
    (set-frame-parameter nil 'my-org-capture-prior-config (current-window-configuration)))
  (defun my-org-capture-teardown ()
    "Restore window configuration prior to `org-capture'."
    (let ((prior-window-configuration (frame-parameter nil 'my-org-capture-prior-config)))
      (when prior-window-configuration
        (set-window-configuration prior-window-configuration))))
  (advice-add 'org-capture :before 'my-org-capture-setup)
  (add-hook 'org-capture-mode-hook 'delete-other-windows)
  (add-hook 'org-capture-after-finalize-hook 'my-org-capture-teardown))

;; UTF-8 bullets for org-mode
(use-package org-bullets
  :pin "MELPA"
  :after org
  :hook (org-mode . org-bullets-mode)
  :config (setq org-bullets-bullet-list '("■" "◆" "▲" "▶")))

;; Gantt charts via LaTeX
;;
;; assumes org-gantt package is installed. To install, git clone the repository
;; at https://github.com/swillner/org-gantt into ~/.emacs.d/site-lisp
;;
;; to create an org-gantt-chart dynamic block in a Org document, select a given
;; Org subtree using the :ID: property, populate it using "C-c C-x C-u" and
;; export the document to LaTeX via the export menu ("C-c C-e")
;;
;; subtree tasks should have either: A. scheduled ("C-c C-s") and deadline
;; ("C-c C-d") dates, or B. a scheduled date for the first child task,
;; :ORDERED: t for parents, and :Effort: <time-string> for task length
;; and :LINKED-TO: <id_list> to indicate downstream tasks for children
;;
;; to hide the subtree for Gantt chart in the LaTeX output, give it a COMMENT
;; state and make sure the dynamic block appears outside any commented subtree
;;
;; note there is a bug in the package converting :Effort: to length of time,
;; fixable by changing a line in `org-gantt-strings-to-time' in org-gantt.el:
;; --
;;              (* 3600 (or hours-per-day (org-gantt-hours-per-day)) (- 7 (length work-free-days)))
;; --
;; to:
;; --
;;              (* 3600 (or hours-per-day (org-gantt-hours-per-day)) (- 7 (length work-free-days)) (org-gantt-string-to-number weeks-string))
;; --
(use-package org-gantt
  :load-path "site-lisp/org-gantt"
  :ensure nil
  :after org)

;; journaling using Org documents
(use-package org-journal
  :pin "MELPA"
  :after org
  :init
  ;; org-capture helper function from https://github.com/bastibe/org-journal
  (defun my-org-journal-find-location ()
    "Find location of today's Org journal, for use with `org-capture'."
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (goto-char (point-min)))
  ;; add org-capture-template for new journal entries
  (push '("j" "Journal" entry (function my-org-journal-find-location)
              "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
        org-capture-templates)
  (setq org-journal-date-prefix "#+TITLE: Daily Journal "
        org-journal-file-format "%Y%m%d.org"
        org-journal-file-type 'daily))

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
