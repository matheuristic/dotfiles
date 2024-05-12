;;; ereader-theme.el --- eReader color theme

;; Modified work Copyright (C) 2020 matheuristic
;; Original work Copyright (C) 2013-2019 Marian Schubert

;; Author: matheuristic
;; URL: https://github.com/matheuristic/emacs-config
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Minimalistic color theme emulating eReader devices with a dash of color,
;; forked and modified from eink-theme.el at https://github.com/maio/eink-emacs

;; Emacs 27 introduced a new face attribute ':extend' to control face extension
;; at EOL until the edge of the window. By default, this attribute is non-nil
;; only for the `region' and `hl-line' faces. Make sure to set this attribute
;; to t for any face that needs to be extended beyond the EOL.

;;; Code:

(deftheme ereader
  "Theme emulating an eReader device, with some splashes of color.")

(let ((fg "#111111")
      (fg-table "#222291")
      (bg "#fbfbf8")
      (bg-light "#dbdbdb")
      (fg-light "#b7b7b7")
      (bg-highlight "#fff1aa")
      (bg-highlight-2 "light cyan")
      (bg-highlight-3 "light green")
      (fg-dim "#909090")
      (bg-region "#f4f09b")
      (bg-search "#ffb347"))

  (custom-theme-set-faces
   'ereader

   ;; generic stuff
   `(default ((t (:background ,bg :foreground ,fg))))
   `(button ((t (:foreground ,fg :underline t))))
   `(cursor ((t (:background ,fg :foreground ,bg))))
   `(custom-variable-tag ((t (:foreground ,fg :weight bold))))
   `(default-italic ((t (:italic t))))
   `(font-latex-bold-face ((t (:foreground ,fg))))
   `(font-latex-italic-face ((t (:foreground ,fg :slant italic))))
   `(font-latex-match-reference-keywords ((t (:foreground ,fg))))
   `(font-latex-match-variable-keywords ((t (:foreground ,fg))))
   `(font-latex-string-face ((t (:foreground "dark gray"))))
   `(font-lock-builtin-face ((t (:foreground ,fg))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,fg :weight bold))))
   `(font-lock-constant-face ((t (:foreground ,fg))))
   `(font-lock-doc-face ((t (:foreground ,fg :weight semi-bold))))
   `(font-lock-function-name-face ((t (:foreground ,fg))))
   `(font-lock-keyword-face ((t (:foreground ,fg))))
   `(font-lock-preprocessor-face ((t (:foreground ,fg))))
   `(font-lock-reference-face ((t (:foreground ,fg))))
   `(font-lock-string-face ((t (:foreground "#606060"))))
   `(font-lock-type-face ((t (:foreground ,fg))))
   `(font-lock-variable-name-face ((t (:foreground ,fg :underline nil))))
   `(font-lock-warning-face ((t (:foreground ,fg :weight bold))))
   `(fringe ((t (:background ,bg :foreground ,fg))))
   `(gnus-header-content ((t (:foreground ,fg))))
   `(gnus-header-from ((t (:foreground ,fg))))
   `(gnus-header-name ((t (:foreground ,fg))))
   `(gnus-header-subject ((t (:foreground ,fg))))
   `(header-line ((t (:background ,bg-light))))
   `(highlight ((t (:background "#fdf17a"))))
   `(hl-line ((t (:background "#fedcba"))))
   `(ido-first-match ((t (:foreground ,fg))))
   `(ido-only-match ((t (:foreground ,fg))))
   `(ido-subdir ((t (:foreground ,fg))))
   `(isearch ((t (:background ,bg-search :foreground ,fg))))
   `(line-number ((t (:foreground "#cacaca" :weight light))))
   `(link ((t (:foreground ,fg :underline t))))
   `(minibuffer-prompt ((t (:foreground ,fg :weight bold))))
   `(mode-line ((t (:background ,bg :foreground ,fg :inverse-video t))))
   `(mode-line-buffer ((t (:foreground ,fg :weight bold))))
   `(mode-line-inactive ((t (:background ,bg :foreground ,fg-dim :inverse-video t))))
   `(mode-line-minor-mode ((t (:weight ultra-light))))
   `(query-replace ((t (:background ,bg-search :strike-through t))))
   `(region ((t (:background ,bg-region :foreground ,fg))))
   `(secondary-selection ((t (:background "#b4e7a0"))))
   `(slime-repl-inputed-output-face ((t (:foreground ,fg))))
   `(tab-bar ((t (:inherit variable-pitch :background ,bg-light :foreground ,fg :height 0.9))))
   `(tab-bar-tab ((t (:inherit variable-pitch :background ,bg-light :foreground "midnight blue" :underline t :weight extra-bold :height 0.9))))
   `(tab-bar-tab-inactive ((t (:inherit variable-pitch-family :background ,bg-light :foreground ,fg :height 0.9))))
   `(vertical-border ((t (:foreground ,bg-light))))
   `(whitespace-line ((t (:background ,bg-highlight-2 :foreground ,fg))))

   ;; org
   `(org-agenda-date ((t (:foreground ,fg))))
   `(org-agenda-date-today ((t (:foreground ,fg :weight bold))))
   `(org-agenda-date-weekend ((t (:foreground ,fg :weight light))))
   `(org-agenda-structure ((t (:foreground ,fg :weight bold))))
   `(org-block ((t (:foreground ,fg :background "#ffffe0" :extend t))))
   `(org-block-begin-line ((t (:foreground "#555555" :background "#e2e1d5" :extend t))))
   `(org-block-end-line ((t (:foreground "#555555" :background "#e2e1d5" :extend t))))
   `(org-date ((t (:foreground ,fg :underline t))))
   `(org-document-info ((t (:foreground "midnight blue" :slant italic))))
   `(org-document-title ((t (:foreground "midnight blue" :weight bold))))
   `(org-done ((t (:foreground ,fg-light))))
   `(org-hide ((t (:foreground ,bg))))
   `(org-indent ((t (:inherit org-hide))))
   ;; use :overline to give headings more top margin
   `(org-level-1 ((t (:foreground ,fg :weight semi-bold :overline ,bg))))
   `(org-level-2 ((t (:foreground ,fg :weight semi-bold :overline ,bg))))
   `(org-level-3 ((t (:foreground ,fg :weight semi-bold :overline ,bg))))
   `(org-level-4 ((t (:foreground ,fg :weight semi-bold :overline ,bg))))
   `(org-level-5 ((t (:foreground ,fg :weight semi-bold :overline ,bg))))
   `(org-level-6 ((t (:foreground ,fg :weight semi-bold :overline ,bg))))
   `(org-link ((t (:foreground "royal blue" :underline t))))
   `(org-meta-line ((t (:inherit org-document-info-keyword))))
   `(org-quote ((t (:foreground ,fg :slant italic :inherit org-block))))
   `(org-scheduled ((t (:foreground ,fg))))
   `(org-sexp-date ((t (:foreground ,fg))))
   `(org-special-keyword ((t (:foreground ,fg))))
   `(org-table ((t (:foreground ,fg-table))))
   `(org-tag ((t (:inherit shadow :weight bold))))
   `(org-todo ((t (:foreground ,fg))))
   `(org-verbatim ((t (:foreground ,fg :weight semi-bold))))
   `(org-verse ((t (:inherit org-block :slant italic))))

   ;; org-super-agenda
   `(org-super-agenda-header ((t (:foreground ,fg :weight semi-bold))))

   ;; powerline
   `(powerline-active1 ((t (:background "gray22" :foreground ,bg :inherit mode-line))))
   `(powerline-active2 ((t (:background "gray40" :foreground ,bg :inherit mode-line))))

   ;; doom-modeline
   `(doom-modeline-bar ((t (:background ,fg))))
   `(doom-modeline-bar-inactive ((t (:background ,fg-dim))))

   ;; magit
   `(magit-header ((t (:weight semi-bold))))
   `(magit-item-mark ((t (:background ,bg-highlight))))
   `(magit-item-highlight ((t (:weight bold))))
   `(magit-section-heading ((t (:weight semi-bold))))
   `(magit-section-highlight ((t (:weight semi-bold))))
   `(magit-diff-context-highlight ((t (:foreground ,fg))))
   `(magit-branch-local ((t (:weight bold))))
   `(magit-branch-remote ((t (:weight bold))))

   ;; diff
   `(diff-added ((t (:background "#e9ffe9"))))
   `(diff-removed ((t (:background "#ffecec"))))
   `(diff-refine-added ((t (:background "#a4f4a3"))))
   `(diff-refine-removed ((t (:background "#f9cbca"))))
   `(magit-diff-added-highlight ((t (:weight demibold :background "#e9ffe9"))))
   `(magit-diff-added ((t (:background "#e9ffe9"))))
   `(magit-diff-removed-highlight ((t (:weight demibold :background "#ffecec"))))
   `(magit-diff-removed ((t (:background "#ffecec"))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-author-face ((t (:inherit default))))
   `(git-timemachine-minibuffer-detail-face ((t (:weight bold))))

   ;; compile
   `(compilation-error ((t (:inherit error))))

   ;; flycheck
   ;; `(flycheck-error ((t (:inherit error))))
   ;; `(flycheck-warning ((t (:inherit warning))))

   ;; dired
   `(dired-directory ((t (:inherit default))))
   `(dired-subtree-depth-1-face ((t (:inherit default))))
   `(dired-subtree-depth-2-face ((t (:inherit default))))
   `(dired-subtree-depth-3-face ((t (:inherit default))))
   `(dired-subtree-depth-4-face ((t (:inherit default))))

   ;; helm
   `(helm-source-header ((t (:foreground ,fg :background "gray90" :weight bold))))
   `(helm-header ((t (:foreground ,fg))))
   `(helm-selection-line ((t (:inherit region :weight bold))))
   `(helm-selection ((t (:background ,bg-highlight))))
   `(helm-ff-directory ((t (:foreground ,fg :weight bold))))
   `(helm-ff-dotted-directory ((t (:foreground ,fg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,fg :slant italic))))
   `(helm-ff-executable ((t (:foreground ,fg))))

   ;; iedit
   `(iedit-occurrence ((t (:background ,bg-highlight-3 :foreground ,fg))))

   ;; company
   `(company-echo-common ((t (:foreground ,fg))))
   `(company-tooltip-selection ((t (:background ,bg-highlight))))

   ;; parens - parenface
   '(parenface-paren-face ((t (:foreground "gray70"))))
   '(parenface-curly-face ((t (:foreground "gray70"))))
   '(parenface-bracket-face ((t (:foreground "gray70"))))

   ;; parens - paren-face
   '(parenthesis ((t (:foreground "gray70"))))

   ;; parens - other
   `(sp-show-pair-match-face ((t (:foreground "black" :weight bold))))
   `(sp-show-pair-mismatch-face ((t (:background "red" :foreground "black" :weight bold))))
   `(show-paren-match ((t (:foreground "black" :weight bold))))
   `(show-paren-mismatch ((t (:background "red" :foreground "black" :weight bold))))

   ;; js2
   `(js2-function-param ((t (:foreground ,fg))))
   `(js2-external-variable ((t (:foreground ,fg))))

   ;; perl
   `(cperl-hash-face ((t (:foreground ,fg))))
   `(cperl-array-face ((t (:foreground ,fg))))
   `(cperl-nonoverridable-face ((t (:foreground ,fg))))

   ;; rpm-spec-mode
   `(rpm-spec-tag-face ((t (:inherit default))))
   `(rpm-spec-package-face ((t (:inherit default))))
   `(rpm-spec-macro-face ((t (:inherit default))))
   `(rpm-spec-doc-face ((t (:inherit default))))
   `(rpm-spec-var-face ((t (:inherit default))))
   `(rpm-spec-ghost-face ((t (:inherit default))))
   `(rpm-spec-section-face ((t (:inherit default :weight bold))))

   ;; linum / nlinum-relative
   `(nlinum-relative-current-face ((t (:inherit shadow :weight bold))))
   `(linum ((t (:inherit shadow :weight bold))))

   ;; web-mode
   `(web-mode-current-element-highlight-face ((t (:inherit normal :weight bold :foreground ,fg))))

   ;; mmm-mode
   `(mmm-default-submode-face ((t (:inherit normal :background "#ffffef"))))

   ;; misc
   `(idle-highlight ((t (:background ,bg-highlight))))
   `(yas-field-highlight-face ((t (:background ,bg-region :foreground ,fg))))
   `(eshell-prompt ((t (:foreground ,fg :weight bold))))
   `(cider-result-overlay-face ((t (:weight bold))))

   ;; markdown-mode
   `(markdown-code-face ((t (:inherit fixed-pitch :foreground ,fg :background "#ffffe0" :extend t))))

   ;; ace-window
   `(aw-background-face ((t (:foreground ,fg-dim))))
   `(aw-leading-char-face ((t (:foreground "red" :weight bold :height 4.0))))
   `(aw-minibuffer-leading-char-face ((t (:foreground "red" :weight bold))))

   ;; vterm
   `(vterm-color-default ((t (:inherit default :foreground ,fg :background ,bg :extend t))))
   `(vterm-color-black ((t (:inherit term-color-black))))
   `(vterm-color-red ((t (:inherit term-color-red))))
   `(vterm-color-green ((t (:inherit term-color-green))))
   `(vterm-color-yellow ((t (:inherit term-color-yellow))))
   `(vterm-color-blue ((t (:inherit term-color-blue))))
   `(vterm-color-magenta ((t (:inherit term-color-magenta))))
   `(vterm-color-cyan ((t (:inherit term-color-cyan))))
   `(vterm-color-white ((t (:inherit term-color-white))))

   ;; nswbuff
   `(nswbuff-default-face ((t (:foreground "red"))))
   `(nswbuff-current-buffer-face ((t (:foreground "red" :weight bold :underline t))))
   `(nswbuff-separator-face ((t (:inherit shadow))))

   ;; highlight-indent-guides
   `(highlight-indent-guides-odd-face ((t (:background "#e1e1df"))))
   `(highlight-indent-guides-even-face ((t (:background "#c8c8c6"))))
   `(highlight-indent-guides-character-face ((t (:foreground "#c8c8c6" :weight ultra-light))))
   `(highlight-indent-guides-top-odd-face ((t (:background "#7d7d7c"))))
   `(highlight-indent-guides-top-even-face ((t (:background "#646463"))))
   `(highlight-indent-guides-top-character-face ((t (:foreground "#646463" :weight ultra-light))))
   `(highlight-indent-guides-stack-odd-face ((t (:background "#afafad"))))
   `(highlight-indent-guides-stack-even-face ((t (:background "#969694"))))
   `(highlight-indent-guides-stack-character-face ((t (:foreground "#969694" :weight ultra-light))))

   ;; volatile-highlights
   `(vhl/default-face ((t (:background ,bg-highlight-3))))

   ;; evil-quickscope
   `(evil-quickscope-first-face ((t (:foreground ,fg :background ,bg-region))))
   `(evil-quickscope-second-face ((t (:foreground ,fg :background ,bg-highlight-3))))

   ;; evil-snipe
   `(evil-snipe-first-match-face ((t (:foreground ,fg :background ,bg-search))))
   `(evil-snipe-matches-face ((t (:foreground ,fg :background ,bg-highlight-3))))

   ;; evil
   `(evil-ex-lazy-highlight ((t (:background ,bg-highlight-2))))
   `(evil-ex-substitute-matches ((t (:background ,bg-highlight-2))))
   `(evil-ex-substitute-replacement ((t (:background ,bg-highlight :underline nil :foreground ,fg))))))

;; (custom-theme-set-variables
;;  'ereader
;;  `(default-frame-alist (add-to-list 'default-frame-alist '(internal-border-width . ,(if (eq system-type 'darwin) 12 6)))))

(custom-theme-set-variables
 'ereader
 '(highlight-indent-guides-auto-enabled nil)
 '(tab-bar-separator " ⋮ "))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))


(provide-theme 'ereader)

(provide 'ereader-theme)
;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; ereader-theme.el ends here
