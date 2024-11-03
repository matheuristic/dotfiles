;;; acme.el --- Minor mode for emulating Plan 9 Acme interface -*- lexical-binding: t; -*-

;; Copyright (c) 2022 Thiam Lee

;; Author: Thiam Lee
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: acme, mouse
;; URL: https://github.com/matheuristic/emacs-config

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Global minor mode to emulate the Plan 9 Acme interface with some
;; Emacs-specific adaptations. When the mode is enabled, the following
;; modes of operation are supported.
;;
;; Keyboard bindings:
;; * C-S-f will complete file names (like C-f in Plan 9 Acme)
;; * C-escape will cut the selected text into the kill-ring (i.e., the
;;   snarf buffer), or if there is no selection it will select just
;;   inserted text if that was the most recent text operation (like
;;   escape in Plan 9 Acme)
;;
;; Mouse bindings:
;; * Button-1 (normally left-click) selects
;; * Button-2 (normally middle-click) will execute selected text or
;;   the word under the mouse cursor if no text is selected
;; * Button-3 (normally right-click) will plumb the selected text or
;;   word at the mouse cursor according to `acme-mode-plumbing-rules'.
;;   If the text does it not match any plumbing rules it will open a
;;   file if it is the path of an existing file, else it will search
;;   for the next occurrence of the text (for more info, see
;;   http://doc.cat-v.org/plan_9/4th_edition/papers/plumb)
;; * 1-2 chord (press and hold button-1, then press button-2) will
;;   cut selected text into the kill-ring (i.e., the snarf buffer)
;; * 1-3 chord will paste the newest entry from the kill-ring
;; * 2-1 chord will execute selected text or the word under the mouse
;;   cursor with the last selected text region as an argument
;;
;; See http://acme.cat-v.org/mouse for more about the Acme mouse
;; interface.
;;
;; For text execution, these keywords are supported (those marked with
;; [+] are not in Plan 9 Acme and [-] have different behavior):
;;
;; * Cut (cut selected buffer region into the kill-ring)
;; * Del (kill window but not the buffer, except for eshell buffers
;;   where it will also kill the buffer) [-]
;; * Delcol (deletes current column; note that Emacs has a notion of a
;;   window tree where each step changes combination direction between
;;   vertical and horizontal, so this actually deletes the nearest
;;   ancestor vertical combination window) [-]
;; * Delete (kill buffer and window) [-]
;; * Dir (set tag buffer `default-directory' to current buffer's,
;;   mainly useful for using Emacs commands instead of button 3 text
;;   execution from the tag buffer) [+]
;; * Dump (if provided an arg that is a directory, calls `desktop-save'
;;   to save the desktop to that directory, or if no arg is provided
;;   saves the desktop to the first dir specified in `desktop-path')
;; * Exit (quit Emacs)
;; * Get (revert the buffer to the saved version)
;; * Indent (needs to be called with an arg, which can be on or off to
;;   enable or disable `electric-indent-local-mode', or ON or OFF to
;;   enable or disable `electric-indent-mode')
;; * Load (if provided an arg that is a directory, calls `desktop-read'
;;   to load the desktop from that directory, or if no arg is provided
;;   loads the desktop from the first dir specified in `desktop-path')
;; * Look (search for next occurrence optional arguments, treated as
;;   literal search strings, or if no arg then the highlighted region
;;   or word at point)
;; * New (needs to be called with an arg, opens a new window using the
;;   argument as the file name)
;; * Newcol (pops open a new window column; note that Emacs has a
;;   notion of a window tree where each step changes combination
;;   direction between vertical and horizontal, so the new window
;;   column is actually the nearest ancestor non-vertical-combination
;;   window split horizontally with special handling when the selected
;;   window displays the tag buffer) [-]
;; * Paste (if provided a numeric arg yank the arg-th kill-ring entry,
;;   or if no arg provided then yank the first kill-ring entry)
;; * Put (save buffer)
;; * Putall (save all modified buffers)
;; * Redo (takes an optional numeric arg for num times to repeat redo)
;; * Rename (needs to be called with an arg, renames buffer file to
;;   the argument) [+]
;; * Snarf (copy region into kill ring)
;; * Spaces (needs to be called with an arg, which can be on or off to
;;   set `indent-tabs-mode' to t or nil locally, or ON or OFF for the
;;   same but also set the default value of `indent-tabs-mode') [+]
;; * Tab (needs to be called with an arg, which should be a number
;;   that `tab-width' will be set to)
;; * Undo (takes an optional numeric argument that indicates the
;;   number of times to repeat undo)
;; * Zerox (create new window with the same buffer)
;;
;; Acme pipes <, | and > are implemented but not ranges:
;;
;; * Selecting "abcdef" and executing "|tr '[abc]' '[123]' to replace
;;   the selected "abcdef" with "123def" will work.
;; * Executing ",|tr '[abc]' '[123]'" to change all a, b and c chars
;;   in the buffer to 1, 2 and 3 will not work.
;;
;; Some Plan 9 programs designed to work with Acme are implemented as
;; command keywords here:
;;
;; * win (create new window and run an interactive shell, can be
;;   called with an argument which will additionally run the given
;;   argument as a command in the shell)
;;
;; There is a limited tag implementation, in the form of a global tag
;; buffer that can be opened with the minor mode menu or by calling
;; `acme-mode-pop-buffer' (optionally with a prefix numeric argument
;; to open additional tag buffers). This is a regular buffer named
;; according to `acme-mode-tag-buffer-name' and it is automatically
;; populated with some executable keywords when created. It is
;; designed to be used in the following manner: select a target window
;; (where the operations are to be applied) and click or highlight in
;; in it as needed, then mouse 2 (execute) or mouse 3 (search) a word
;; or highlighted region in the global tag buffer to the apply the
;; corresponding operation on the selected window if run without
;; selecting the tag buffer, or on the last selected non-tag buffer if
;; the tag buffer is not selected. For example, if text "abcdef" is
;; selected in a file window and after that "|sed -e 's/abc/123/g'" is
;; executed in a tag buffer without selecting the tag buffer, then the
;; text "abcdef" is changed to "123def". As another example, if text
;; "xyz123" is selected in a file window, then a tag buffer is
;; selected and "|sed -e 's/xyz/abc/g'" is executed, the text "xyz123"
;; in the file window is changed to "abc123". There is no per-file tag
;; like in Plan 9 Acme.
;;
;; When tag buffer windows are created by `acme-mode-pop-tag-buffer',
;; they are fixed to a height of 3 lines. If more lines are desired,
;; resize them using the menu by dragging the its modeline, then call
;; `acme-mode-pop-tag-buffer' again (using the minor mode menu or M-x
;; or some binding) to fix the height to the new size.
;;
;; Recommended settings for the above tag workflow:
;;
;;   ;; mouse focus settings for better Acme mode tag buffer workflow
;;   (setq mouse-autoselect-window nil
;;         acme-mode-use-frames (not (null focus-follows-mouse)))
;;
;; On trackpads, clicks with modifier keys pressed can be used to
;; simulate middle- and right-clicks, and keyboard keys can be used to
;; simulate a left-click, middle-click and right-click during a chord
;; (at least one mouse button held down), allowing for 1-2 and 1-3
;; chords. This can be enabled by adding the appropriate bindings to
;; `acme-mode-map' and customizing `acme-mode-keyboard-chord-keylist'
;; prior to enabling Acme mode (see sample configuration below).
;;
;; Basic plumbing support is implemented, by specifying the
;; `acme-mode-plumbing-rules' customization variable, whose value
;; should be an associative list of the form
;; '((REGEXP1 . FUNCTION1) (REGEXP2 . FUNCTION2) ...)
;; where plumbed text is matched against the regular expressions
;; and the first match will have its function called with the
;; plumbed text. Should no regular expression in the associative
;; list keys match the plumbed text, the plumbed text is checked
;; to see if it is the path to an existing file and opened in a
;; new buffer window if so, or used to do a forward search in the
;; buffer otherwise. See the code for `acme-mode--plumb-python-error'
;; and the definition of `acme-mode-default-plumbing-rules' for
;; examples of plumbing regexps and functions. The default rules
;; include plumbing website URLs and Python error locations. Example:
;;
;;   (setq acme-mode-plumbing-rules
;;         '(("https?://.*" . browse-url)
;;           (" *File \"[a-zA-Z¡-￿0-9_./-]+\", line [0-9]+.*" . acme-mode--plumb-python-error))
;;
;; There is some support for using frames instead of window enabled by
;; setting `acme-mode-use-frames' to t, but this is quite untested. It
;; requires that the windowing system be set to focus follows mouse
;; (otherwise the tag buffer frame does not work properly), and that
;; `focus-focus-mouse' is appropriately set.
;;
;; Sample configuration:
;;
;;   (require 'acme)
;;   (with-eval-after-load 'acme
;;     ;; Customize options
;;     (setq acme-mode-use-frames nil
;;           acme-mode-per-dir-shell-output nil
;;           mouse-autoselect-window nil)
;;     (setq acme-mode-user-command-keywords
;;           '(("Commentcol" . (lambda (arg) (comment-set-column (string-to-number arg))))
;;             ("Expand" . (lambda () (call-interactively 'er/expand-region))) ; requires expand-region
;;             ("Fillcol" . (lambda (&optional arg)
;;                            (if arg
;;                                (setq fill-column (string-to-number arg))
;;                              (setq fill-column 70))))
;;             ("Fillpar" . (lambda () (call-interactively 'fill-paragraph)))
;;             ("Fontlock" . (lambda () (call-interactively 'font-lock-mode)))
;;             ("Imenu" . (lambda () (save-selected-window (imenu-list-smart-toggle)))) ; requires imenu-list
;;             ("Lnumbers" . (lambda () (call-interactively 'display-line-numbers-mode)))
;;             ("TRAMPCleanup" . (lambda ()
;;                                 (when (y-or-n-p "Cleanup all TRAMP buffers and connections? ")
;;                                   (tramp-cleanup-all-buffers))))
;;             ("term" . (lambda () (call-interactively 'ansi-term)))))
;;     (setq acme-mode-initial-tag-line (concat acme-mode-initial-tag-line "\n"
;;                                              "Expand (Fillcol 70) Fillpar Fontlock "
;;                                              "Imenu Lnumbers TRAMPCleanup term win "))
;;     (setq acme-mode-plumbing-rules
;;           (append '(("^\\(gemini\\|gopher\\)://[^ |{};]*[^/]/?$" . (lambda (url) (elpher-go url))) ; requires elpher
;;                     ("^PEP-?[0-9]+$" . (lambda (pep-line)
;;                                          (save-match-data
;;                                            (and (string-match "PEP-?\\([0-9]+\\)" pep-line)
;;                                                 (acme-mode--plumb
;;                                                  (concat "https://www.python.org/dev/peps/pep-"
;;                                                          (format "%04d"
;;                                                                  (string-to-number (match-string 1 pep-line)))
;;                                                          "/")))))))
;;                   acme-mode-plumbing-rules))
;;     (push 'imenu-list-major-mode acme-mode-exclude-major-modes)
;;     (push 'rg-mode acme-mode-exclude-major-modes)
;;     ;; Open tag buffer key binding
;;     (define-key acme-mode-map [C-s-return] #'acme-mode-pop-tag-buffer)
;;     (define-key acme-mode-map [s-return] #'acme-mode-set-tag-buffer-default-directory-to-current)
;;     ;; Trackpad support
;;     (cond ((eq system-type 'darwin)
;;            ;; middle-click
;;            (define-key acme-mode-map [M-down-mouse-1] #'acme-mode--down-mouse-2)
;;            (define-key acme-mode-map [M-mouse-1] #'acme-mode--mouse-2)
;;            (define-key acme-mode-map [M-drag-mouse-1] #'acme-mode--mouse-2)
;;            ;; right-click
;;            (define-key acme-mode-map [s-down-mouse-1] #'acme-mode--down-mouse-3)
;;            (define-key acme-mode-map [s-double-down-mouse-1] #'acme-mode--down-mouse-3)
;;            (define-key acme-mode-map [s-triple-down-mouse-1] #'acme-mode--down-mouse-3)
;;            (define-key acme-mode-map [s-mouse-1] #'acme-mode--mouse-3)
;;            (define-key acme-mode-map [s-drag-mouse-1] #'acme-mode--mouse-3)
;;            (setq acme-mode-keyboard-chord-keylist
;;                  '(("1" . 1) ("s-1" . 1) ("M-1" . 1)
;;                    ("2" . 2) ("s-2" . 2) ("M-2" . 2)
;;                    ("3" . 3) ("s-3" . 3) ("M-3" . 3))))
;;           ((eq system-type 'gnu/linux)
;;            ;; middle-click
;;            (define-key acme-mode-map [C-down-mouse-1] #'acme-mode--down-mouse-2)
;;            (define-key acme-mode-map [C-mouse-1] #'acme-mode--mouse-2)
;;            (define-key acme-mode-map [C-drag-mouse-1] #'acme-mode--mouse-2)
;;            ;; right-click
;;            (define-key acme-mode-map [M-down-mouse-1] #'acme-mode--down-mouse-3)
;;            (define-key acme-mode-map [M-double-down-mouse-1] #'acme-mode--down-mouse-3)
;;            (define-key acme-mode-map [M-triple-down-mouse-1] #'acme-mode--down-mouse-3)
;;            (define-key acme-mode-map [M-mouse-1] #'acme-mode--mouse-3)
;;            (define-key acme-mode-map [M-drag-mouse-1] #'acme-mode--mouse-3)
;;            ;; in ChromeOS, Alt-left-click gets auto-translated to right-click
;;            (define-key acme-mode-map [M-down-mouse-3] #'acme-mode--down-mouse-3)
;;            (define-key acme-mode-map [M-double-down-mouse-3] #'acme-mode--down-mouse-3)
;;            (define-key acme-mode-map [M-triple-down-mouse-3] #'acme-mode--down-mouse-3)
;;            (define-key acme-mode-map [M-mouse-3] #'acme-mode--mouse-3)
;;            (define-key acme-mode-map [M-drag-mouse-3] #'acme-mode--mouse-3)
;;            (setq acme-mode-keyboard-chord-keylist
;;                  '(("1" . 1) ("C-1" . 1) ("M-1" . 1)
;;                    ("2" . 2) ("C-2" . 2) ("M-2" . 2)
;;                    ("3" . 3) ("C-3" . 3) ("M-3" . 3)))))
;;     ;; enable `acme-mode'
;;     (acme-mode 1))
;;
;; Originally based on chording and file finding code from
;; https://github.com/akrito/acme-mouse/blob/master/acme-mouse.el
;; (which incorporates code for Acme-like search from Dan McCarthy's
;; https://www.emacswiki.org/emacs/acme-search.el) that was adapted
;; into a minor mode, with modifications that include middle-click for
;; executing text in an external shell, expanded chording, plumbing,
;; tag implementation, Acme pipes (though no range selection), and
;; accommodations for trackpad use.
;;
;; TODO:
;;
;; * Moving the mouse after plumbing sometimes inputs a prefix arg C-u
;;   on ChromeOS so look into why that happens
;;
;; Differences versus Plan 9 Acme:
;;
;; * Window tags (see above description of tag buffer)
;; * Execution output is not streaming, only displays on shell return
;; * Indent ON and Indent OFF do not automatically apply its settings
;;   to already open buffers aside from the currently selected one,
;;   and these settings may be superceded by other major-mode or
;;   minor-mode bindings (e.g., in markdown-mode <return> is bound
;;   to `markdown-enter-key')
;; * Dir, Rename and Spaces keywords
;; * Del keyword closes the window but not the buffer (in Plan 9 Acme,
;;   when a file's last window is closed the file is closed too if it
;;   is not dirty)
;; * Delete keyword kills the buffer and closes the window (this is
;;   actually similar to Plan 9 Acme's Del keyword, whereas Delete in
;;   Plan 9 Acme actually closes the window, and the file if the last
;;   window for that file, without checking if the window is dirty)
;; * Paste takes an optional argument to facilitate yanking entries
;;   in the kill-ring other than the most recent one (in Plan 9 Acme,
;;   there is no kill-ring and the snarf buffer only holds one entry)
;; * Redo and Undo optionally take integer arguments to repeat the
;;   action more than once
;; * Newcol and Delcol are attuned to how Emacs handles windows
;;   in frames, which is not the same as Plan 9 Acme which
;;   groups windows into columns
;; * win always drops the user back into a shell (in Plan 9 this
;;   program when given an argument does not use an interactive shell)
;; * No drag-and-drop to move or reorder windows
;; * Many others, in all likelihood
;;
;; Limitations:
;;
;; * Currently, there does not appear to be a way to keep the point
;;   in the same location within the buffer (not the viewport) when
;;   scrolling, see https://stackoverflow.com/a/72302778
;;
;; Maybe later:
;;
;; * Ranges for pipes like "12,23-/[Aa]bc/|tr '[abc]' '[123]'"
;;   (for now select manually, e.g., "C-x h" for the whole buffer)
;; * Migrate button action logic fully to transient maps for better
;;   robustness?
;;
;; Out-of-scope for now:
;;
;; * Edit (need to implement structural regexps)
;; * Font (use the menubar)
;; * ID (window IDs as strings are not helpful for the user)
;; * Kill (use `kill-process')
;; * Send (term implementations each have their own mechanism)
;; * Sort (Emacs window management behaves differently from Acme's)
;;
;; Known bugs:
;;
;; * When the same buffer is visible in multiple windows, the
;;   highlighting in non-selected windows can be wonky. Do
;;     (add-hook 'acme-mode-hook (lambda () (setq highlight-nonselected-windows nil)))
;;   if this is too distracting to turn off highlighting of regions
;;   in all non-selected windows. For more info, see email thread at
;;   https://lists.gnu.org/archive/html/emacs-devel/2015-03/msg01062.html

;;; Code:

(require 'cl-extra)
(require 'desktop)
(require 'ring)
(require 'subr-x)

(defgroup acme nil
  "Acme mouse emulation."
  :group 'environment
  :group 'editing
  :group 'mouse)

;; INTERNAL VARIABLES

;; Default command keywords
(defvar acme-mode--builtin-command-keywords
  '(("Cut" . (lambda ()
               (if (region-active-p)
                   (kill-region (mark) (point))
                 (message "Cut command requires a selected buffer region."))))
    ("Del" . (lambda ()          ; delete window but don't kill buffer
               (when (or (not (buffer-modified-p))
                         (or (acme-mode--tag-buffer-window-p)
                             (y-or-n-p "Buffer modified. Delete window anyway? ")))
                 (let ((eshell-buffer-p (eq major-mode 'eshell-mode))
                       (windows (seq-remove 'acme-mode--tag-buffer-window-p (window-list))))
                   (if (> (length windows) 1)
                       (progn
                         (when eshell-buffer-p
                           (and (y-or-n-p "Window buffer is an eshell buffer. Kill it along with its window? ")
                                (kill-buffer)))
                         (delete-window))
                     (message "Cannot delete dedicated tag buffer frame or only non-tag buffer window."))))))
    ("Delcol" . (lambda ()
                  (let ((root (frame-root-window))
                        (win (selected-window)))
                    (while (and (not (eq win root))
                                (not (window-combined-p win t)))
                      (setq win (window-parent win)))
                    (if (eq win root)
                        (message "Not deleting column as there is only one in the window frame.")
                      (let ((parent-win (window-parent win)))
                        (delete-window win))))))
    ("Delete" . (lambda ()             ; kill buffer and delete window
                  (if (acme-mode--tag-buffer-window-p)
                      (message "Tag buffer frames have to be killed manually using `kill-buffer' or \"C-x k\".")
                    (when (or (not (buffer-modified-p))
                              (y-or-n-p "Buffer modified. Kill buffer anyway? "))
                      (let ((windows (seq-remove 'acme-mode--tag-buffer-window-p (window-list))))
                        (if (> (length windows) 1)
                            (kill-buffer-and-window)
                          (kill-buffer)))))))
    ("Dir" . (lambda ()
               (acme-mode-set-tag-buffer-default-directory-to-current)))
    ("Dump" . (lambda (&optional directory-path)
                (let ((directory-path (or directory-path (car desktop-path))))
                  (if (and directory-path (> (length directory-path) 0))
                      (progn
                        (desktop-save directory-path)
                        (message "Saved desktop to directory `%s'" directory-path))
                    (message "Dump desktop dir path empty and none specified in `desktop-path'.")))))
    ("Exit" . (lambda () (when (y-or-n-p "Quit Emacs? ") (save-buffers-kill-terminal))))
    ("Get" . (lambda () (revert-buffer)))
    ("Indent" . (lambda (&optional arg)
                  (if (and arg (> (length arg) 0))
                      (cond ((string-equal arg "on")
                             (electric-indent-local-mode 1)
                             (message "Indent set to `%s'" arg))
                            ((string-equal arg "ON")
                             (electric-indent-local-mode 1)
                             (electric-indent-mode 1)
                             (message "Indent set to `%s'" arg))
                            ((string-equal arg "off")
                             (electric-indent-local-mode 0)
                             (message "Indent set to `%s'" arg))
                            ((string-equal arg "OFF")
                             (electric-indent-local-mode 0)
                             (electric-indent-mode 0)
                             (message "Indent set to `%s'" arg))
                            (t
                             (message "Insert command argument `%s' unknown." arg)))
                    (message "Indent command requires an argument (on, off, ON or OFF)."))))
    ("Load" . (lambda (&optional directory-path)
                (let ((directory-path (or directory-path (car desktop-path))))
                  (if (and directory-path (> (length directory-path) 0))
                      (progn
                        (desktop-read directory-path)
                        (message "Loaded desktop from directory `%s'" directory-path))
                    (message "Load desktop dir path empty and none specified in `desktop-path'.")))))
    ("Look" . (lambda (&optional text)
                (unless text
                  (let* ((event (list 'mouse-3 (posn-at-point)))
                         (seltext (acme-mode--get-seltext event 'filename)))
                    (acme-mode--clear-secondary-selection)
                    (setq text seltext)))
                (if (and text (> (length text) 0))
                    (acme-mode--search text t)
                  (message "Look argument empty or nothing at point."))))
    ("New" . (lambda (&optional file-name)
               (if (and file-name (> (length file-name) 0))
                   (let (new-win)
                     (acme-mode--with-last-non-tag-buffer-window
                      (let ((win (selected-window)))
                        ;; If calling from tag, open shell from the last non-tag buffer window
                        (when (acme-mode--tag-buffer-window-p win)
                          (setq win acme-mode--last-non-tag-buffer-window))
                        (with-selected-window win
                          (setq new-win (acme-mode--pop-file-window file-name acme-mode-use-frames)))))
                     (when new-win
                       (select-window new-win)
                       (unless acme-mode-no-warp-mouse
                         (acme-mode--warp-mouse-to-point))))
                 (message "New command requires a non-empty file name argument."))))
    ("Newcol" . (lambda () ; split horizontally on nearest ancestor vertical combination window
                  (let (new-win)
                    (setq new-win (acme-mode--pop-new-column))
                    (when new-win
                      (select-window new-win)
                      (balance-windows (window-parent new-win))))))
    ("Paste" . (lambda (&optional arg)
                 (yank (when arg (string-to-number arg)))))
    ("Put" . (lambda ()                 ; save buffer
               (save-buffer)))
    ("Putall" . (lambda () (save-some-buffers))) ; save all buffers
    ("Redo" . (lambda (&optional arg)
                (if (version<= "28" emacs-version)
                    (progn
                      (deactivate-mark)
                      (call-interactively 'undo-redo
                                          t
                                          (when arg (vector (string-to-number arg)))))
                  (message "Redo is supported only for Emacs version>=28.1."))))
    ("Rename" . (lambda (&optional file-name)
                  (if (and file-name (> (length file-name) 0))
                      (acme-mode--rename-buffer-file file-name)
                    (message "Rename command requires a non-empty file name argument."))))
    ("Snarf" . (lambda ()              ; copy selection into kill ring
                 (if (region-active-p)
                     (progn (setq deactivate-mark nil)
                            (kill-ring-save (mark) (point)))
                   (message "Snarf command requires a selected buffer region."))))
    ("Spaces" . (lambda (&optional arg)
                  (if (and arg (> (length arg) 0))
                      (cond ((string-equal arg "on")
                             (setq indent-tabs-mode nil)
                             (message "Spaces set to `%s'" arg))
                            ((string-equal arg "ON")
                             (setq indent-tabs-mode nil)
                             (setq-default indent-tabs-mode nil)
                             (message "Spaces set to `%s'" arg))
                            ((string-equal arg "off")
                             (setq indent-tabs-mode t)
                             (message "Spaces set to `%s'" arg))
                            ((string-equal arg "OFF")
                             (setq indent-tabs-mode t)
                             (setq-default indent-tabs-mode nil)
                             (message "Spaces set to `%s'" arg))
                            (t
                             (message "Spaces command argument `%s' unknown." arg)))
                    (message "Spaces command requires an argument (on, off, ON or OFF)."))))
    ("Tab" . (lambda (&optional arg)
               (if (and arg (> (length arg) 0))
                   (progn
                     (setq tab-width (string-to-number arg))
                     (message "Tab width set to `%s'" arg))
                 (message "Tab command requires a integer argument."))))
    ("Undo" . (lambda (&optional arg)
                (if (version<= "28" emacs-version)
                    (progn
                      (deactivate-mark)
                      (call-interactively 'undo-only
                                          t
                                          (when arg (vector (string-to-number arg)))))
                  (message "Redo is supported only for emacs version>=28.1."))))
    ("Zerox" . (lambda ()
                 (let (new-win)
                   (if acme-mode-use-frames
                       (make-frame-command)
                     (setq new-win (split-window-below)))
                   (when new-win
                     (select-window new-win)
                     (unless acme-mode-no-warp-mouse
                       (acme-mode--warp-mouse-to-point))))))
    ("win" . (lambda (&optional arg)
               (let (shell-buf win)
                 (save-window-excursion
                   (setq shell-buf (eshell t)))
                 ;; If calling from tag, open shell from the last non-tag buffer window
                 (when (acme-mode--tag-buffer-window-p win)
                   (setq win acme-mode--last-non-tag-buffer-window))
                 (setq win (acme-mode--pop-buffer-window shell-buf acme-mode-use-frames nil 'below win))
                 (when win
                   (select-window win)
                   (unless acme-mode-no-warp-mouse
                     (acme-mode--warp-mouse-to-point))
                   (when arg
                     (with-current-buffer shell-buf
                       (eshell-return-to-prompt)
                       (insert arg)
                       (eshell-send-input))))))))
  "Default command keywords association list for Acme mode.")

;; Default plumbing rules
(defvar acme-mode-default-plumbing-rules
  '(;; HTTP and HTTPS URLs
    ("^https?://[^ ]*$" . browse-url)
    ;; Python error locations
    ("^ *File \"[~a-zA-Z¡-￿0-9_./-]+\", line [0-9]+.*" . acme-mode--plumb-python-error)
    ;; EPUB files (open generically)
    ("^[a-zA-Z¡-￿0-9_./\\(\\)&-][ a-zA-Z¡-￿0-9_./\\(\\)&-]*\\.[Ee][Pp][Uu][Bb]$" . acme-mode--plumb-file-system-open)
    ;; PDF files (open generically)
    ("^[a-zA-Z¡-￿0-9_./\\(\\)&-][ a-zA-Z¡-￿0-9_./\\(\\)&-]*\\.[Pp][Dd][Ff]$" . acme-mode--plumb-file-system-open)
    ;; TRAMP file or directory, format is
    ;; /method:user@host:/path/to/dir/or/file (e.g., "/ssh:dotsshconfighost:.") or
    ;; /method1:user1@host1|method2:user2@host2:/path/to/dir/or/file (e.g. "/ssh:dotsshconfighost|sudo::.")
    ;; For more info on the second format, see
    ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Ad_002dhoc-multi_002dhops.html and
    ;; https://stackoverflow.com/questions/2177687/open-file-via-ssh-and-sudo-with-emacs
    ("^/[a-zA-Z0-9._-]+:[a-zA-Z0-9._@-]*\\(|[a-zA-Z0-9._-]+:[a-zA-Z0-9._@-]*\\)*:\\([a-zA-Z¡-￿0-9_./\\(\\)&-][ a-zA-Z¡-￿0-9_./\\(\\)&-]*\\)" . find-file))
  "Default plumbing rules for Acme mode.

See `acme-mode-plumbing-rules'.")

;; Button values
(defvar acme-mode--lbutton 1)
(defvar acme-mode--mbutton 2)
(defvar acme-mode--rbutton 4)
(defvar acme-mode--allbuttons (+ acme-mode--lbutton
                                 acme-mode--mbutton
                                 acme-mode--rbutton))
(defvar acme-mode--nobuttons 0)

(defvar acme-mode--state 'noselect
  "State of Acme mode.

Possible states:
* \\='noselect (neutral state with no mouse buttons pressed)
* \\='textselect (1 pressed but no further buttons)
* \\='textselect-cut (1-2 chord last run but some buttons still pressed)
* \\='textselect-paste (1-3 chord last run but some buttons still pressed)
* \\='textselect2 (2 pressed but no further buttons)
* \\='textselect3 (3 pressed but no further buttons)
* \\='donothing (2-3 chord or 3-2 chord pressed, to cancel execute or look)")

(defvar acme-mode--buttons acme-mode--nobuttons
  "Current buttons pressed stored as bits of an integer.

Examples:
  (eql acme-mode-state-down acme-mode--nobuttons)
    => t if no buttons pressed, nil if any pressed
  (eql acme-mode-state-down acme-mode--lbutton)
    => t if _only_ left button pressed, nil otherwise
  (> (logand acme-mode-state-down acme-mode--rbutton) 0)
    => t if right button pressed, 0 if not")

(defvar acme-mode--region-start nil
  "Start of last cut region.")

(defvar acme-mode--region-end nil
  "End of last cut region.")

(defvar acme-mode--last-mouse-event nil
  "Last button 1, 2 or 3 mouse event.")

(defvar acme-mode--argtext nil
  "Cached argument for 2-1 chords.")

(defvar acme-mode--prior-delete-selection-mode nil
  "Whether Delete Selection mode was enabled prior to Acme mode.")

(defvar acme-mode--prior-highlight-nonselected-windows nil
  "Whether highlighting of non-selected windows was active prior to Acme mode.")

(defvar acme-mode--prior-transient-mark-mode nil
  "Whether Transient Mark mode was enabled prior to Acme mode.")

(defvar acme-mode--last-non-tag-buffer-window nil
  "Last non-tag buffer window selected.")

(defvar acme-mode--older-non-tag-buffer-window-ring (make-ring 16)
  "Selected non-tag buffer windows previous to the last.

Used mainly for chained Del text executions from the tag buffer.")

(defvar acme-mode--textselect-cut-or-paste-first nil
  "Whether cut or paste action was done first during current textselect.

Possible values: \\='cut or \\='paste or nil")

;; CUSTOMIZATION VARIABLES

(defcustom acme-mode-use-frames nil
  "Use new frames instead of windows when popping to a tag file or plumbed file.

This should be enabled only if the windowing system is set to
focus follows mouse. Additionally, `mouse-autoselect-window'
should be set to nil and `focus-follows-mouse' is appropriate set
to t or `auto-raise'.

This is currently EXPERIMENTAL and quite untested."
  :type 'boolean)

(defcustom acme-mode-plumbing-rules acme-mode-default-plumbing-rules
  "Association list ((REGEXP . FUNCTION) ...) for plumb dispatch.

Whenever a string is plumbed by `acme-mode--plumb', it is
dispatched to the function associated with the first key in
`acme-mode-plumbing-rules' which regexp-matches the string.

The corresponding function is called with the plumbed string.

If there is no key that regexp-matches the plumbed string, it
is instead dispatched to `acme-mode--find-file-or-search'."
  :type '(alist :key-type (string :tag "Key") :value-type (function :tag "Value")))

(defcustom acme-mode-user-command-keywords nil
  "Association list ((KEYWORD . FUNCTION) ...) of user-defined command keywords.

When executing text, if the first word of the text matches
a key in this association list, the corresponding function
is called with the appropriate arity:

* If the text has only one word (i.e., the keyword) then FUNCTION
  is called with no arguments, that is, (FUNCTION) is called.

* If the text has more than one word, then FUNCTION is called
  with the substring of the text after the first word and
  whitespace as an argument, that is (FUNCTION ARG) is called.
  E.g., if the text is \"abc def ghi\" then (abc \"def ghi\") is
  called.

Functions that take no arguments should have form:
  (lambda () body)

Those must take an argument should have form:
  (lambda (arg) body)

And those that take an optional argument should have form:
  (lambda (&optional arg) body)

Command keywords in this association list take precedence over
the built-in command keywords, so the keywords in the latter
group by be shadowed by user-defined ones as desired.

Example:

  (setq acme-mode-user-command-keywords
        \\='((\"Commentcol\" . (lambda (arg)
                              (comment-set-column (string-to-number arg))))
          (\"Fillcol\" . (lambda (&optional arg)
                            (if arg
                                (setq fill-column (string-to-number arg))
                              (setq fill-column 70))))
          (\"Fontlock\" . (lambda ()
                            (call-interactively \\='font-lock-mode)))
          (\"Lnumbers\" . (lambda ()
                            (call-interactively \\='display-line-numbers-mode)))))"
  :type '(alist :key-type (string :tag "Key") :value-type (function :tag "Value")))

(defcustom acme-mode-keyboard-chord-keylist nil
  "Association list ((KEY . MOUSEBUTTON) ...) to specify keyboard chord keys.

When a mouse button is held down in Acme mode, each specified KEY
will trigger a press-down and release of its associated MOUSEBUTTON.

Can be helpful when using a laptop trackpad.

Example:

  (setq acme-mode-keyboard-chord-keylist \\='((\"z\" . 1) (\"x\" . 2) (\"c\" . 3)))"
  :type '(alist :key-type (string :tag "Key") :value-type (integer :tag "Mouse button")))

(defcustom acme-mode-no-warp-mouse nil
  "Don't warp the mouse when searching and opening files by plumbing in Acme mode."
  :type 'boolean)

(defcustom acme-mode-exclude-major-modes '(compilation-mode
                                           completion-list-mode
                                           dired-mode
                                           flymake-diagnostics-buffer-mode
                                           ibuffer-mode
                                           Info-mode
                                           minibuffer-inactive-mode
                                           minibuffer-mode
                                           special-mode ; help-mode, occur-mode, vundo-mode are derived from special-mode
                                           undo-tree-visualizer-mode)
  "List of major modes for which to not use Acme mode mouse interace.

Acme mode mouse mappings are also not used for any major modes derived
from ones in this list.

Leverages approach from https://emacs.stackexchange.com/a/59509
for https://emacs.stackexchange.com/questions/59494 that shows
how to wrap/intercept commands bound to a given key in Emacs."
  :type '(list :tag "Major modes"
           (symbol :tag "Major mode")))

(defcustom acme-mode-tag-buffer-name "*AcmeTag*"
  "Base name of Acme mode tag buffer."
  :type 'string)

(defcustom acme-mode-shell-output-buffer-name "*AcmeOutput*"
  "Base name of Acme mode shell output buffer."
  :type 'string)

(defcustom acme-mode-per-dir-shell-output nil
  "Use a buffer per directory for Acme text execution shell output."
  :type 'boolean)

(defcustom acme-mode-initial-tag-line "Dump Load Exit | Newcol Delcol Del Snarf Get Undo Redo Put Zerox | Look "
  "Initial tag line for new tag buffers."
  :type 'string)

;; MODE DEFINITIONS AND FUNCTIONS

(defun acme-mode--enable ()
  "Setup for Acme mode."
  (setq acme-mode--prior-delete-selection-mode (symbol-value delete-selection-mode))
  (setq acme-mode--prior-highlight-nonselected-windows 'highlight-nonselected-windows)
  (setq acme-mode--prior-transient-mark-mode (symbol-value transient-mark-mode))
  (delete-selection-mode 1)
  (transient-mark-mode 1)
  (setq highlight-nonselected-windows t)
  (setq acme-mode--state 'noselect
        acme-mode--buttons acme-mode--nobuttons
        acme-mode--region-start nil
        acme-mode--last-mouse-event nil
        acme-mode--argtext nil)
  (add-hook 'post-command-hook 'acme-mode--update-last-non-tag-buffer-window)
  (acme-mode--update-last-non-tag-buffer-window)
  (unless (display-graphic-p)
    (message (concat "Note that Acme mode is mainly designed for and tested "
                     "with graphical Emacs, it may not work as intended in the "
                     "terminal."))))

(defun acme-mode--disable ()
  "Teardown for Acme mode."
  (remove-hook 'post-command-hook 'acme-mode--update-last-non-tag-buffer-window)
  (setq highlight-nonselected-windows acme-mode--prior-highlight-nonselected-windows)
  (transient-mark-mode acme-mode--prior-transient-mark-mode)
  (delete-selection-mode acme-mode--prior-delete-selection-mode))

;; See https://emacs.stackexchange.com/questions/64964/difference-between-mouse-1-and-down-mouse-1
(defvar acme-mode-map
  (let ((map (make-sparse-keymap)))
    ;; file name completion (like C-f in Plan 9 Acme)
    (define-key map (kbd "C-S-f") #'acme-mode--complete-file-name)
    ;; select last insertion (like ESC in Plan 9 Acme)
    (define-key map [C-escape] #'acme-mode--kill-region-or-select-last-insertion)
    ;; left-click
    (define-key map [down-mouse-1] #'acme-mode--down-mouse-1)
    (define-key map [mouse-1] #'acme-mode--mouse-1)
    (define-key map [double-mouse-1] #'acme-mode--double-mouse-1)
    (define-key map [triple-mouse-1] #'acme-mode--double-mouse-1)
    (define-key map [drag-mouse-1] #'acme-mode--drag-mouse-1)
    ;; middle-click
    (define-key map [down-mouse-2] #'acme-mode--down-mouse-2)
    (define-key map [mouse-2] #'acme-mode--mouse-2)
    (define-key map [drag-mouse-2] #'acme-mode--mouse-2)
    ;; right-click
    (define-key map [down-mouse-3] #'acme-mode--down-mouse-3)
    (define-key map [double-down-mouse-3] #'acme-mode--down-mouse-3)
    (define-key map [triple-down-mouse-3] #'acme-mode--down-mouse-3)
    (define-key map [mouse-3] #'acme-mode--mouse-3)
    (define-key map [drag-mouse-3] #'acme-mode--mouse-3)
    map)
  "Acme mode keymap.")

;; Mode-line and menu
(easy-menu-define acme-mode-menu acme-mode-map "Acme mode menu."
  '("Acme"
    [ "Open tag buffer" acme-mode-pop-tag-buffer t ]
    [ "Set tag buffer default-dir" acme-mode-set-tag-buffer-default-directory-to-current t ]
    "--"
    [ "Turn off Acme mode" acme-mode t ]))

;;;###autoload
(define-minor-mode acme-mode
  "Global minor mode to emulate the Plan 9 Acme mouse interface.

When called interactively, toggle `acme-mode'. With prefix ARG,
enable `acme-mode' if ARG is positive, otherwise disable it.

When called from Lisp code, enable `acme-mode' if ARG is omitted,
nil or positive. If ARG is `toggle', toggle `acme-mode'.

\\{acme-mode-map}"
  :init-value nil
  :lighter " Acme"
  :keymap acme-mode-map
  :global t
  (if acme-mode
      (acme-mode--enable)
    (acme-mode--disable)))

;; HELPER FUNCTIONS

(defmacro acme-mode--with-last-non-tag-buffer-window (&rest body)
  "Execute forms in BODY with the last non-tag buffer as the selected window.
The value returned is the value of the last form in BODY.

This macro is otherwise similar to `with-selected-window', most
notably that after the forms in BODY are run, the selected window
is set back to the window and buffer at the time of calling."
  `(let ((non-tagbuf-win (if (acme-mode--tag-buffer-window-p)
                           acme-mode--last-non-tag-buffer-window
                         (selected-window))))
     (with-selected-window non-tagbuf-win ,@body)))

(defun acme-mode--down-p (&rest buttons)
  "Check `acme-mode-buttons' if exactly the given BUTTONS are pressed."
  (let ((checkval (apply '+ buttons)))
    (eql acme-mode--buttons checkval)))

(defun acme-mode--button-down (button)
  "Update `acme-mode--buttons' on BUTTON press."
  (setq acme-mode--buttons
        (logior acme-mode--buttons button)))

(defun acme-mode--button-up (button)
  "Update `acme-mode--buttons' on BUTTON release."
  (setq acme-mode--buttons
        (logand acme-mode--buttons
                (- acme-mode--allbuttons button))))

(defun acme-mode--maybe-reset-state ()
  "Set `acme-mode--state' to \\='noselect if no buttons are pressed."
  (when (acme-mode--down-p acme-mode--nobuttons)
    (setq acme-mode--state 'noselect)))

(defun acme-mode--select-region ()
  "Acme mode region selection function to faciliate 1-2 and 1-3 chords."
  (let ((range (mouse-start-end (mark)
                                (point)
                                mouse-selection-click-count)))
    (setq acme-mode--region-start (nth 0 range))
    (setq acme-mode--region-end (nth 1 range))
    (set-mark acme-mode--region-start)
    (goto-char acme-mode--region-end)))

(defun acme-mode--update-last-mouse-events (event)
  "Update `acme-mode--last-mouse-event' with new EVENT."
  (setq acme-mode--last-mouse-event event))

(defun acme-mode--clear-secondary-selection ()
  "Clears the secondary selection."
  (gui-set-selection 'SECONDARY nil)
  (delete-overlay mouse-secondary-overlay))

(defun acme-mode--get-active-region-text ()
  "Return selected text without properties, if any."
  (when (region-active-p)
    (let* ((str (buffer-substring (mark) (point)))
             (start 0)
             (end (length str)))
        (set-text-properties start end nil str)
        str)))

(defun acme-mode--update-argtext ()
  "Update `acme-mode--argtext' with selected region, if any."
  (if (region-active-p)
      (setq acme-mode--argtext (acme-mode--get-active-region-text))
    (setq acme-mode--argtext nil)))

(defun acme-mode--make-mouse-event (type)
  "Generate a mouse event of given TYPE at point.

For example,

  (acme-mode--make-mouse-event \\='double-down-mouse-1)

creates an event similar to that generated when the left mouse
is pressed twice and using last tracked mouse event position."
  ;; Kludge so can keep repeating cut and paste chords with keyboard,
  ;; else we end up progressively undo-ing instead of back and forth
  (when (eq last-command 'undo)
    (setq last-command 'left-char))
  (cond ((= mouse-selection-click-count 0)
         (let* ((mousepos (mouse-pixel-position))
                (frame (car mousepos))
                (x (cadr mousepos))
                (y (cddr mousepos))
                (posn (posn-at-x-y x y frame)))
           (list type posn)))
        (acme-mode--last-mouse-event
         (list type (nth 1 acme-mode--last-mouse-event)))
        (t
         (error "No last mouse event but `mouse-selection-click' greater than zero"))))

(defun acme-mode--make-keyboard-chord-transient-map ()
  "Set a transient key map to support `acme-mode-keyboard-chord-keylist'."
  (when acme-mode-keyboard-chord-keylist
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (dolist (elt acme-mode-keyboard-chord-keylist)
         (let ((key (car elt))
               (but (cdr elt)))
           (cond ((eql but 1)
                  (define-key map (kbd key)
                    (lambda () (interactive)
                      (acme-mode--down-mouse-1 (acme-mode--make-mouse-event 'down-mouse-1))
                      (acme-mode--mouse-1 (acme-mode--make-mouse-event 'mouse-1)))))
                 ((eql but 2)
                  (define-key map (kbd key)
                    (lambda () (interactive)
                      (acme-mode--down-mouse-2 (acme-mode--make-mouse-event 'down-mouse-2))
                      (acme-mode--mouse-2 (acme-mode--make-mouse-event 'mouse-2))
                      ;; Kludge to work around 3-2 chords sometimes sending only
                      ;; down-mouse-3 down-mouse-2 mouse-2 (no final mouse-3)
                      (when (eq acme-mode--state 'donothing)
                        (acme-mode--button-up acme-mode--rbutton)
                        (acme-mode--maybe-reset-state)))))
                 ((eql but 3)
                  (define-key map (kbd key)
                    (lambda (arg) (interactive "P")
                      (acme-mode--down-mouse-3 (acme-mode--make-mouse-event 'down-mouse-3) arg)
                      (acme-mode--mouse-3 (acme-mode--make-mouse-event 'mouse-3))
                      ;; Kludge to work around 2-3 chords sometimes sending only
                      ;; down-mouse-2 down-mouse-3 mouse-3 (no final mouse-2)
                      (when (eq acme-mode--state 'donothing)
                        (acme-mode--button-up acme-mode--mbutton)
                        (acme-mode--maybe-reset-state)))))
                 (t
                  (error "Unsupported button number")))))
       map)
     t nil)))

(defun acme-mode--pop-buffer-window (buffer-or-name &optional use-frames size side window)
  "Switch to buffer BUFFER-OR-NAME if visible, and if not open in new window.

If USE-FRAMES is t, switching can be across frames. If it is nil,
then switching is only on the same frame.

When opening BUFFER-OR-NAME in a new window, split a new window
below, switch to it, and switch the displayed buf to that of
BUFFER-OR-NAME.

If a new window is created, if SIZE is positive then the window
used for split will be SIZE lines tall after the split, if SIZE
is negative then split is so the new window is -SIZE lines tall,
and if SIZE is nil then the window split is done evenly. See
`split-window' for more about this parameter.

SIDE controls on which side the new window is opened if in the
same frame, and can be \\='above , \\='below , \\='left , \\='right or nil.
See `split-window' for more about this parameter.

If WINDOW is non-nil, split it instead of the current window if
creating a new window."
  (let* ((win (get-buffer-window buffer-or-name use-frames)))
    (cond (win
           (progn
             (select-window win)
             (raise-frame)
             win))
          (use-frames
           (select-frame (make-frame-command))
           (switch-to-buffer buffer-or-name)
           (raise-frame)
           (selected-window))
          (t
           (let ((new-win (split-window window size side)))
             (select-window new-win)
             (switch-to-buffer buffer-or-name)
             new-win)))))

(defun acme-mode--pop-file-window (file-name &optional use-frames)
  "Switch to window with FILE-NAME if visible, and if not open in new window.

If USE-FRAMES is t, switching can be across frames. If it is nil,
then switching is only on the same frame.

When opening FILE-NAME in a new window, open it in a new buffer
if needed, split a new window below, switch to it, and switch the
displayed buf to that of FILE-NAME.

If FILE-NAME is not an existing file, this creates a buffer with
that name."
  (let ((buffer (find-file-noselect file-name)))
    (acme-mode--pop-buffer-window buffer use-frames)))

(defun acme-mode-pop-tag-buffer (arg)
  "Pops to an Acme mode tag buffer, optionally numbered with prefix ARG.

If the tag buffer is visible in a window, that window is
selected. If the tag buffer is not already visible, a window is
created showing that buffer. If the tag buffer does not exist, it
is created. If `acme-mode-use-frames' is nil, selection and
window creation are within the current frame. If
`acme-mode-use-frames' is t, selection can be across frames and
new windows are created in their own frames.

Tag buffers have name determined by `acme-mode-tag-buffer-name'.
Numbered tag buffers are additionally suffixed by <prefix ARG>.
If prefixed with an ARG that is not an integer, ARG is ignored.

To hide the tag buffer after, right-click on its modeline if it
is in a frame with two windows or more, or close the frame if it
occupies a frame by itself."
  (interactive "P")
  (let* ((buffer-name (concat acme-mode-tag-buffer-name
                              (when (integerp arg)
                                (concat "<" (number-to-string arg) ">"))))
         (maybe-buffer (get-buffer buffer-name))
         (buffer (or maybe-buffer
                     (generate-new-buffer buffer-name))))
    (save-selected-window
      ;; Pop tag buffer in a small window, dedicate it to the tag buffer,
      ;; and preserve its size
      (let* ((tag-height 3)
             (tag-win (acme-mode--pop-buffer-window buffer
                                                    acme-mode-use-frames
                                                    (- tag-height)
                                                    'above
                                                    (frame-root-window))))
        (set-window-dedicated-p tag-win t)
        (set-window-parameter tag-win 'no-other-window t)
        (set-window-parameter tag-win 'window-atom t)
        (window-preserve-size tag-win nil t))
      ;; Insert base tag keywords for new tag buffers
      (unless maybe-buffer
        (with-current-buffer buffer
          (insert acme-mode-initial-tag-line))))))

(defun acme-mode--tag-buffer-window-p (&optional window)
  "Return t if the current window displays a tag buffer, else nil.

If WINDOW is non-null, check that window's buffer instead."
  (if window
      (when (window-live-p window)
        (with-selected-window window
          (string-prefix-p acme-mode-tag-buffer-name (buffer-name))))
    (string-prefix-p acme-mode-tag-buffer-name (buffer-name))))

(defun acme-mode-set-tag-buffer-default-directory-to-current (&optional arg)
  "Set `default-directory' of tag buffer to the current buffer's.

If prefix ARG is an integer, set the `default-directory' of the
specifically numbered tag buffer instead of the generic one.

Generally only needed for Meta-x and Elisp commands, as button 2
text execution when the tag buffer is selected is uses the
context of the last selected non-tag buffer."
  (interactive "P")
  (let* ((directory default-directory)
         (tag-buffer-name (concat acme-mode-tag-buffer-name
                                  (when (integerp arg)
                                    (concat "<" (number-to-string arg) ">"))))
         (tag-buffer (get-buffer tag-buffer-name)))
    (cond (tag-buffer
           (with-current-buffer tag-buffer
             (setq default-directory directory)
             (message (concat "Set default-directory to `"
                              directory
                              "' in tag buffer `"
                              tag-buffer-name
                              "'"))))
          (t
           (message (concat "Tag buffer `"
                            tag-buffer-name
                            "' does not exist"))))))

(defun acme-mode--shell-output-buffer ()
  "Get the output buffer for displaying shell execution output."
  (let ((buffer-name
         (let ((bname (buffer-name))
               (fname (buffer-file-name)))
           (cond ((and acme-mode-per-dir-shell-output fname)
                  (concat acme-mode-shell-output-buffer-name
                          "<" (file-name-directory fname) ">"))
                 ((string-prefix-p acme-mode-shell-output-buffer-name bname)
                  bname)
                 (t
                  acme-mode-shell-output-buffer-name)))))
    (or (get-buffer buffer-name)
        (generate-new-buffer buffer-name))))

(defun acme-mode-pop-shell-output-buffer (&optional buffer)
  "Pop open the shell output buffer in a new window.

Try splitting live windows in order given by `window-list' for
the new window.

If BUFFER is non-nil, it is used directly as the shell output
buffer instead of usual one."
  (let* ((buffer (or buffer (acme-mode--shell-output-buffer)))
         (windows (window-list)))
    (cl-some (lambda (win)
               (condition-case nil
                   (acme-mode--pop-buffer-window
                    buffer nil nil nil win)
                 (error nil)))
             windows)))

(defun acme-mode--header-line-active-p ()
  "Check if there is an active head-line in the window."
  (not (null header-line-format)))

(defun acme-mode--warp-mouse-to-point ()
  "Warp mouse pointer to point in the current window."
  (let* ((coords (posn-col-row (posn-at-point)))
         (window-coords (window-inside-edges))
         (x (+ (car coords) (car window-coords) -1)) ; the fringe is 0
         (y (+ (cdr coords) (cadr window-coords) 1
               (if (acme-mode--header-line-active-p)
                   -1
                 0))))
    (set-mouse-position (selected-frame) x y)))

(defun acme-mode--highlight-search (text)
  "Set the region to current search result for TEXT.

Assumes point is at the end of the result."
  (set-mark (point))
  (search-backward text nil t)
  (exchange-point-and-mark))

(defun acme-mode--search (text &optional no-warp)
  "Search forward for the next occurence of TEXT.

When searching forward, the mouse is warped to the search result
if one exists, unless NO-WARP is non-nil."
  (if (search-forward text nil t)
      (acme-mode--highlight-search text)
    (let ((saved-point (point)))
      (message "Wrapped search")
      (goto-char (point-min))
      (if (search-forward text nil t)
          (acme-mode--highlight-search text)
        (goto-char saved-point))))
  ;; recenter screen if search result is beyond the viewport
  (unless (posn-at-point)
    (universal-argument)
    (recenter))
  ;; warp the mouse to the result
  (unless no-warp
    (acme-mode--warp-mouse-to-point))
  ;; when using Look from the tag buffer, if the next result is in the
  ;; same viewport highlighting may not update if the screen does not
  ;; redraw, so force it to
  (redraw-frame))

(defun acme-mode--find-file (filename &optional no-warp)
  "Find given FILENAME in another window if it exists.

FILENAME may be specified with a linenumber and a column number:
  <filepath>
  <filepath>:<linenum>
  <filepath>:<linenum>:<colnum>

If the given file is successfully opened, the function returns t.
If the given file does not exist, the function returns nil.

If successful and NO-WARP is nil, the mouse pointer is warped to
the file window."
  (let ((filepath)
        (linenum)
        (colnum))
    (save-match-data
      (cond ((string-match "\\([~.a-zA-Z¡-￿0-9_/@-]*[a-zA-Z¡-￿0-9_/-]\\):\\([0-9]+\\)[:.]\\([0-9]+\\)" filename)
             (setq filepath (match-string 1 filename)
                   linenum (string-to-number (match-string 2 filename))
                   colnum (string-to-number (match-string 3 filename))))
            ((string-match "\\([~.a-zA-Z¡-￿0-9_/@-]*[a-zA-Z¡-￿0-9_/-]\\):\\([0-9]+\\)" filename)
             (setq filepath (match-string 1 filename)
                   linenum (string-to-number (match-string 2 filename))))
            ((string-match "\\([~.a-zA-Z¡-￿0-9_/@-]*[a-zA-Z¡-￿0-9_/-]\\)" filename)
             (setq filepath (match-string 1 filename))))
      (when (and filepath
                 (file-readable-p filepath))
        (acme-mode--pop-file-window filepath acme-mode-use-frames)
        (when linenum
          (goto-char (point-min))
          (forward-line (1- linenum)))
        (when colnum
          (forward-char (1- colnum)))
        (unless no-warp
          (acme-mode--warp-mouse-to-point))
        t))))

(defun acme-mode--find-file-or-search (file-or-text)
  "Open FILE-OR-TEXT if a file path, else search forward for next occurrence.

When searching forward, the mouse is warped to the search result
if one exists."
  (or (acme-mode--find-file file-or-text acme-mode-no-warp-mouse)
      (acme-mode--search file-or-text acme-mode-no-warp-mouse)))

(defun acme-mode--get-seltext (event thing)
  "Get text for plumbing based on EVENT, THING, and selections.

THING should be one of the choices from `thing-at-point'.

Priority order is secondary selection if it exists, then selected
text if EVENT position is within the selected text, then the
THING-at-point."
  (let* ((posn (event-end event))
         (bufpos (posn-point posn))
         (win (posn-window posn)))
    (with-selected-window win
      (cond ((secondary-selection-exist-p)
             (gui-get-selection 'SECONDARY))
            ((and (region-active-p)
                  (>= bufpos (min (mark) (point)))
                  (<= bufpos (max (mark) (point))))
             (buffer-substring (mark) (point)))
            (t
             (let* ((restorepointevent (list 'mouse-1 (posn-at-point)))
                    (_ (mouse-set-point event))
                    (seltext (thing-at-point thing)))
               (mouse-set-point restorepointevent)
               seltext))))))

(defun acme-mode--plumb (text)
  "Plumb TEXT."
  (when text
      ;; See https://emacs.stackexchange.com/questions/69743/use-regex-as-key-car-in-alist
      (let ((res (assoc text acme-mode-plumbing-rules 'string-match-p)))
        (if res
            (funcall (cdr res) text)
          (acme-mode--find-file-or-search text)))))

(defun acme-mode--plumb-event (event)
  "Plumb selected text or thing at EVENT position."
  (let ((seltext (acme-mode--get-seltext event 'filename)))
    (acme-mode--clear-secondary-selection)
    (acme-mode--with-last-non-tag-buffer-window
     (acme-mode--plumb seltext))))

(defun acme-mode--pop-new-column ()
  "Pop open a new column. Used for Newcol keyword."
  ;; Assume tag buffer window is always on top and never elsewhere in the frame,
  ;; and that there are no combination windows with only one child
  (let ((root (frame-root-window))
        (win (selected-window))
        (apply-win nil))       ; apply-win will be the window to split
    (while (not apply-win)
      (if (eq win root)
          (setq apply-win win)
        (cond ((window-combined-p win t) ; once we traverse upward to a horizontal combo, split that
               (setq apply-win (window-parent win)))
              ((let* ((next-win (window-parent win))
                      (next-win-child (window-child next-win)))
                 (while (and next-win-child
                             (not (acme-mode--tag-buffer-window-p next-win-child)))
                   (setq next-win-child (window-next-sibling next-win-child)))
                 next-win-child) ; t if parent window has a child that is a tag buffer window
               (setq apply-win win))
              (t
               ;; Else, traverse upward
               (setq win (window-parent win))))))
    (let ((apply-win-tag-p (acme-mode--tag-buffer-window-p apply-win))
          (apply-win-child (window-child apply-win))) ; instantiate a temporary variable for use below
      (cond
       ;; Case 1: apply-win is the root window and also a tag buffer window -> error
       ((and (eq apply-win root)
             apply-win-tag-p)
        (message "Cannot add new column to a frame with only the tag buffer.")
        nil)
       ;; Case 2: apply-win is the root window and has a direct child tag buffer window -> split window after tag buffer window
       ((and (eq apply-win root)
             (progn
               (while (and apply-win-child
                           (not (acme-mode--tag-buffer-window-p apply-win-child)))
                 (setq apply-win-child (window-next-sibling apply-win-child)))
               apply-win-child))
        (setq apply-win (window-next-sibling apply-win-child))
        (if apply-win
            (split-window apply-win nil 'right)
          (message "Tag buffer window has no next sibling.")
          nil))
       ;; Case 3: apply-win is not the root window but is a tag buffer window -> split window after tag buffer
       ((and (not (eq apply-win root))
             apply-win-tag-p)
        (setq apply-win (window-next-sibling apply-win))
        (if apply-win
            (let ((base-win apply-win))
              (while (not (window-live-p base-win))
                (setq base-win (window-child base-win)))
              (with-selected-window base-win
                (split-window apply-win nil 'right)))
          (message "Tag buffer window has no next sibling.")
          nil))
       ;; Case 4: otherwise -> traverse to first non-horizontal combo window and split apply-win there
       (t
        (when (window-combined-p (window-child apply-win) t)
          (setq apply-win (window-child apply-win))
          (let ((next-win (window-next-sibling apply-win)))
            (while next-win
              (setq apply-win next-win)
              (setq next-win (window-next-sibling apply-win)))))
        (split-window apply-win nil 'right))))))

(defun acme-mode--rename-buffer-file (file-name)
  "Rename current buffer and file it is visiting to FILE-NAME.

Return value is non-nil if renaming was done, and nil if not."
  (interactive)
  (let* ((current-buffer-name (buffer-name))
         (current-file-name (buffer-file-name)))
    (cond ((not current-file-name)
           (message "Buffer `%s' is not a file buffer" current-buffer-name)
           nil)
          ((buffer-modified-p)
           (message "Buffer `%s' file is modified, save before renaming" current-buffer-name)
           nil)
          ((file-exists-p file-name)
           (message "Target file `%s' already exists" file-name))
          (t
           (rename-file current-file-name file-name nil)
           (set-visited-file-name file-name)
           (set-buffer-modified-p nil)
           (message "Renamed file to `%s'" (buffer-file-name))
           t))))

(defun acme-mode--execute-command-keyword (command keyword-alist)
  "Execute Acme mode COMMAND keywords in KEYWORD-ALIST.

Used when executing text. If the first word of the text matches a
key (the keyword) in this association list, the corresponding
function is called with the appropriate arity:

* If the text has only one word (i.e., the keyword) then FUNCTION
  is called with no arguments, that is, (FUNCTION) is called.

* If the text has more than one word, then FUNCTION is called
  with the substring of the text after the first word and
  whitespace as an argument, that is (FUNCTION ARG) is called.
  E.g., if the text is \"abc def ghi\" then (abc \"def ghi\") is
  called.

Functions that take no arguments should have form:
  (lambda () body)

Those must take an argument should have form:
  (lambda (arg) body)

And those that take an optional argument should have form:
  (lambda (&optional arg) body)"
  (let* ((keyword (car (split-string command)))
         (res (assoc keyword keyword-alist)))
    (when res
      (let ((func (cdr res)))
        (cond ((string-equal command keyword)
               (if (= (car (func-arity func)) 0)
                   (funcall func)
                 (message "Keyword `%s' function requires an argument." keyword)))
              (t            ; command has more characters than keyword
               (let ((arg (substring command (1+ (length keyword)))))
                 (if (>= (cdr (func-arity func)) 1)
                     (if (> (length arg) 0)
                         (funcall func arg)
                       (message "Keyword `%s' called with empty argument." keyword))
                   (message "Keyword `%s' function does not take an argument." keyword)))))
        ;; Kludge, wait to update so users are guided into
        ;; clicking slower, else events have incorrect position
        (sleep-for 0.2)
        t))))

(defun acme-mode--execute-command-shell (command)
  "Acme mode dispatcher for executing a given COMMAND in an external shell."
  (let ((win (if (acme-mode--tag-buffer-window-p)
                 acme-mode--last-non-tag-buffer-window
               (selected-window))))
    (with-selected-window win
      (let* ((command (string-trim command))
             (seltext (acme-mode--get-active-region-text))
             (end (point))
             (start (if seltext (mark) end))
             (command-type (cond ((string-prefix-p "<" command) 'insert)
                                 ((string-prefix-p "|" command) 'replace)
                                 ((string-prefix-p ">" command) 'pipe)
                                 (t nil)))
             (command (if command-type
                          (string-trim (substring command 1))
                        command))
             ;; Append to instead of overwriting output buffer
             (shell-command-dont-erase-buffer t)
             ;; Make sure shell command output does resize the minibuffer
             (max-mini-window-height 0.01)
             ;; Shell output display buffer
             (disp-buffer (acme-mode--shell-output-buffer)))
        (cond ((eq command-type 'insert)
               (when seltext
                 (let ((left (min start end))
                       (right (max start end)))
                   (setq start left)
                   (setq end right)))
               (delete-region start end)
               (call-process-shell-command command nil t nil))
              ((eq command-type 'replace)
               (call-shell-region start end command t t))
              ((eq command-type 'pipe)
               (unless (get-buffer-window disp-buffer)
                 (let ((win (selected-window)))
                   (acme-mode-pop-shell-output-buffer disp-buffer)
                   (select-window win)))
               (with-current-buffer disp-buffer
                 (goto-char (point-max)))
               (call-shell-region start end command nil disp-buffer)
               (with-current-buffer disp-buffer
                 (goto-char (point-max))))
              (t
               (unless (get-buffer-window disp-buffer)
                 (let ((win (selected-window)))
                   (acme-mode-pop-shell-output-buffer disp-buffer)
                   (select-window win)))
               (with-current-buffer disp-buffer
                 (goto-char (point-max)))
               (call-process-shell-command command nil disp-buffer t)
               (with-current-buffer disp-buffer
                 (goto-char (point-max)))))))))

(defun acme-mode--execute (event &optional arg)
  "Execute selected text or sexp at EVENT posn, with optional ARG."
  (let* ((seltext (acme-mode--get-seltext event 'sexp)) ; sexp to get leading <, | or >
         (command (if arg
                      (string-join (list seltext arg) " ")
                    seltext)))
    (acme-mode--clear-secondary-selection)
    ;; Priority order: user keyword, built-in keyword, external shell
    (acme-mode--with-last-non-tag-buffer-window
     (or (acme-mode--execute-command-keyword command acme-mode-user-command-keywords)
         (acme-mode--execute-command-keyword command acme-mode--builtin-command-keywords)
         (acme-mode--execute-command-shell command)))))

(defun acme-mode--mouse-drag-secondary (start-event)
  "Set the secondary selection to the text that the mouse is dragged over.
Highlight the drag area as you move the mouse.
This must be bound to a button-down mouse event that is START-EVENT.
The function returns a non-nil value if it creates a secondary selection.

This is a duplicated `mouse-drag-secondary' but modified to use
`set-transient-map' like `'mouse-drag-track' (the original
function eats the first non-movement event so a simple mouse 3
press-drag-release will not result in an immediate forward
search), and without the 1-second wait if there is no selected
region (e.g., a click without dragging)."
  (interactive "e")
  (mouse-minibuffer-check start-event)
  (let* ((echo-keystrokes 0)
         (start-posn (event-start start-event))
         (start-point (posn-point start-posn))
         (start-window (posn-window start-posn))
         (start-buffer (window-buffer start-window))
         (bounds (window-edges start-window))
         (top (nth 1 bounds))
         (bottom (if (window-minibuffer-p start-window)
                     (nth 3 bounds)
                   ;; Don't count the mode line.
                   (1- (nth 3 bounds))))
         (click-count (1- (event-click-count start-event)))
         (old-track-mouse track-mouse))
    (with-current-buffer start-buffer
      (setq mouse-secondary-click-count click-count)
      (if (> (mod click-count 3) 0)
          ;; Double or triple press: make an initial selection
          ;; of one word or line.
          (let ((range (mouse-start-end start-point start-point click-count)))
            (set-marker mouse-secondary-start nil)
            (move-overlay mouse-secondary-overlay (car range) (nth 1 range)
                          start-buffer))
        ;; Single-press: cancel any preexisting secondary selection.
        (or mouse-secondary-start
            (setq mouse-secondary-start (make-marker)))
        (set-marker mouse-secondary-start start-point)
        (delete-overlay mouse-secondary-overlay))
      ;; Use a transient map like in `mouse-drag-track'
      (setq track-mouse 'drag-tracking)
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map [switch-frame] #'ignore)
         (define-key map [select-window] #'ignore)
         (define-key map [mouse-movement]
           (lambda (event) (interactive "e")
             (let* ((end (event-end event))
                    (end-point (posn-point end)))
               (cond ((and (eq (posn-window end) start-window)
                           (integer-or-marker-p end-point))
                      (let ((range (mouse-start-end start-point end-point click-count)))
                        (if (or (/= start-point end-point)
                                (null (marker-position mouse-secondary-start)))
                            (progn
                              (set-marker mouse-secondary-start nil)
                              (move-overlay mouse-secondary-overlay
                                            (car range) (nth 1 range)
                                            start-buffer)))))
                     (t
                      (let ((mouse-row (cdr (cdr (mouse-position)))))
                        (cond
                         ((null mouse-row))
                         ((< mouse-row top)
                          (mouse-scroll-subr start-window (- mouse-row top)
                                             mouse-secondary-overlay start-point))
                         ((>= mouse-row bottom)
                          (mouse-scroll-subr start-window (1+ (- mouse-row bottom))
                                             mouse-secondary-overlay start-point)))))))))
         map)
       t
       (lambda ()
         (setq track-mouse old-track-mouse)
         (with-current-buffer start-buffer
           (if (marker-position mouse-secondary-start)
               (progn
                 (delete-overlay mouse-secondary-overlay)
                 (gui-set-selection 'SECONDARY nil)
                 nil)
             (gui-set-selection
              'SECONDARY
              (buffer-substring (overlay-start mouse-secondary-overlay)
                                (overlay-end mouse-secondary-overlay))))))))))

(defun acme-mode--complete-file-name ()
  "File name completion at point.

Used to emulate Plan 9 Acme Control-f file name completion when Acme
mode is enabled."
  (interactive)
  (let ((completion-at-point-functions (list 'comint-filename-completion)))
    (call-interactively 'completion-at-point)))

(defun acme-mode--update-last-non-tag-buffer-window ()
  "Update `acme-mode--last-non-tag-buffer-window' if current buf is not a tag one."
  (let ((win (selected-window)))
    (unless (or (not (window-live-p win))
                (acme-mode--tag-buffer-window-p win)
                (eq acme-mode--last-non-tag-buffer-window win)
                (window-minibuffer-p win))
      (when acme-mode--last-non-tag-buffer-window
        (ring-insert acme-mode--older-non-tag-buffer-window-ring
                     acme-mode--last-non-tag-buffer-window))
      ;; Remove current window from older selected non-tag buffer
      ;; window ring, if present
      (let ((remove-idx
             (cl-some
              (lambda (k)
                (and (eq (ring-ref acme-mode--older-non-tag-buffer-window-ring k) win) k))
              (number-sequence 0 (1- (ring-length acme-mode--older-non-tag-buffer-window-ring))))))
        (when remove-idx
          (ring-remove acme-mode--older-non-tag-buffer-window-ring remove-idx)))
      (setq acme-mode--last-non-tag-buffer-window win))
    (while (and (not (window-live-p acme-mode--last-non-tag-buffer-window))
                (not (ring-empty-p acme-mode--older-non-tag-buffer-window-ring)))
      (setq acme-mode--last-non-tag-buffer-window
            (ring-remove acme-mode--older-non-tag-buffer-window-ring 0)))))

(defun acme-mode--kill-region-or-select-last-insertion ()
  "Kill selected region or select newest inserted text if no region is selected.

Note that the newest inserted text is selected only if it was
the last text operation."
  (interactive)
  (if (region-active-p)
      (kill-region (mark) (point))
    (let ((reg (catch 'exit-loop
                 (dolist (op buffer-undo-list)
                   (when op             ; skip nil entries
                     (if (and (number-or-marker-p (car op))
                              (number-or-marker-p (cdr op)))
                         (throw 'exit-loop op)
                       (throw 'exit-loop nil)))))))
      (when reg
        (set-mark (car reg))
        (goto-char (cdr reg))))))

;; MOUSE FUNCTIONS

;; Button 1 down-press
(defun acme-mode--down-mouse-1 (event)
  "Acme mode handler for left-button press EVENT."
  (interactive "e")
  (cond ((apply 'derived-mode-p acme-mode-exclude-major-modes)
         (let ((acme-mode nil))
           (call-interactively (key-binding (this-command-keys)))))
        (t
         (acme-mode--update-last-mouse-events event)
         (acme-mode--button-down acme-mode--lbutton)
         (cond ((eq acme-mode--state 'noselect)
                (setq acme-mode--state 'textselect)
                (setq acme-mode--textselect-cut-or-paste-first nil)
                (mouse-set-mark event)
                (acme-mode--make-keyboard-chord-transient-map)
                (mouse-drag-region event))
               ((eq acme-mode--state 'textselect2) ; 2-1 chord
                (setq acme-mode--state 'donothing)
                (acme-mode--execute event acme-mode--argtext))))))

;; Button 1 release, no mouse pointer movement since down-mouse-1
(defun acme-mode--mouse-1 (event)
  "Acme mode handler for left-button release EVENT."
  (interactive "e")
  (cond ((member major-mode acme-mode-exclude-major-modes)
         (let ((acme-mode nil))
           (call-interactively (key-binding (this-command-keys)))))
        (t
         (acme-mode--update-last-mouse-events event)
         (acme-mode--button-up acme-mode--lbutton)
         (unwind-protect
             (cond ((eq acme-mode--state 'textselect)
                    (setq deactivate-mark nil)
                    (mouse-set-point event)
                    (unless transient-mark-mode
                      (setq transient-mark-mode (cons 'only t)))
                    (acme-mode--update-argtext)))
           (acme-mode--maybe-reset-state)))))

;; Double button 1 release, no mouse pointer movement since down-mouse-1
(defun acme-mode--double-mouse-1 (event)
  "Acme mode handler for double or triple left-click EVENT."
  (interactive "e")
  (cond ((apply 'derived-mode-p acme-mode-exclude-major-modes)
         (let ((acme-mode nil))
           (call-interactively (key-binding (this-command-keys)))))
        (t
         (acme-mode--update-last-mouse-events event)
         (acme-mode--button-up acme-mode--lbutton)
         (unwind-protect
             (cond ((eq acme-mode--state 'textselect)
                    (setq deactivate-mark nil)
                    (mouse-set-point event)
                    (acme-mode--select-region)
                    (unless transient-mark-mode
                      (setq transient-mark-mode (cons 'only t)))
                    (acme-mode--update-argtext)))
           (acme-mode--maybe-reset-state)))))

;; Button 1 release, mouse pointer moved since down-mouse-1
(defun acme-mode--drag-mouse-1 (event)
  "Acme mode handler for left-button drag release EVENT."
  (interactive "e")
  (cond ((apply 'derived-mode-p acme-mode-exclude-major-modes)
         (let ((acme-mode nil))
           (call-interactively (key-binding (this-command-keys)))))
        (t
         (acme-mode--update-last-mouse-events event)
         (acme-mode--button-up acme-mode--lbutton)
         (unwind-protect
             (cond ((eq acme-mode--state 'textselect)
                    (setq deactivate-mark nil)
                    (mouse-set-region event)
                    (unless transient-mark-mode
                      (setq transient-mark-mode (cons 'only t)))
                    (acme-mode--update-argtext)))
           (acme-mode--maybe-reset-state)))))

;; Button 2 down-press
(defun acme-mode--down-mouse-2 (event)
  "Acme mode handler for middle-button press EVENT."
  (interactive "e")
  (cond ((apply 'derived-mode-p acme-mode-exclude-major-modes)
         (let ((acme-mode nil))
           (call-interactively (key-binding (this-command-keys)))))
        (t
         (acme-mode--update-last-mouse-events event)
         (acme-mode--button-down acme-mode--mbutton)
         (cond ((eq acme-mode--state 'noselect)
                (setq acme-mode--state 'textselect2)
                (acme-mode--make-keyboard-chord-transient-map)
                (acme-mode--mouse-drag-secondary event))
               ((eq acme-mode--state 'textselect)
                (setq acme-mode--state 'textselect-cut)
                (setq acme-mode--textselect-cut-or-paste-first 'cut)
                (mouse-set-point event)
                (acme-mode--select-region)
                (kill-region (mark) (point)))
               ((eq acme-mode--state 'textselect-paste)
                (setq acme-mode--state 'textselect-cut)
                (if (version<= "28" emacs-version)
                    (progn
                      (cond ((eq acme-mode--textselect-cut-or-paste-first 'cut)
                             (call-interactively 'undo-redo))
                            ((eq acme-mode--textselect-cut-or-paste-first 'paste)
                             (call-interactively 'undo-only))))
                  (call-interactively 'undo)))
               ((eq acme-mode--state 'textselect3)
                (acme-mode--clear-secondary-selection)
                (setq acme-mode--state 'donothing))))))

;; Button 2 release, no mouse movement since down-mouse-2
(defun acme-mode--mouse-2 (event)
  "Acme mode handler for middle-button release EVENT."
  (interactive "e")
  (cond ((apply 'derived-mode-p acme-mode-exclude-major-modes)
         (let ((acme-mode nil))
           (call-interactively (key-binding (this-command-keys)))))
        (t
         (acme-mode--update-last-mouse-events event)
         (acme-mode--button-up acme-mode--mbutton)
         (unwind-protect
             (cond ((eq acme-mode--state 'textselect2)
                    (acme-mode--execute event)))
           (acme-mode--maybe-reset-state)))))

;; Button 3 down-press
(defun acme-mode--down-mouse-3 (event arg)
  "Acme mode handler for right-button press EVENT.

Specify a prefix ARG to insert a specific kill ring entry.

For example, '<down-mouse-left> Control-u 3 <down-mouse-right>'
will insert the 3rd most recent entry in the kill ring."
  (interactive "e\nP")
  (cond ((apply 'derived-mode-p acme-mode-exclude-major-modes)
         (let ((acme-mode nil))
           (call-interactively (key-binding (this-command-keys)))))
        (t
         (acme-mode--update-last-mouse-events event)
         (acme-mode--button-down acme-mode--rbutton)
         (cond ((eq acme-mode--state 'noselect)
                (setq acme-mode--state 'textselect3)
                (acme-mode--make-keyboard-chord-transient-map)
                (acme-mode--mouse-drag-secondary event))
               ((eq acme-mode--state 'textselect)
                (setq acme-mode--state 'textselect-paste)
                (setq acme-mode--textselect-cut-or-paste-first 'paste)
                (mouse-set-point event)
                (acme-mode--select-region)
                (delete-region (mark) (point))
                (yank arg)
                (setq deactivate-mark nil)
                (activate-mark))
               ((eq acme-mode--state 'textselect-cut)
                (setq acme-mode--state 'textselect-paste)
                (if (version<= "28" emacs-version)
                    (cond ((eq acme-mode--textselect-cut-or-paste-first 'cut)
                           (call-interactively 'undo-only))
                          ((eq acme-mode--textselect-cut-or-paste-first 'paste)
                           (call-interactively 'undo-redo)))
                  (call-interactively 'undo))
                (set-mark acme-mode--region-start)
                (goto-char acme-mode--region-end))
               ((eq acme-mode--state 'textselect2)
                (acme-mode--clear-secondary-selection)
                (setq acme-mode--state 'donothing))))))

;; Button 3 release
(defun acme-mode--mouse-3 (event)
  "Acme mode handler for right-button release EVENT."
  (interactive "e")
  (cond ((apply 'derived-mode-p acme-mode-exclude-major-modes)
         (let ((acme-mode nil))
           (call-interactively (key-binding (this-command-keys)))))
        (t
         (acme-mode--update-last-mouse-events event)
         (acme-mode--button-up acme-mode--rbutton)
         (unwind-protect
             (cond ((eq acme-mode--state 'textselect3)
                    (acme-mode--plumb-event event)))
           (acme-mode--maybe-reset-state)))))

;; PLUMBING FUNCTIONS

(defun acme-mode--plumb-file-system-open (filename)
  "Function to generically open given FILENAME using system \\='xdg-open\\=' or \\='open\\='."
  (let ((system-open-command (or (executable-find "xdg-open")
                                 (executable-find "open"))))
    (if system-open-command
        (start-process "default-app" nil system-open-command filename)
      (message "No xdg-open or open on the system to generically open file."))))

(defun acme-mode--plumb-python-error (error-line)
  "Function to plumb a Python ERROR-LINE.

Specifically, this parses lines like
  File \"somefile.py\", line 5
and opens the relevant file at the appropriate line in a new window."
  (save-match-data
    (and (string-match " *File \"\\([~a-zA-Z¡-￿0-9_./-]+\\)\", line \\([0-9]+\\).*" error-line)
         (acme-mode--find-file
          (concat (match-string 1 error-line)
                  ":"
                  (match-string 2 error-line))))))

(provide 'acme)

;;; acme.el ends here
