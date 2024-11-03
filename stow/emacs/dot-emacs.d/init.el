;;; init.el --- Emacs init file -*- lexical-binding: t; -*-

;; Author: matheuristic
;; URL: https://github.com/matheuristic/emacs-config
;; Generated: Sat Nov  2 20:59:36 2024

;;; Commentary:

;; Emacs initialization configuration file, symlink or copy to
;; ~/.emacs.d/init.el or $XDG_CONFIG_HOME/.emacs.d/init.el

;; In Emacs 27+, the sequence of initialization is
;; 1. early-init.el
;; 2. package.el
;; 3. init.el

;;; Code:

;; Customize file and local configuration

;; store Customize settings in a separate file, custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; load local init configuration and Customize settings on startup
(add-hook 'after-init-hook
          (lambda ()
            (require 'init-local nil t) ; don't raise errors
            (load custom-file 'noerror))
          10) ; load this after regular `after-init-hook' functions

;; Custom variables and utility functions / Custom variables

(defcustom my-system-open-command "xdg-open"
  "System command to open file/URL according to preferred app by filetype.
Usually \"xdg-open\" on Linux and \"open\" on Mac."
  :type 'string
  :group 'convenience)

(defcustom my-mode-lighter-abbrev-alist '(;; Minor modes
                                          (abbrev-mode . "")
                                          (auto-revert-mode . " ⤒")
                                          (buffer-face-mode . "")
                                          (eldoc-mode . "")
                                          (paredit-mode . " ⁽⁾")
                                          (too-long-lines-mode . " ⋯")
                                          ;; (tree-sitter-mode . " ¥")
                                          (visual-line-mode . " ⇌")
                                          ;; Major modes
                                          ;; (lisp-interaction-mode . "λ")
                                          ;; (hi-lock-mode . "")
                                          ;; (python-mode . "Py")
                                          ;; (nxhtml-mode . "nx")
                                          (emacs-lisp-mode . "ELisp"))
  "Alist for `my-abbrev-mode-line' containing mode line lighter abbreviations.

Each entry should be a cons cell (a . b) where a is the minor or
major mode symbol and b is the string to be used as the
abbreviated mode lighter in the mode line (can be an empty string).

Abbreviations for minor modes should typically be prefixed by a
space to make them easier to distinguish, but there is no need to
do so for major mode abbreviations. Use an empty string as an
abbreviation to not show a lighter for a mode.")

(defun my-mode-line-lighter-abbrev ()
  "Abbreviate mode line major and minor mode lighters.

Configure `my-mode-lighter-abbrev-alist' to determine which mode
lighters are abbreviated and what they are abbreviated to."
  (interactive)
  (dolist (abbr my-mode-lighter-abbrev-alist)
    (let* ((mode (car abbr))
           (mode-str (cdr abbr))
           (is-minor-mode (member mode minor-mode-list))
           (mode-str-old (cdr (assq mode minor-mode-alist))))
      (if is-minor-mode
          (let ((minor-mode-alist-entry (assq mode minor-mode-alist)))
            ;; if entry for minor mode exists in `minor-mode-alist'
            ;; modify its value, otherwise append a new entry
            (if minor-mode-alist-entry
                (setcdr minor-mode-alist-entry (list mode-str))
              (add-to-list 'minor-mode-alist
                           (cons mode (list mode-str))
                           t)))
        (when (eq mode major-mode)
          (setq mode-name mode-str))))))

;; rerun on major mode changes
(add-hook 'after-change-major-mode-hook #'my-mode-line-lighter-abbrev)

;; Custom variables and utility functions / Utility functions

(defun my-after-jump-context-actions (&rest _)
  "Useful context actions to perform after jumping to a new location.
This is meant for use with `advice-add' with the :after
combinator.

One useful context action example is to run `org-show-context'
after jumping to an Org buffer location to ensure the region
around the new point location is visible."
  (cond ((eq major-mode 'org-mode) (org-show-context))))

(defun my-persist-variables-to-file (varlist filename)
  "Persist variables in VARLIST to a file FILENAME."
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (dolist (var varlist)
        (print (list 'setq var (list 'quote (symbol-value var)))
               buf))
      (save-buffer)
      (kill-buffer))))

(defun my-frame-monitor-dpi (&optional frame)
  "Get the pixel density in dots per inch (DPI) for the screen containing FRAME.
If FRAME is nil, use the current frame.

DPI (or really points per inch, PPI) is computed with the formula
  PPI = diag_in_pixels / diag_in_inches
where
  diag_in_pixels = sqrt(width_in_pixels**2 + height_in_pixels**2)
  diag_in_inches = sqrt(width_in_mm**2 + height_in_mm**2) / inch_in_mm
  inch_in_mm = 25.4

See https://en.wikipedia.org/wiki/Pixel_density for more details."
  (let* ((attrs (frame-monitor-attributes frame))
         (geom (assoc 'geometry attrs))
         ;; diagonal in pixels
         (width-pixels (nth 3 geom))
         (height-pixels (nth 4 geom))
         (diag-pixels (sqrt (+ (* width-pixels width-pixels)
                               (* height-pixels height-pixels))))
         ;; diagonal in inches
         (scrn (assoc 'mm-size attrs))
         (width-mm (nth 1 scrn))
         (height-mm (nth 2 scrn))
         (diag-mm (sqrt (+ (* width-mm width-mm)
                           (* height-mm height-mm))))
         (diag-inches (/ diag-mm 25.4))) ; 25.4mm per inch
    ;; dpi (or ppi) = diagonal in pixels / diagonal in inches
    (/ diag-pixels diag-inches)))

;; helper function for pulsing the current line, adapted from
;; https://protesilaos.com/dotemacs/#h:6bbc41d6-da7c-4301-84c6-c5887c29283f
(defun my-pulse-line (&rest _)
    "Pulse the current line .
If the point is at the newline at the end of the buffer, pulse
the line before that. Additionally, the current line is not pulsed
if the point is in the minibuffer."
    (unless (minibufferp)
      (let ((start (if (and (eobp)
                            (= (point) (line-beginning-position)))
                       (line-beginning-position 0)
                     (line-beginning-position)))
            (end (line-beginning-position 2)))
        (pulse-momentary-highlight-region start end nil))))

(defun my-save-and-bury-buffer (&rest _)
  "Save and bury the current buffer."
  (interactive)
  (save-buffer)
  (bury-buffer))

;; hacky workaround to install ELPA/MELPA version of a package
;; adapated from https://github.com/jwiegley/use-package/issues/319
(defun my-install-elpa-package (pkg-symb)
  "Install the ELPA-compatible repository version of package PKG-SYMB.
Useful for working around `use-package' behavior of not
installing the repository version of a package when a built-in
version is present (even if pinned to a specific repository)."
  (let ((pkg-pattern (concat package-user-dir
                             "/" (symbol-name pkg-symb) "-[0-9]*")))
    (unless (file-expand-wildcards pkg-pattern)
      (package-install (elt (cdr (assoc pkg-symb
                                        package-archive-contents))
                            0)))))

;; use FONT-FAMILY-NAME for buffer-face-mode and enable the mode
(defun my-set-buffer-face-mode-font-family (font-family-name)
  "Configure `buffer-face-mode' to use FONT-FAMILY-NAME and enable it."
  (if (find-font (font-spec :family font-family-name))
      (progn
        (setq-local buffer-face-mode-face `(:family ,font-family-name))
        (buffer-face-mode))
    (message "Skipping `buffer-face-mode' changes: cannot find font-family %s."
             font-family-name)))

;; Package management

;; set ELPA-compatible package repositories and their priorities
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
      package-archive-priorities '(("gnu"   . 1)
                                   ("nongnu" . 2)))

;; initialize package.el
(require 'package)
(package-initialize)

;; bootstrap use-package, provides configuration macros
;; for info, see https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; preload use-package and bind-key packages
;; configure imenu support for the `require' and `use-package' keywords
(eval-when-compile
  (setq use-package-enable-imenu-support t)
  (require 'use-package)
  (require 'bind-key)
  (setq use-package-always-ensure t)) ;; default to ":ensure t"

;; gather use-package stats, "M-x use-package-report" to see report
(setq use-package-compute-statistics t)

;; convenience function to reinstall and reload an Emacs package
(require 'cl-macs)
(require 'seq)
(defun my-package-reinstall (pkg)
  "Prompts for an installed package PKG and reinstalls it.

All loaded features that correspond to Elisp filenames in the
package install directory (but not its subdirectories) are
unloaded, the package reinstalled, and the previously unloaded
features are reloaded."
  (interactive (list (intern (completing-read
                              "Reinstall package: "
                              (mapcar #'car package-alist)))))
  (let* ((pkg-desc (car (alist-get pkg package-alist)))
         (pkg-dir (file-name-as-directory
                   (cl-struct-slot-value 'package-desc 'dir pkg-desc)))
         (pkg-files (directory-files pkg-dir nil "\\.el$"))
         (pkg-features (mapcar
                        (lambda (fname)
                          (intern (file-name-sans-extension fname)))
                        pkg-files))
         (reload-features (seq-filter 'featurep pkg-features)))
    (dolist (feat reload-features)
      (ignore-errors ; handle when pkg is a dependency of another package
        (unload-feature feat t)))
    (package-reinstall pkg)
    (dolist (feat reload-features)
      (require feat))))

;; Environment variables

;; set `exec-path' and other environment variables
(dolist (maybe-dir (mapcar
		    #'expand-file-name
		    '("~/.local/bin/"
		      "~/go/bin/"
		      "~/racket/bin/"
		      "~/macports/bin/"
		      "~/spack/var/spack/environments/default/.spack-env/view/bin/")))
  (when (file-directory-p maybe-dir)
    (add-to-list 'exec-path maybe-dir)
    (setenv "PATH" (concat maybe-dir ":" (getenv "PATH")))))
(when (executable-find "emacsclient")
  (setenv "EDITOR" "emacsclient"))
(let ((maybe-dir (expand-file-name "~/.config/enchant/")))
  (when (file-directory-p maybe-dir)
    (setenv "DICPATH" maybe-dir)))
;; on macOS, set path to Perl libraries installed by XCode command-line tools
(let ((maybe-dir "/Library/Developer/CommandLineTools/usr/share/git-core/perl"))
  (when (file-directory-p maybe-dir)
    (setenv "PERL5LIB" (concat maybe-dir ":" (getenv "PERL5LIB")))))

;; Backend and frontend frameworks for building user interfaces

;; enable flex completion, requires Emacs 27+
(with-eval-after-load 'minibuffer
  (add-to-list 'completion-styles 'flex t))

;; edit regions in separate buffers, used by other packages like markdown-mode
(use-package edit-indirect)

;; tree-sitter incremental parser
(require 'treesit nil t)

;; Visual (part 1)

;; Backups

;; backup files to ~/.backup/
(let ((backup-dir (expand-file-name "~/.backup/")))
  (when (not (file-directory-p backup-dir))
    (make-directory backup-dir t))
  (setq backup-directory-alist `(("." . ,backup-dir))
        version-control t ;; use version numbers for backups
        kept-new-versions 3 ;; number of newest versions to keep
        kept-old-versions 0 ;; number of oldest versions to keep
        delete-old-versions t ;; don't ask before deleting old backups
        backup-by-copying t)) ;; backup by copying instead of renaming

;; Bookmarks and history

;; recently opened files
(setq recentf-max-menu-items 10
      recentf-max-saved-items 100
      recentf-auto-cleanup 'mode) ;; clean up recent list when turning on mode
(recentf-mode 1)
;; exclude compressed files
(add-to-list 'recentf-exclude ".gz")
(add-to-list 'recentf-exclude ".xz")
(add-to-list 'recentf-exclude ".zip")
;; exclude source code files in installed packages from ELPA-compatible repos
(add-to-list 'recentf-exclude
             (concat "^" (expand-file-name "elpa/" user-emacs-directory)))
;; exclude files opened with SSH so TRAMP is not spammed with stat calls
;; exclude files opened as the superuser with su or sudo
(add-to-list 'recentf-exclude "^/\\(?:scp\\|ssh\\|su\\|sudo\\)?:")
;; exclude files from /var/folder as these are temp files
(add-to-list 'recentf-exclude "^/var/folders")
;; exclude files in `org-agenda-files'
;; these files are quickly accessible from their respective tooling
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (dolist (file-list (list org-agenda-files))
;;               (dolist (exclude-file file-list)
;;                 (add-to-list 'recentf-exclude
;;                              (concat "^" exclude-file))))))

;; binding for recentf
(global-set-key (kbd "C-c f") #'recentf-open-files)

;; select file to open from `recentf-list' using `completing-read'
(defun my-recentf-find-file ()
  "Use `completing-read' to find a recent file."
  (interactive)
  (find-file (completing-read "Find recent file: " recentf-list)))

;; binding for `my-recentf-open-files' when in recentf dialog buffers
(define-key recentf-dialog-mode-map (kbd "f") #'my-recentf-find-file)

(save-place-mode 1)

;; save minibuffer and other history across sessions
;; don't persist kill-ring if in the habit of copy-pasting passwords
(setq history-delete-duplicates t
      history-length 100
      ;; if `desktop-save-mode' is enabled, it saves `register-alist'
      ;; and `search-ring' by default so it is unnecessary to add
      ;; those to `savehist-additional-variables'
      savehist-additional-variables '(Info-history-list
                                      ;; kill-ring
                                      kmacro-ring
                                      regexp-search-ring
                                      ;; register-alist
                                      last-kbd-macro
                                      ;; search-ring
                                      shell-command-history))

;; enable save history mode
(savehist-mode 1)

;; Buffers, windows, frames, workspaces / Buffer management

;; protect these buffers, locking them to make them unkillable
(dolist (buf '("*scratch*" "*Messages*"))
  (with-current-buffer buf
    (emacs-lock-mode 'kill)))

;; advanced buffer management with Ibuffer
(setq ibuffer-expert t ; skip extraneous confirm messages
      ibuffer-show-empty-filter-groups nil)

(global-set-key (kbd "C-x C-b") #'ibuffer)

;; configure Ibuffer filter groups
(with-eval-after-load 'ibuffer
  ;;   (defun my-ibuffer-org-agenda-files-filter ()
  ;;     "Ibuffer filter for checking if current buffer is an Org agenda file.

  ;; Specifically, the current buffer is checked to see if it is in
  ;; `org-agenda-files', is the agenda inbox file
  ;; `my-org-agenda-inbox', or is the someday inbox file
  ;; `my-org-someday-inbox'."
  ;;     (let* ((bufname (buffer-file-name))
  ;;            (fname (and bufname (file-truename bufname))) ; filename if a file buffer, nil otherwise
  ;;            (agenda-fnames (mapcar #'file-truename (append (org-agenda-files) ; agenda and inbox filenames
  ;;                                                           (list my-org-agenda-inbox
  ;;                                                                 my-org-someday-inbox)))))
  ;;       (and fname
  ;;            (member fname agenda-fnames))))
  (setq ibuffer-saved-filter-groups
        ;; files are grouped by the first matching filter group in the list
        '(("default"
           ("Emacs" (or (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")))
           ("Calendar" (or (name . "^\\*?[Cc]alendar.*$")
                           (name . "^diary$")))
           ("DocView" (mode . doc-view-mode))
           ("Images" (mode . image-mode))
           ("Web" (or (mode . eww-mode)
                      (mode . eww-bookmark-mode)))
           ("Shell" (or (mode . eshell-mode)
                        (mode . shell-mode)
                        (mode . term-mode)
                        (mode . eat-mode)))
           ("Data" (or (mode . csv-mode)
                       (mode . json-mode)
                       (mode . nxml-mode)))
           ;; ("Analytics" (or (mode . ess-r-mode)
           ;;                  (mode . inferior-ess-r-mode)))
           ("Programming" (derived-mode . prog-mode))
           ;; ("Agenda" (or (mode . org-agenda-mode)
           ;;               (predicate . (my-ibuffer-org-agenda-files-filter))))
           ("Org" (derived-mode . org-mode))
           ("Text" (derived-mode . text-mode))
           ("Fundamental" (mode . fundamental-mode))
           ("Dired" (mode . dired-mode))
           ("Magit" (derived-mode . magit-mode))
           ("Help" (or (derived-mode . apropos-mode)
                       (derived-mode . help-mode)
                       (derived-mode . Info-mode))))))
  (defun my-ibuffer-filter-groups-setup ()
    "Custom configuration to load when a new Ibuffer buffer gets created."
    ;; use "default" saved filter groups list by default
    (ibuffer-switch-to-saved-filter-groups "default"))
  (add-hook 'ibuffer-mode-hook #'my-ibuffer-filter-groups-setup))

(define-minor-mode revert-without-query-mode
  "Minor mode for adding/removing current file to/from `revert-without-query'.

Enabling the minor mode adds the file to `revert-without-query'.

Disabling the minor mode removes the file from `revert-without-query'.

This minor mode has no effect when the buffer is not visiting a file."
  :init-value nil
  :lighter " 🅠"
  :keymap nil
  ;; match filename from `find-file-noselect'
  (let ((fname (abbreviate-file-name (expand-file-name
                                      (buffer-file-name)))))
    (if buffer-file-name
        (if (symbol-value revert-without-query-mode)
            (progn
              (setq revert-without-query (add-to-list 'revert-without-query fname))
              (message "Buffer revert without query ON."))
          (setq revert-without-query (remove fname revert-without-query))
          (message "Buffer revert without query OFF."))
      (message "Current buffer is NOT visiting a file."))))

;; Buffers, windows, frames, workspaces / Window management

;; traverse window config changes, C-c left/right to undo/redo
;; uncomment to not bind C-c left/right keys by default
;; (setq winner-dont-bind-my-keys t)
;; enable winner-mode at end of initialization
(add-hook 'after-init-hook #'winner-mode)

;;; more convenient bindings for `other-window' and `other-frame'
(global-set-key (kbd "M-o") #'other-window)
(with-eval-after-load 'term
  (define-key term-raw-map (kbd "M-o") (global-key-binding (kbd "M-o"))))

(defun my-rotate-window-buffers (rotations)
  "Rotate buffers in the windows of the current frame ROTATIONS times.
ROTATIONS can be negative, which rotates in the opposite direction."
  (interactive "P")
  (let* (;; windows that do not contain transient buffers
         (windows (seq-filter (lambda (w)
                                (not
                                 (string= (buffer-name
                                           (window-buffer w))
                                          transient--buffer-name)))
                              (window-list)))
         (num-windows (length windows)))
    (if (not (> num-windows 1))
        (message "Only one window in the frame. Nothing to rotate.")
      (let* (;; original window order properties
             (window-props (mapcar (lambda (w)
                                     `(:buffer ,(window-buffer w)
                                       :start ,(window-start w)
                                       :point ,(window-point w)))
                                   windows))
             ;; new window order after rotation
             (window-moves (mapcar
                            (lambda (k)
                              (elt windows (mod (+ k rotations)
                                                num-windows)))
                            (number-sequence 0 (1- num-windows))))
             ;; create alist for easier looping later
             (wins-props (cl-mapcar #'cons window-moves window-props)))
        ;; iteratively assign orig window props in new window order
        (dolist (w-p wins-props)
          (let ((win (car w-p))
                (prop (cdr w-p)))
            (set-window-buffer-start-and-point
             win
             (plist-get prop :buffer)
             (plist-get prop :start)
             (plist-get prop :point))))))))

(defun my-rotate-buffers-forward ()
  "Rotate buffers in current frame's windows forward."
  (interactive)
  (my-rotate-window-buffers 1))
(defun my-rotate-buffers-backward ()
  "Rotate buffers in current frame's windows backward."
  (interactive)
  (my-rotate-window-buffers -1))

;; bind "C-x 4 [" and "C-x 4 ]" to rotation of window buffers
(global-set-key (kbd "C-x 4 [") #'my-rotate-buffers-backward)
(global-set-key (kbd "C-x 4 ]") #'my-rotate-buffers-forward)

;; automatically focus on help windows when they are opened
(setq help-window-select t)

;; Buffers, windows, frames, workspaces / Frame management

;; resize frames by pixel instead of by character
(setq frame-resize-pixelwise t)

(require 'transpose-frame) ; in site-lisp
(global-set-key (kbd "C-x 5 [") #'rotate-frame-anticlockwise)
(global-set-key (kbd "C-x 5 ]") #'rotate-frame-clockwise)

;; more convenient bindings for `other-frame'
(global-set-key (kbd "M-O") #'other-frame)

;; Buffers, windows, frames, workspaces / Workspace management

;; settings for desktop.el
;; desktops are saved to ~/.emacs.d/.emacs.desktop
;; and locks are saved to ~/.emacs.d/.emacs.desktop.lock
;; - enable desktop-save-mode to save on exit and load on entry;
;;   this is added to `after-init-hook' to avoid a prompt on startup
;;   warning about the desktop file being in use that occurs when
;;   `desktop-save-mode' is enabled before initialization is done,
;;   even though the Emacs process PID is the owner of the lock file;
;;   might be specific to emacs-mac port
;; - set `desktop-autosave-timeout' to nil to disable timer auto-saves
;; - restore frames to their original displays
;; - re-use existing frames
(setq desktop-auto-save-timeout nil
      desktop-restore-in-current-display nil
      desktop-restore-reuses-frames t
      desktop-files-not-to-save (concat "\\("
                                        (mapconcat
                                         'identity
                                         '("\\`/[^/:]*:"
                                           "(ftp)\\'"
                                           "\\.log"
                                           "\\.gz")
                                         "\\|"
                                         )
                                        "\\)"))
(add-hook 'after-init-hook
          (lambda ()
            (desktop-save-mode 1)
            (desktop-read))
          50) ; load after all other `after-init-hook' functions

;; multiple window configs in a frame, most commands have a "C-x t"
;; prefix, and C-TAB and S-C-TAB to switch to next and previous tabs
;; respectively, requires Emacs 27+
(setq tab-bar-show t ; always show tab bar, set to 1 to hide bar when there's only 1 tab
      tab-bar-close-last-tab-choice 'delete-frame ; deleting the last tab deletes the frame
      tab-bar-select-tab-modifiers '(super) ; super-<num> goes to tab<num>
      tab-bar-tab-hints t ; show tab numbers
      tab-bar-tab-name-truncated-max 20 ; truncate name if exceed 20 chars
      tab-bar-new-button-show t ; show new button
      tab-bar-close-button-show t) ; show close button
;; use text instead of default XPM image (better for high DPI) for
;; new and close buttons for tab bar entries, see
;; https://debbugs.gnu.org/db/51/51648.html
;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2021-11/msg00529.html
(add-hook 'tab-bar-mode-hook
          (lambda ()
            (setq tab-bar-new-button (propertize " ⧉ "
                                                 :help "Click to add tab")
                  tab-bar-close-button (propertize " ⮾"
                                                   'close-tab t
                                                   :help "Click to close tab"))))
(with-eval-after-load 'tab-bar
  (with-eval-after-load 'org
    (define-key org-mode-map [C-tab] #'tab-next)))
(tab-bar-mode 1) ; enable mode

;; Command-line interaction

(setq eshell-history-size 1024)

;; enable Eshell to spawn visual commands inside
(require 'em-term)
;; run visual commands and subcommands in term sessions
(dolist (cmd '("htop" "lftp" "ssh" "vi" "vim" "watch"))
  (add-to-list 'eshell-visual-commands cmd))
(dolist (subcmd '(("tail" "-f" "-F")
                  ("sudo" "vi" "vim")
                  ("vagrant" "ssh")))
  (add-to-list 'eshell-visual-subcommands subcmd))

;; ensure Git does not launch a pager for easier usage with eshell
(setenv "GIT_PAGER" "")

;; adapted from https://arte.ebrahimi.org/blog/named-eshell-buffers
(defun my-eshell-with-name ()
  "Prompts for the name of a eshell buffer to open or switch to.
If the NAME given at the prompt is not an existing eshell buffer,
a new one named *eshell*<NAME> will be opened. If no name is
provided, the default interactive `eshell' command is run."
  (interactive)
  (let* ((my-es-bufs (seq-filter
                      (lambda (buf)
                        (string-match-p "*eshell*" (buffer-name buf)))
                      (buffer-list)))
         (my-es-buf-name-list (mapcar #'buffer-name my-es-bufs))
         (my-es-buf-name (completing-read
                          "Eshell Buffer : " my-es-buf-name-list)))
    (if (member my-es-buf-name (mapcar #'buffer-name (buffer-list)))
        (switch-to-buffer my-es-buf-name)
      (if (string= "" my-es-buf-name)
          (eshell)
        (progn
          (eshell 42)
          (rename-buffer (concat "*eshell*<" my-es-buf-name ">")))))))

(defun my-eshell-send (command)
  "Select an Eshell buffer and execute COMMAND."
  (interactive)
  (let* ((my-es-bufs (seq-filter
                      (lambda (buf)
                        (string-match-p "*eshell*" (buffer-name buf)))
                      (buffer-list)))
         (my-es-buf-name-list (mapcar #'buffer-name my-es-bufs)))
    (if my-es-buf-name-list
        (let* ((my-es-buf-name (completing-read
                                (concat "Send to Eshell buffer ("
                                        (car my-es-buf-name-list)
                                        ") : ")
                                my-es-buf-name-list nil t
                                nil nil my-es-buf-name-list)))
          (with-current-buffer my-es-buf-name
            (goto-char (point-max))
            (insert command)
            (eshell-send-input)))
      (message "No Eshell buffers"))))

(defun my-eshell-send-region ()
  "Select an Eshell buffer and execute the current region."
  (interactive)
  (cond
   ((use-region-p)
    (my-eshell-send (buffer-substring-no-properties
                     (region-beginning)
                     (region-end))))
   (t (message "No region selected"))))

(defun my-eshell-send-line ()
  "Select an Eshell buffer and execute the current line."
  (interactive)
  (my-eshell-send
   (buffer-substring-no-properties (point-at-bol) (point-at-eol))))

;; make shell prompts read-only
(setq comint-prompt-read-only t)

;; kill term buffers with 'q' after session end
(defun term-handle-exit--close-buffer-on-cmd (&rest args)
  "Kill term buffer with 'q' after session exit."
  (when (null (get-buffer-process (current-buffer)))
    (use-local-map (let ((keymap (make-sparse-keymap)))
                     (define-key keymap (kbd "q")
                       (lambda ()
                         (interactive)
                         (kill-buffer (current-buffer))))
                     keymap))))
(advice-add 'term-handle-exit :after #'term-handle-exit--close-buffer-on-cmd)

;;; pass through M-x and M-: keys in term/ansi-term inferior buffers
(with-eval-after-load 'term
  (dolist (keyseq (list (kbd "M-x")
                        (kbd "M-:")))
    (define-key term-raw-map keyseq (global-key-binding keyseq))))

;; convenience functions for sent commands to an active tmux session
;; adapted from https://explog.in/notes/tmux.html

(defun tmux-send (command)
  "Execute COMMAND in the most recently active tmux pane."
  (when command
    (call-process "tmux" nil nil nil "send-keys" command "Enter")))

(defun tmux-send-region ()
  "Execute region in the most recently active tmux pane."
  (interactive)
  (cond
   ((use-region-p)
    (tmux-send (buffer-substring-no-properties (region-beginning) (region-end))))
   (t (message "No region selected"))))

(defun tmux-send-line ()
  "Execute line in the most recently active tmux pane."
  (interactive)
  (tmux-send (buffer-substring-no-properties (point-at-bol) (point-at-eol))))

(use-package eat
  :init
  (setq eat-term-name "xterm-256color") ; workaround for forward blank when backspacing
  :config
  (add-hook 'eshell-load-hook 'eat-eshell-visual-command-mode))

;; Comparison tools

;; always set up Ediff control window in the same frame as the diff,
;; open with horizontal window split instead of the default vertical
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; copy diff hunk from buffers A and B to C in 3-way Ediff
;; adapted from https://stackoverflow.com/a/29757750
(defun ediff-copy-A-and-B-to-C (arg)
  "Copies ARGth diff region from both buffers A and B to C.
ARG is a prefix argument.  If nil, copy the current difference region."
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (if (eq arg '-) (setq arg -1)) ;; translate neg arg to -1
  (if (numberp arg) (ediff-jump-to-difference arg))
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference
                                               'A
                                               ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference
                                               'B
                                               ediff-control-buffer)))
  ;; recenter with rehighlighting, but no messages
  (ediff-recenter))
(add-hook 'ediff-keymap-setup-hook
          (lambda ()
            (when ediff-3way-job
              (define-key ediff-mode-map "d" 'ediff-copy-A-and-B-to-C))))
(with-eval-after-load 'ediff-help
  (setq ediff-long-help-message-compare3
        (concat ediff-long-help-message-compare3
                "                                                 |"
                "  d -copy A + B regions to C
"
)))

;; view and compare directory trees, like Beyond Compare
(use-package ztree
  :pin gnu
  :bind (("C-x D" . ztree-dir)
         ("C-c d z" . ztree-diff))
  :config
  (setq ztree-dir-move-focus t ;; RET in ztree-dir also moves focus
        ztree-draw-unicode-lines t ;; unicode lines
        ztree-show-number-of-children t)) ;; show number of files in subdir tree

;; convenience navigation bindings for `ztreedir-mode' and `ztreediff-mode'
(with-eval-after-load 'ztree-view
  (define-key ztree-mode-map (kbd "n") #'ztree-next-line)
  (define-key ztree-mode-map (kbd "p") #'ztree-previous-line))

;; Dired

(require 'dired-x) ; extra features
(require 'dired-aux) ; even more extra features
(setq dired-auto-revert-buffer 'dired-directory-changed-p ; when revisiting Dired buffers, refresh if dir has changed on disk
      dired-dwim-target t ; use neighboring dired buffer as default target dir
      dired-listing-switches "-alhvFG" ; more readable file listings
      dired-omit-files (concat dired-omit-files "\\|^\\..+$") ; omit dot files in dired-omit-mode
      dired-recursive-copies 'always ; always copy recursively
      dired-recursive-deletes 'always) ; always delete recursively
;; uncomment below to automatically update Dired buffers every
;; `auto-revert-interval' seconds, at cost of some slowdown
;; (add-hook 'dired-mode-hook #'auto-revert-mode) ; auto-refresh on file change
(add-hook 'dired-mode-hook #'dired-hide-details-mode) ; hide details initially

;; bind "z" in dired-mode to open file at point using system command
;; to open files by type
(with-eval-after-load 'dired
  (defun dired--open-file-at-pt ()
    "Opens file at point in Dired using system open command.
This opens the file using the preferred application by filetype."
    (interactive)
    (let ((filename (dired-get-file-for-visit)))
      (start-process "default-app"
                     nil
                     my-system-open-command
                     filename)))
  (define-key dired-mode-map (kbd "z") #'dired--open-file-at-pt))

;; Editing text

;; indent with soft tabs; use C-q <TAB> for real tabs
(setq-default indent-tabs-mode nil)

(defun my-yank-from-kill-ring ()
  "Yank from the kill ring into buffer at point or region.
Uses `completing-read' for selection, which is set by Ido, Ivy, etc."
  (interactive)
  (let ((to-insert (completing-read
                    "Yank : " (cl-delete-duplicates kill-ring :test #'equal))))
    ;; delete selected buffer region if any
    (if (and to-insert (region-active-p))
        (delete-region (region-beginning) (region-end)))
    ;; insert the selected entry from the kill ring
    (insert to-insert)))

;; bind `my-yank-from-kill-ring'
(global-set-key (kbd "C-c y") #'my-yank-from-kill-ring)

;; typing text replaces the active (i.e. selected) region, if any is selected
(delete-selection-mode)

;; use single spaces after sentences
(setq sentence-end-double-space nil)

;; enable transparent editing of GPG files
(require 'epa-file)
(epa-file-enable)

;; display available bindings in popup
(use-package which-key
  :pin gnu
  :bind ("C-c H w" . which-key-show-top-level)
  :init
  (setq which-key-allow-multiple-replacements t
        which-key-compute-remaps t
        ;; configure for manual activation using C-h in the middle of a key seq
        ;; see https://github.com/justbur/emacs-which-key#manual-activation
        which-key-idle-delay 10000
        which-key-idle-secondary-delay 0.05
        which-key-show-early-on-C-h t)
  :config
  ;; hide mode line lighter
  (add-to-list 'my-mode-lighter-abbrev-alist '(which-key-mode . ""))
  (which-key-mode 1))
;; force-load which-key package
(require 'which-key)

;; expand selected region by semantic units
(use-package expand-region
  :pin gnu
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package iedit
  :pin nongnu
  :init (setq iedit-toggle-key-default (kbd "C-;"))
  :config
  ;; advise iedit functions that jump to new point locations to
  ;; perform context actions after they are run
  (dolist (jump-fun '(iedit-next-occurrence
                      iedit-prev-occurrence
                      iedit-goto-first-occurrence
                      iedit-goto-last-occurrence
                      iedit-expand-to-occurrence))
    (advice-add jump-fun :after #'my-after-jump-context-actions)))

;; multiple cursors
;; using `set-rectangular-region-anchor' is probably the easiest
;; see https://emacs.stackexchange.com/a/773
(use-package multiple-cursors
  :pin nongnu
  :bind (("M-C" . mc/edit-lines)
         ("M-V" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-S-<mouse-1>" . mc/toggle-cursor-on-click))
  :init (setq mc/always-run-for-all nil
              mc/always-repeat-command nil
              mc/insert-numbers-default 1)
  :config
  ;; decrease width of the multiple-cursors bar
  ;; setting a height of 1 ends up rendering a thick bar
  ;; probably because it is too small a value
  (set-face-attribute 'mc/cursor-bar-face nil :height 10))

;; structured editing of S-expressions with Paredit
(use-package paredit
  :pin nongnu
  :commands paredit-mode
  :bind (:map paredit-mode-map
         ("{" . paredit-open-curly)
         ("}" . paredit-close-curly))
  :hook ((emacs-lisp-mode . paredit-mode)
         ;; when in minibuffer via `eval-expression`
         (eval-expression-minibuffer-setup . paredit-mode)
         ;; *scratch* default mode
         (lisp-interaction-mode . paredit-mode))
  :config
  ;; non-terminal bindings, see https://www.racket-mode.com/#paredit
  (unless terminal-frame
    (define-key paredit-mode-map (kbd "M-[") #'paredit-wrap-square)
    (define-key paredit-mode-map (kbd "M-{") #'paredit-wrap-curly))
  ;; restore old behavior of RET evaluating input in ielm and other interactive modes
  ;; see https://paredit.org/cgit/paredit/plain/NEWS
  ;; and https://old.reddit.com/r/emacs/comments/17k6pu4/m_aka_mx_evalexpression_does_nothing_upon_hitting/
  (define-key paredit-mode-map (kbd "RET") nil)
  (define-key paredit-mode-map (kbd "C-j") 'paredit-newline)
  ;; make delete-selection-mode work within paredit-mode
  (with-eval-after-load 'delsel
    (put 'paredit-forward-delete 'delete-selection 'supersede)
    (put 'paredit-backward-delete 'delete-selection 'supersede)
    (put 'paredit-open-round 'delete-selection t)
    (put 'paredit-open-square 'delete-selection t)
    (put 'paredit-doublequote 'delete-selection t)
    (put 'paredit-newline 'delete-selection t)))

;; remap C-/ to `undo-only' (keep C-_ for `undo')
;; note that C-? is by default mapped to `undo-redo'
(global-set-key (kbd "C-/") #'undo-only)

;; visualize and traverse the undo tree
(when (version<= "28" emacs-version) ;; vundo requires Emacs 28+
  (use-package vundo
    :commands (vundo)
    :bind ("C-x u" . vundo)
    :init (setq vundo-compact-display t
                vundo-glyph-alist vundo-unicode-symbols
                vundo-window-max-height 5)))

;; bind over `zap-to-char' (defaults to "M-z") with `zap-up-to-char'
(global-set-key [remap zap-to-char] #'zap-up-to-char)

(global-set-key [remap just-one-space] #'cycle-spacing)

;; Join next line to end of current line, like "J" in Vim
(defun my-join-next-line ()
  "Join the next line to the end of the current line."
  (interactive)
  (let ((col (current-column)))
    (join-line -1)
    (move-to-column col)))

(global-set-key (kbd "C-S-j") #'my-join-next-line)

(defun my-open-line-below (n)
  "Open a new line below and go to it.
With arg N, insert N newlines."
  (interactive "*p")
  (end-of-line)
  (newline n)
  (indent-according-to-mode))

(defun my-open-line-above (n)
  "Open a new line above and go to it.
With arg N, insert N newlines."
  (interactive "*p")
  (beginning-of-line)
  (newline n)
  (forward-line (- n))
  (indent-according-to-mode))

;; bind over `open-line' ("C-o") with `my-open-line-below'
(global-set-key [remap open-line] #'my-open-line-below)
;; binding for `my-open-line-above
(global-set-key (kbd "C-S-o") #'my-open-line-above)

;; use built-in DWIM versions of default editing commands
;; note that comment insertion command ("M-;") is already DWIM-ified
(global-set-key (kbd "M-u") #'upcase-dwim)
(global-set-key (kbd "M-l") #'downcase-dwim)

(use-package editorconfig
  :config (editorconfig-mode 1))

;; Emacs as an edit server

;; start server mode
(server-mode 1)

;; server mode restart safety valve
(defun restart-emacs-server ()
  "Restarts an Emacs server."
  (interactive)
  (server-force-delete)
  (server-mode 1)
  (message "Restarted Emacs server."))

;; bind SIGUSR1 signal to call `server-restart'
(define-key special-event-map [sigusr1] #'restart-emacs-server)

;; Non-programming files

(with-eval-after-load 'doc-view
  (easy-menu-define my-doc-view-menu doc-view-mode-map "Menu for Doc-View Mode."
    '("DocView"
      ["Switch to a different mode" doc-view-toggle-display :help "Switch to a different mode"]
      ["Open Text" doc-view-open-text :help "Display the current doc's contents as text"]
      "--"
      ("Navigate Doc"
       ["Goto Page ..." doc-view-goto-page :help "View the page given by PAGE"]
       "--"
       ["Scroll Down" doc-view-scroll-down-or-previous-page :help "Scroll page down ARG lines if possible, else goto previous page"]
       ["Scroll Up" doc-view-scroll-up-or-next-page :help "Scroll page up ARG lines if possible, else goto next page"]
       "--"
       ["Next Line" doc-view-next-line-or-next-page :help "Scroll upward by ARG lines if possible, else goto next page"]
       ["Previous Line" doc-view-previous-line-or-previous-page :help "Scroll downward by ARG lines if possible, else goto previous page"]
       ("Customize"
        ["Continuous Off"
         (setq doc-view-continuous nil)
         :help "Stay put in the current page, when moving past first/last line" :style radio :selected
         (eq doc-view-continuous nil)]
        ["Continuous On"
         (setq doc-view-continuous t)
         :help "Goto to the previous/next page, when moving past first/last line" :style radio :selected
         (eq doc-view-continuous t)]
        "---"
        ["Save as Default"
         (customize-save-variable 'doc-view-continuous doc-view-continuous)
         t])
       "--"
       ["Next Page" doc-view-next-page :help "Browse ARG pages forward"]
       ["Previous Page" doc-view-previous-page :help "Browse ARG pages backward"]
       "--"
       ["First Page" doc-view-first-page :help "View the first page"]
       ["Last Page" doc-view-last-page :help "View the last page"])
      "--"
      ("Adjust Display"
       ["Enlarge" doc-view-enlarge :help "Enlarge the document by FACTOR"]
       ["Shrink" doc-view-shrink :help "Shrink the document"]
       "--"
       ["Fit Width To Window" doc-view-fit-width-to-window :help "Fit the image width to the window width"]
       ["Fit Height To Window" doc-view-fit-height-to-window :help "Fit the image height to the window height"]
       "--"
       ["Fit Page To Window" doc-view-fit-page-to-window :help "Fit the image to the window"]
       "--"
       ["Set Slice From Bounding Box" doc-view-set-slice-from-bounding-box :help "Set the slice from the document's BoundingBox information"]
       ["Set Slice Using Mouse" doc-view-set-slice-using-mouse :help "Set the slice of the images that should be displayed"]
       ["Set Slice" doc-view-set-slice :help "Set the slice of the images that should be displayed"]
       ["Reset Slice" doc-view-reset-slice :help "Reset the current slice"])
      ("Search"
       ["New Search ..."
        (doc-view-search t)
        :help "Jump to the next match or initiate a new search if NEW-QUERY is given"]
       "--"
       ["Search" doc-view-search :help "Jump to the next match or initiate a new search if NEW-QUERY is given"]
       ["Backward" doc-view-search-backward :help "Call `doc-view-search' for backward search"]
       "--"
       ["Show Tooltip" doc-view-show-tooltip :help nil])
      ("Maintain"
       ["Reconvert Doc" doc-view-reconvert-doc :help "Reconvert the current document"]
       "--"
       ["Clear Cache" doc-view-clear-cache :help "Delete the whole cache (`doc-view-cache-directory')"]
       ["Dired Cache" doc-view-dired-cache :help "Open `dired' in `doc-view-cache-directory'"]
       "--"
       ["Revert Buffer" doc-view-revert-buffer :help "Like `revert-buffer', but preserves the buffer's current modes"]
       "--"
       ["Kill Proc" doc-view-kill-proc :help "Kill the current converter process(es)"]
       ["Kill Proc And Buffer" doc-view-kill-proc-and-buffer :help "Kill the current buffer"])
      "--"
      ["Customize"
       (customize-group 'doc-view)]))
  (easy-menu-define my-doc-view-minor-mode-menu doc-view-minor-mode-map "Menu for Doc-View Minor Mode."
    '("DocView*"
      ["Display in DocView Mode" doc-view-toggle-display :help "View"]
      ["Exit DocView Mode" doc-view-minor-mode])))

;; render DocView images at full resolution and downsample using the
;; built-in image scaler in Emacs 28+ or ImageMagick in prior versions
(when (display-graphic-p)
  (setq doc-view-scale-internally t
        doc-view-resolution (floor (my-frame-monitor-dpi))
        doc-view-image-width (let* ((attrs (frame-monitor-attributes))
                                    (geom (assoc 'geometry attrs))
                                    (width-pixels (nth 3 geom)))
                               ;; default width is 40% of screen width
                               (floor (* 0.4 width-pixels)))))

(use-package csv-mode
  :pin gnu
  :commands csv-mode
  :bind (:map csv-mode-map
         ("C-c C-S-a" . csv-align-visible-fields))
  :config
  (setq csv-align-style 'auto) ;; `csv-align-fields' left/right-aligns text/numbers
  (defun csv-align-visible-fields ()
    "Align visible lines in `csv-mode'. Useful for large CSV files where
`csv-align-fields' can take a very long time to run."
    (interactive)
    (csv-align-fields nil (window-start) (window-end))))

(use-package dockerfile-mode
  :pin nongnu
  :commands dockerfile-mode
  :config (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; provides a major mode for editing JSON files
(use-package json-mode
  :pin gnu
  :defer t
  :init (setq js-indent-level 2
              json-reformat:indent-width 2
              json-reformat:pretty-string? nil))

(when (executable-find "jq")
  (with-eval-after-load 'flymake-quickdef
    (flymake-quickdef-backend flymake-jq-backend
      :pre-let ((jq-exec (executable-find "jq")))
      :pre-check (unless jq-exec (error "Cannot find jq executable"))
      :write-type 'file
      :proc-form (list jq-exec "." fmqd-temp-file)
      :search-regexp
      "\\(parse error: \\)?\\(.+\\) at line \\([[:digit:]]+\\), column \\([[:digit:]]+\\).*$"
      :prep-diagnostic (let* ((msg (match-string 2))
                              (lnum (string-to-number (match-string 3)))
                              (lcol (string-to-number (match-string 4)))
                              (pos (flymake-diag-region fmqd-source lnum lcol))
                              (beg (car pos))
                              (end (cdr pos))
                              (type :error))
                         (list fmqd-source beg end type msg)))
    ;; define function for enabling the Flymake backend
    (defun flymake-jq-setup ()
      "Enable jq backend for Flymake."
      (add-hook 'flymake-diagnostic-functions
                #'flymake-jq-backend
                nil
                t))
    (with-eval-after-load 'json-mode
      ;; enable Flymake jq backend in JSON buffers
      (add-hook 'json-mode-hook #'flymake-jq-setup t)
      (add-hook 'json-mode-hook #'flymake-mode))))

(when (executable-find "jq")
  (with-eval-after-load 'json-mode
    ;; use jq with the basic operator "." (to pretty print but leave
    ;; the input unmodified otherwise) to format JSON code
    (with-eval-after-load 'reformatter
      ;; json
      ;; define `json-jq-format-{buffer|region|on-save-mode}'
      (reformatter-define json-jq-format
        :program "jq"
        :args '("--sort-keys" "." "-")
        :group 'json-mode
        :lighter " JSONFmt")
      ;; dwim function calling `json-jq-format-region' if a region is
      ;; selected, or `json-jq-format-buffer' otherwise
      (defun json-jq-format-buffer-or-region ()
        "Format the current JSON buffer or a region if selected.
Formatting a selected region only works on top-level objects."
        (interactive)
        (cond
         ((use-region-p) (json-jq-format-region (region-beginning)
                                                (region-end)))
         (t (json-jq-format-buffer))))
      ;; jsonlines
      ;; define `jsonl-jq-format-{buffer|region|on-save-mode}'
      (reformatter-define jsonl-jq-format
        :program "jq"
        :args '("--compact-output" "--sort-keys" "." "-")
        :group 'json-mode
        :lighter " JSONLFmt")
      ;; dwim function calling `jsonl-jq-format-region' if a region is
      ;; selected, or `jsonl-jq-format-buffer' otherwise
      (defun jsonl-jq-format-buffer-or-region ()
        "Format the current JSONL buffer or a region if selected.
Formatting a selected region only works on top-level objects."
        (interactive)
        (cond
         ((use-region-p) (jsonl-jq-format-region (region-beginning)
                                                 (region-end)))
         (t (jsonl-jq-format-buffer)))))))

;; major mode for editing Markdown files
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  ;; place header markup only at the start of a line
  ;; syntax highlighting in fenced code blocks
  ;; use underscores for italics instead of asterisks
  (setq markdown-asymmetric-header t
        markdown-fontify-code-blocks-natively t
        markdown-italic-underscore t)
  ;; if available, use pandoc for converting markdown files
  (when (executable-find "pandoc")
    (setq markdown-command (concat "pandoc --from markdown --to html"
                                   " --standalone"
                                   " --katex"
                                   " --highlight-style=pygments"
                                   " --quiet") ; suppress warnings
          markdown-command-needs-filename t)))

;; provides a major mode for editing YAML files
(use-package yaml-mode
  :pin nongnu
  :commands yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; Programming / Buffer reformatter macro

;; defines the `reformatter-define' macro that allows definition of
;; commands that run reformatters on the current buffer
(use-package reformatter)

;; Programming / Flymake syntax checker

;; basic Flymake customizations
(setq flymake-no-changes-timeout nil ;; don't run checks on no typing
      flymake-start-on-save-buffer t) ;; only run checks on saving

;; deferred Flymake customizations
(with-eval-after-load 'flymake
  ;; don't use legacy Flymake checker
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)
  ;; function for toggling Flymake diagnostics window
  (defun my-toggle-flymake-diagnostics ()
    "Toggles flymake diagnostics window for current buffer."
    (interactive)
    (if flymake-mode
        (let* ((buf-name (buffer-name (current-buffer)))
               (flymake-winds (condition-case nil
                                  (get-buffer-window-list
                                   (concat "*Flymake diagnostics for " buf-name "*"))
                                (error nil))))
          (if flymake-winds
              (dolist (wind flymake-winds) (quit-window nil wind))
            (flymake-show-diagnostics-buffer)))))
  ;; shorten Flymake mode line symbol
  (defun my-flymake-modeline-filter (ret)
    "Filter function for `flymake--mode-line-format`."
    (setf (seq-elt (car ret) 1) " FlyM")
    ret)
  (advice-add #'flymake--mode-line-format :filter-return
              #'my-flymake-modeline-filter)
  ;; convenience bindings
  (define-key flymake-mode-map (kbd "C-c ! n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c ! p") #'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c ! l") #'my-toggle-flymake-diagnostics))

;; enable Flymake when editing Emacs Lisp buffers
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

(require 'flymake-quickdef)

;; Programming / DevSkim and Flymake

;; Code security analysis using devskim, https://github.com/microsoft/DevSkim
;; A Flymake backend for it is defined here, and can be used by calling
;; `flymake-devskim-toggle' before `flymake-mode' in a given mode's hook, e.g.
;;   (add-hook 'python-mode-hook 'flymake-devskim-toggle)
;;   (add-hook 'python-mode-hook 'flymake-mode t)
;; For more info on the different severity types, see
;; https://github.com/microsoft/DevSkim/wiki/Rule-Object-Schema
(with-eval-after-load 'flymake-quickdef
  (flymake-quickdef-backend flymake-devskim-backend
    :pre-let ((devskim-exec (executable-find "devskim")))
    :pre-check (unless devskim-exec (error "Cannot find devskim executable"))
    :write-type 'file
    :proc-form (list devskim-exec
                     "analyze"
                     "-f" "text"
                     "-o" "%L:%C: %S : [%R] %N"
                     fmqd-temp-file)
    :search-regexp
    "\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\([[:alpha:]]+\\) : \\(.+\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 1)))
                            (lcol (string-to-number (match-string 2)))
                            (severity (downcase (match-string 3)))
                            (msg (match-string 4))
                            (pos (flymake-diag-region fmqd-source lnum lcol))
                            (beg (car pos))
                            (end (cdr pos))
                            (type (cond
                                    ((string= severity "critical") :error)
                                    ((string= severity "important") :error)
                                    ((string= severity "moderate") :warning)
                                    ((string= severity "best-practice") :note)
                                    ((string= severity "manual-review") :note)
                                    (t :note))))
                       (list fmqd-source beg end type msg)))
  ;; define function for toggling the Flymake backend
  (defun flymake-devskim-toggle ()
    "Toggle devskim backend for Flymake."
    (interactive)
    (if (memq 'flymake-devskim-backend flymake-diagnostic-functions)
        (remove-hook 'flymake-diagnostic-functions #'flymake-devskim-backend t)
      (add-hook 'flymake-diagnostic-functions #'flymake-devskim-backend nil t))))

;; Programming / Bash and sh shell scripts

(with-eval-after-load 'flymake-quickdef
  (flymake-quickdef-backend flymake-shellcheck-backend
    :pre-let ((shellcheck-exec (executable-find "shellcheck")))
    :pre-check (unless shellcheck-exec (error "Cannot find shellcheck executable"))
    :write-type 'file
    :proc-form (list shellcheck-exec
                     "-f" "gcc"
                     fmqd-temp-file)
    :search-regexp
    "^.+?:\\([0-9]+\\):\\([0-9]+\\): \\(.*\\): \\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 1)))
                            (lcol (string-to-number (match-string 2)))
                            (severity (downcase (match-string 3)))
                            (msg (match-string 4))
                            (pos (flymake-diag-region fmqd-source lnum lcol))
                            (beg (car pos))
                            (end (cdr pos))
                            (type (cond
                                    ((string= severity "note") :note)
                                    ((string= severity "warning") :warning)
                                    (t :error))))
                       (list fmqd-source beg end type msg)))
  ;; define function for enabling the Flymake backend
  (defun flymake-shellcheck-setup ()
    "Enable shellcheck backend for Flymake."
    (add-hook 'flymake-diagnostic-functions #'flymake-shellcheck-backend nil t))
  (with-eval-after-load 'sh-script
    (add-hook 'sh-mode-hook #'flymake-shellcheck-setup)
    (add-hook 'sh-mode-hook #'flymake-mode)))

;; Programming / Python

; ;; install ELPA version of python.el
; (my-install-elpa-package 'python)

;; use the py script for initializing the Python REPL if available
(when (executable-find "py")
  (setq python-shell-interpreter "py"
               python-shell-interpreter-args ""
               python-shell-prompt-detect-failure-warning nil)
  (with-eval-after-load 'python
    (add-to-list 'python-shell-completion-native-disabled-interpreters "py")))

;; modify Python syntax table to keep underscores within a word
;; boundary when editing Python buffers
(with-eval-after-load 'python
  (modify-syntax-entry ?_ "w" python-mode-syntax-table))

(with-eval-after-load 'reformatter
  ;; define `python-black-format-buffer', `python-black-format-region'
  ;; and `python-black-format-on-save-mode'
  (reformatter-define python-black-format
    :program "black"
    :args '("-")
    :group 'python
    :lighter 'PyBlFmt)
  ;; dwim function that calls `python-black-format-region' if a region
  ;; is selected, or `python-black-format-buffer' otherwise
  (defun python-black-format-buffer-or-region ()
       "Format the current Python buffer or a region if selected.
Formatting a selected region only works on top-level objects."
       (interactive)
       (cond
        ((use-region-p) (python-black-format-region (region-beginning)
                                                    (region-end)))
        (t (python-black-format-buffer)))))

;; Project interaction

;; binding for calling Magit
(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status)
  :config
  (add-hook 'magit-process-find-password-functions
            #'magit-process-password-auth-source)
  ;; use Emacs minibuffer for GnuPG passphrases
  ;; see https://vxlabs.com/2021/03/21/gnupg-pinentry-via-the-emacs-minibuffer/
  (setq epg-pinentry-mode 'loopback))

;; Uncomment to check VC info on file auto-revert (increases I/O load)
;; https://magit.vc/manual/magit/The-mode_002dline-information-isn_0027t-always-up_002dto_002ddate.html
;; (setq auto-revert-check-vc-info t)

;; Search

;; "C-c C-p" in grep bufs allow writing with changes pushed to files
(use-package wgrep
  :config (setq wgrep-auto-save-buffer nil
                wgrep-too-many-file-length 10))

(setq imenu-auto-rescan t)

;; fold characters when searching with Isearch
(setq search-default-mode #'char-fold-to-regexp)

;; Visual (part 2)

;; display line numbers by default when editing code
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode 1)))

;; show trailing whitespace when editing code
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

;; show point location column number in mode line
(setq column-number-mode t)

;; show matching parentheses with no delay
(setq show-paren-delay 0)
(show-paren-mode 1)

; ;; add internal frame border
; (add-to-list 'default-frame-alist
;              `(internal-border-width . 12))
; (defun my-default-frame-border-teardown ()
;   "Removes internal-border-width entries from `default-frame-alist'."
;   (setq default-frame-alist
;         (assq-delete-all 'internal-border-width default-frame-alist)))
; ;; add teardown function to be run before closing Emacs, which needs
; ;; to run early when closing so add it to the end of `after-init-hook'
; (add-hook 'after-init-hook
;           (lambda ()
;             (add-hook 'kill-emacs-hook #'my-default-frame-border-teardown))
;           t)

(require 'censor)

;; pulse line when point is cycled btw top/middle/bottom of window
(advice-add 'move-to-window-line-top-bottom :after #'my-pulse-line)

;; pulse line after changing focused window using `other-window'
(advice-add 'other-window :after #'my-pulse-line)

;; also pulse line when Emacs regains focus, covers focus on new frame
(add-hook 'focus-in-hook #'my-pulse-line)

;; as well as pulsing the line when popping the mark ring
(advice-add 'pop-to-mark-command :after #'my-pulse-line)
(advice-add 'pop-global-mark :after #'my-pulse-line)

(require 'too-long-lines-mode) ; in site-lisp
(too-long-lines-mode 1)

;; enable `hs-minor-mode' in programming code files
(with-eval-after-load 'prog-mode
    (add-hook 'prog-mode-hook 'hs-minor-mode))

;; Web

;; built-in Emacs text web browser
(use-package eww
  :ensure nil ;; built-in
  :commands (eww eww-follow-link)
  :init (setq eww-search-prefix "https://duckduckgo.com/lite?q=")
  :config
  ;; don't render images in HTML pages by default
  (setq-default shr-inhibit-images t)
  ;; toggle for enabling and disabling images
  (defun eww--toggle-images ()
    "Toggle displaying of images when rendering HTML."
    (interactive)
    (setq-local shr-inhibit-images (not shr-inhibit-images))
    (eww-reload)
    (message "Images are now %s" (if shr-inhibit-images "off" "on")))
  (define-key eww-mode-map "I" #'eww--toggle-images))

;; client for Gopher and Gemini protocol
(use-package elpher
  :config
  ;; work around TLS verification issues for Gemini sites
  (add-hook 'elpher-mode-hook
            (lambda ()
              (setq-local gnutls-verify-error nil))))

(defun open-gnutls-stream--after-sleep-250ms (&rest args)
  "Workaround for race condition bug in `open-gnutls-stream'.

Adds 250ms to the opening of GnuTLS connections.

ARGS is a list of the original arguments passed to
`open-gnutls-stream' and is ignored.

See https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=930573#10
for more information."
  (sleep-for 0 250))

;; add advice to `open-gnutls-stream' to have it sleep for 250ms
;; as a workaround for GnuTLS race condition bug
(advice-add #'open-gnutls-stream :after #'open-gnutls-stream--after-sleep-250ms)

;; increase network security settings
(setq gnutls-verify-error t)
(setq gnutls-min-prime-bits 1024)
(setq network-security-level 'high)
(setq nsm-save-host-names t)

;; HTTP requests privacy settings
(setq url-cookie-untrusted-urls '(".*")) ;; no cookies
(setq url-privacy-level 'paranoid) ;; more private HTTP requests
(url-setup-privacy-info) ;; apply `url-privacy-level'

;; Writing

;; advise flyspell jump functions to perform context actions after
;; they are run
(with-eval-after-load 'flyspell
  (advice-add 'flyspell-goto-next-error :after #'my-after-jump-context-actions))

;; unbind some Flyspell default bindings that conflict with other more
;; useful bindings
(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-;") nil)) ; `iedit-mode' binding

;; Other

;; mouse settings
(when (display-graphic-p)
  (add-hook
   'after-init-hook
   (lambda ()
     ;; use super-left-click as middle-click (trackpad workaround)
     ;; (define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>"))
     ;; smooth scrolling, hold SHIFT/CONTROL for 5 line/full window increments
     (setq mouse-wheel-scroll-amount '(1
                                       ((shift) . 5)
                                       ((control) . nil)))
     ;; enable horizontal scrolling
     (setq mouse-wheel-flip-direction t ;; t/nil for trackpad/mouse
           mouse-wheel-tilt-scroll t))
   t))

;; set *scratch* buffer major-mode to fundamental-mode
(setq initial-major-mode 'fundamental-mode)

;; scroll a line at a time at window edge
(setq scroll-conservatively 101)

;; bindings for scrolling line-at-a-time like "C-e" and "C-y" in Vim
(global-set-key (kbd "C-S-e") #'scroll-up-line)
(global-set-key (kbd "C-S-y") #'scroll-down-line)

;; turn off audio and visual bells
(setq ring-bell-function 'ignore)

;; suppress auto-revert minibuffer messages
(setq auto-revert-verbose nil)

;; suppress splash screen that appears on startup by default
(setq inhibit-startup-message t)

;; visit large files without loading it entirely
(use-package vlf
  :pin gnu
  :config (require 'vlf-setup))

;; automatically disable major and minor modes that can slow down
;; Emacs when visiting files with long lines, Emacs 27+ only
(when (require 'so-long nil :noerror)
  (global-so-long-mode 1)
  ;; leave major modes alone, only disable minor modes
  ;; increase threshold before so-long action is invoked
  (setq so-long-action 'so-long-minor-mode
        so-long-max-lines 10
        so-long-threshold 500))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq calc-multiplication-has-precedence nil
      calc-ensure-consistent-units t
      calc-context-sensitive-enter t
      calc-undo-length 100
      calc-highlight-selections-with-faces nil)

;; Enable some functions disabled by default.
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-set-key (kbd "<f5>") #'revert-buffer)

;; add remote user paths to the TRAMP remote search paths
(with-eval-after-load 'tramp-sh
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; repeating "C-SPC" after popping mark with "C-u C-SPC" pops it again
(setq set-mark-command-repeat-pop t)

(defun my-clear-register (register)
  "Clear the contents in register REGISTER."
  (interactive (list (register-read-with-preview "Clear register: ")))
  (set-register register nil)
  (message "Cleared register %c" register))

;; global binding for clearing a register's contents
(global-set-key (kbd "C-x r DEL") #'my-clear-register)

;; convert regexp to rx notation
(use-package xr)

;; alternative binding for opening the menu bar
(global-set-key (kbd "C-c B o") #'menu-bar-open)

;; support for mouse bindings to emulate Acme mouse interface
(require 'acme)
;; emulate Acme interface
(with-eval-after-load 'acme
  (setq acme-mode-no-warp-mouse nil)
  (setq acme-mode-user-command-keywords
        '(("Commentcol" . (lambda (arg) (comment-set-column (string-to-number arg))))
          ("Expand" . (lambda () (call-interactively 'er/expand-region))) ; requires expand-region
          ("Fillcol" . (lambda (&optional arg) (if arg (setq fill-column (string-to-number arg)) (setq fill-column 70))))
          ("Fontlock" . (lambda () (call-interactively 'font-lock-mode)))
          ("Imenu" . (lambda () (save-selected-window (imenu-list-smart-toggle)))) ; requires imenu-list
          ("Lnumbers" . (lambda () (call-interactively 'display-line-numbers-mode)))))
  (setq acme-mode-initial-tag-line (concat acme-mode-initial-tag-line "\n"
                                           "Expand Fontlock Imenu Lnumbers Fillcol "))
  (setq acme-mode-plumbing-rules
        (append '(("^\\(gemini\\|gopher\\)://[^ |{};]*[^/]/?$" . (lambda (url) (elpher-go url))) ; requires elpher
                  ("^PEP-?[0-9]+$" . (lambda (pep-line)
                                     (save-match-data
                                       (and (string-match "PEP-?\\([0-9]+\\)" pep-line)
                                            (acme-mode--plumb
                                             (concat "https://www.python.org/dev/peps/pep-"
                                                     (format "%04d"
                                                             (string-to-number (match-string 1 pep-line)))
                                                     "/")))))))
                acme-mode-plumbing-rules))
  (push 'imenu-list-major-mode acme-mode-exclude-major-modes)
  (push 'rg-mode acme-mode-exclude-major-modes)
  ;; open tag buffer key binding
  (define-key acme-mode-map [C-s-return] #'acme-mode-pop-tag-buffer)
  (define-key acme-mode-map [s-return] #'acme-mode-set-tag-buffer-default-directory-to-current)
  ;; trackpad support
  (cond ((eq system-type 'darwin)
         ;; middle-click
         (define-key acme-mode-map [M-down-mouse-1] #'acme-mode--down-mouse-2)
         (define-key acme-mode-map [M-mouse-1] #'acme-mode--mouse-2)
         (define-key acme-mode-map [M-drag-mouse-1] #'acme-mode--mouse-2)
         ;; right-click
         (define-key acme-mode-map [s-down-mouse-1] #'acme-mode--down-mouse-3)
         (define-key acme-mode-map [s-double-down-mouse-1] #'acme-mode--down-mouse-3)
         (define-key acme-mode-map [s-triple-down-mouse-1] #'acme-mode--down-mouse-3)
         (define-key acme-mode-map [s-mouse-1] #'acme-mode--mouse-3)
         (define-key acme-mode-map [s-drag-mouse-1] #'acme-mode--mouse-3)
         (setq acme-mode-keyboard-chord-keylist
               '(("1" . 1) ("s-1" . 1) ("M-1" . 1)
                 ("2" . 2) ("s-2" . 2) ("M-2" . 2)
                 ("3" . 3) ("s-3" . 3) ("M-3" . 3))))
        ((eq system-type 'gnu/linux)
         ;; middle-click
         (define-key acme-mode-map [C-down-mouse-1] #'acme-mode--down-mouse-2)
         (define-key acme-mode-map [C-mouse-1] #'acme-mode--mouse-2)
         (define-key acme-mode-map [C-drag-mouse-1] #'acme-mode--mouse-2)
         ;; right-click
         (define-key acme-mode-map [M-down-mouse-1] #'acme-mode--down-mouse-3)
         (define-key acme-mode-map [M-double-down-mouse-1] #'acme-mode--down-mouse-3)
         (define-key acme-mode-map [M-triple-down-mouse-1] #'acme-mode--down-mouse-3)
         (define-key acme-mode-map [M-mouse-1] #'acme-mode--mouse-3)
         (define-key acme-mode-map [M-drag-mouse-1] #'acme-mode--mouse-3)
         ;; in ChromeOS, Alt-left-click gets auto-translated to right-click
         (define-key acme-mode-map [M-down-mouse-3] #'acme-mode--down-mouse-3)
         (define-key acme-mode-map [M-double-down-mouse-3] #'acme-mode--down-mouse-3)
         (define-key acme-mode-map [M-triple-down-mouse-3] #'acme-mode--down-mouse-3)
         (define-key acme-mode-map [M-mouse-3] #'acme-mode--mouse-3)
         (define-key acme-mode-map [M-drag-mouse-3] #'acme-mode--mouse-3)
         (setq acme-mode-keyboard-chord-keylist
               '(("1" . 1) ("C-1" . 1) ("M-1" . 1)
                 ("2" . 2) ("C-2" . 2) ("M-2" . 2)
                 ("3" . 3) ("C-3" . 3) ("M-3" . 3)))))
  ;; enable `acme-mode'
  ;(acme-mode 1)
  )

;; OS-specific / macOS

;; on macOS, use Option keys as Meta and file polling for auto-revert
(when (eq system-type 'darwin)
  (setq auto-revert-use-notify nil ;; macOS does not support file notifications
        mac-option-modifier 'meta ;; use Option key as Meta
        mac-right-option-modifier 'left ;; right Option uses left's mapping
        mac-command-modifier 'super)) ;; keep Super key as is

(when (eq window-system 'ns)
  (setq mac-option-modifier 'meta ; use Option key as Meta
        mac-right-option-modifier 'left ; right Option uses left's mapping
        mac-command-modifier 'super) ; keep Super key as is
  ;; unbind "s-w" and "s-q" to avoid unintended frame and Emacs closure
  (global-unset-key (kbd "s-w"))
  (global-unset-key (kbd "s-q")))

;; revert Command keys in Emacs Mac Port to match Emacs for Mac OS X bindings
(when (eq window-system 'mac)
  (setq mac-option-modifier 'meta
        mac-right-option-modifier 'left
        mac-command-modifier 'super)
  (global-set-key (kbd "s-'") 'next-multiframe-window)
  (global-set-key (kbd "s-,") 'customize)
  (global-set-key (kbd "s-`") 'other-frame)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-c") 'kill-ring-save) ;; ns-copy-including-secondary
  (global-set-key (kbd "s-d") 'isearch-repeat-backward)
  (global-set-key (kbd "s-f") 'isearch-forward)
  (global-set-key (kbd "s-g") 'isearch-repeat-forward)
  ;; (global-set-key (kbd "s-h") 'ns-do-hide-emacs) ;; done by default
  (global-set-key (kbd "s-j") 'exchange-point-and-mark)
  (global-set-key (kbd "s-k") 'kill-this-buffer)
  (global-set-key (kbd "s-l") 'goto-line)
  (global-set-key (kbd "s-m") 'iconify-frame)
  (global-set-key (kbd "s-n") 'make-frame)
  ;; (global-set-key (kbd "s-o") 'ns-open-file-using-panel) ;; no equivalent
  ;; (global-set-key (kbd "s-p") 'ns-print-buffer) ;; no equivalent
  (global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "s-u") 'revert-buffer)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-w") 'delete-frame)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-y") 'yank) ;; ns-paste-secondary
  (global-set-key (kbd "s-z") 'undo))

;; case-insensitive sorting in Dired
;; http://pragmaticemacs.com/emacs/case-insensitive-sorting-in-dired-on-os-x/
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-ignore-case t)
  (setq ls-lisp-use-string-collate nil)
  ;; customise the appearance of the listing
  (setq ls-lisp-verbosity '(links uid))
  ;; (setq ls-lisp-format-time-list '("%b %e %H:%M" "%b %e %Y"))
  (setq ls-lisp-use-localized-time-format t))

;; configure system open file by type command for macOS
(when (eq system-type 'darwin)
  (setq my-system-open-command "open"))

;; exclude Emacs source files from recentf history on macOS
(add-to-list 'recentf-exclude "^/Applications/Emacs.app/")

;; scale up LaTeX fragment preview images on macOS
(if (and (display-graphic-p)
         (eq system-type 'darwin)
         (executable-find "dvipng"))
    (setq org-format-latex-options (plist-put org-format-latex-options
                                              :scale 1.5)))

;; increment DocView DPI resolution for Mac Retina screens
;; when using emacs-mac port
(when (eq window-system 'mac)
  (setq doc-view-resolution 300))

;; Transient commands

(use-package transient
  :pin gnu
  :init
  ;; convenience function for specifying transient toggle descriptions
  (defun transient--make-description (desc is-enabled)
    "Return a string description for transient descriptions.
The returned string has format \"DESC [ ]\" if IS-ENABLED is nil
or \"DESC [X]\" if is-enabled is non-nil.

Examples:

  (transient--make-description \"highlight\" symbol-overlay-mode)
  => \"highlight [x]\"

Example of use with transient suffix definitions in a
`transient-define-prefix' macro:

  ...
  (\"m\" (lambda () (transient--make-description
                       \"highlight-at-pt\"
                       symbol-overlay-mode))
   symbol-overlay-mode)
  ...

  => \"m highlight-at-pt [ ]\"
"
    (concat desc " " (if is-enabled "[X]" "[ ]")))
  :config
  ;; bind "q" to quit transient popups by default, transient defns
  ;; binding "q" will attempt to bind "Q" or "M-q" instead
  (transient-bind-q-to-quit))

;; Transient commands / Global transients

;; add transient popup for bookmark commands
(transient-define-prefix transient/bookmarks ()
  "Various bookmark commands."
  ["Bookmarks"
   ["Navigate"
    ("j" "Jump" bookmark-jump)
    ("l" "List" list-bookmarks)
    ]
   ["Add/Remove"
    ("s" "Set" bookmark-set)
    ("d" "Delete" bookmark-delete)
    ("i" "Insert" bookmark-insert)
    ]
   ["File"
    ("L" "Load" bookmark-load)
    ("W" "Write" bookmark-write)
    ]
   ]
  )
(global-set-key (kbd "C-c B m") #'transient/bookmarks)

(defun transient/buffer--tramp-cleanup-buffers ()
  "Clean up all TRAMP buffers and connections with confirm prompt."
  (interactive)
  (when (y-or-n-p "Cleanup all TRAMP buffers and connections? ")
    (tramp-cleanup-all-buffers)))

(defun transient/buffer--kill-other-buffers ()
  "Kill all file buffers except the current one."
  (interactive)
  (when (y-or-n-p "Kill all file buffers except the current one? ")
    (seq-each
     #'kill-buffer
     (delete (current-buffer)
             (seq-filter #'buffer-file-name (buffer-list))))))

(defun transient/buffer--indent-region-or-buffer ()
  "Indent a selected region, or the buffer otherwise."
  (interactive)
  (cond
   ((use-region-p) (indent-region (region-beginning) (region-end)))
   (t (indent-region (point-min) (point-max)))))

(defun transient/buffer--untabify-region-or-buffer ()
  "Convert tabs to spaces in a selected region, or the buffer otherwise."
  (interactive)
  (cond
   ((use-region-p) (untabify (region-beginning) (region-end)))
   (t (untabify (point-min) (point-max)))))

(defun transient/buffer--apply-all-hygiene-ops-region-or-buffer ()
  "Apply standard hygiene operations for selected region, or buffer otherwise.
The standard hygiene operations include removing trailing
whitespace, indenting and untabifying."
  (interactive)
  (progn
    (whitespace-cleanup)
    (transient/buffer--indent-region-or-buffer)
    (transient/buffer--untabify-region-or-buffer)))

(defun transient/buffer--open-containing-dir-externally (&optional path)
  "Opens the directory containing PATH or the buffer if unspecified externally."
  (interactive)
  (let* ((my-path (cl-some 'identity (list path
                                           (buffer-file-name)
                                           default-directory)))
         (my-full-path (expand-file-name my-path))
         (my-dir-path (file-name-directory my-full-path))
         (my-process-args (list "my-open-dir" nil
                                my-system-open-command my-dir-path)))
    (apply 'start-process my-process-args)))

(defun transient/buffer--print-minor-modes ()
  "Print enabled minor modes for the current buffer."
  (interactive)
  (let* ((maybe-active-minor-modes (mapcar #'car minor-mode-alist))
         (active-minor-modes (seq-filter (lambda (mode)
                                           (and (boundp mode)
                                                (symbol-value mode)))
                                         maybe-active-minor-modes))
         ;; sort alphabetically
         (active-minor-modes-sorted (sort (mapcar #'symbol-name
                                                  active-minor-modes)
                                          'string<))
         ;; five minor modes to a line
         (active-minor-mode-tuples (seq-partition active-minor-modes-sorted 5))
         (active-minor-mode-tuples-strs (mapcar
                                         (lambda (tup)
                                           (mapconcat #'identity tup " "))
                                         active-minor-mode-tuples))
         (msg-str (mapconcat 'identity active-minor-mode-tuples-strs "\n")))
    (message msg-str)))

(defun transient/buffer--clone-indirect-buffer-other-window ()
  "Create an indirect buffer of the current one in another window.
This wraps `clone-indirect-buffer-other-window' but provides a
name for the cloned indirect buffer ending with \"-INDIRECT\"."
  (interactive)
  (let ((bufname (generate-new-buffer-name
                  (concat (buffer-name)
                          "-INDIRECT"))))
    (clone-indirect-buffer-other-window bufname t)))

;; add transient for buffer management commands
(transient-define-prefix transient/buffer ()
  "Buffer management commands."
  ["Buffer"
   ["Select"
    ("b" "Switch" switch-to-buffer)
    ("n" "Next" next-buffer :transient t)
    ("p" "Previous" previous-buffer :transient t)
    ("z" "Open external" transient/buffer--open-containing-dir-externally)
    ]
   ["Hygiene"
    ("ci" "Indent" transient/buffer--indent-region-or-buffer)
    ("ct" "Untabify" transient/buffer--untabify-region-or-buffer)
    ("cr" "Whitespace report" whitespace-report)
    ("cw" "Whitespace cleanup" whitespace-cleanup)
    ("ca" "All hygiene ops" transient/buffer--apply-all-hygiene-ops-region-or-buffer)
    ]
   ["File operations"
    ("r" "Revert" revert-buffer)
    ("B" "Bury" bury-buffer)
    ("U" "Unbury" unbury-buffer)
    ("s" "Save" save-buffer)
    ("S" "Save all" save-some-buffers)
    ("k" "Kill" kill-this-buffer)
    ("K" "Kill matching" kill-matching-buffers)
    ("o" "Kill others" transient/buffer--kill-other-buffers)
    ("T" "TRAMP cleanup" transient/buffer--tramp-cleanup-buffers)
    ("I" "Make indirect" transient/buffer--clone-indirect-buffer-other-window)
    ]
   ]
  [
   ["Other"
    ("C" "Count words" count-words)
    ("W" "What line" what-line)
    ("M" "Minor modes" transient/buffer--print-minor-modes)
    ("R" (lambda ()
           (transient--make-description
            "Autorevert"
            (and (boundp 'auto-revert-mode) auto-revert-mode)))
     auto-revert-mode :transient t)
    ("M-r" (lambda ()
             (transient--make-description
              "Revert without query"
              revert-without-query-mode))
     revert-without-query-mode :transient t)
    ]
   ]
  )
(global-set-key (kbd "C-c b") #'transient/buffer)

(require 'debug)

(defun transient/debugger--list-variables ()
  "Print variables configured to invoke the debugger to the minibuffer."
  (interactive)
  (prin1 (debug--variable-list)))

;; add transient popup for debugger commands
(transient-define-prefix transient/debugger ()
  "Emacs debugger settings."
  ["Emacs debugger settings"
   ["Toggle"
    ("1" (lambda ()
           (transient--make-description "Debug on error" debug-on-error))
     toggle-debug-on-error :transient t)
    ("2" (lambda ()
           (transient--make-description "Debug on quit" debug-on-quit))
     toggle-debug-on-quit :transient t)
    ]
   ["Invoke on function entry"
    ("fl" "List" debugger-list-functions)
    ("fa" "Add" debug-on-entry)
    ("fc" "Cancel" cancel-debug-on-entry)
    ]
   ["Invoke on variable change"
    ("vl" "List" transient/debugger--list-variables)
    ("va" "Add" debug-on-variable-change)
    ("vc" "Cancel" cancel-debug-on-variable-change)
    ]
   ]
  )
(global-set-key (kbd "C-c D") #'transient/debugger)

;; add transient popup for Ediff commands
(transient-define-prefix transient/ediff ()
  "Various Ediff launch commands."
  ["Ediff"
   ["2 Way"
    ("b" "Buffers" ediff-buffers)
    ("f" "Files" ediff-files)
    ("d" "Directories" ediff-directories)
    ("c" "Buffer vs File" ediff-current-file)
    ("~" "File vs Backup" ediff-backup)
    ]
   ["3 Way"
    ("3b" "Buffers" ediff-buffers3)
    ("3f" "Files" ediff-files3)
    ("3d" "Directories" ediff-directories3)
    ]
   ["Patches"
    ("pb" "Buffer" ediff-patch-buffer)
    ("pf" "File" ediff-patch-file)
    ]
   ["Regions"
    ("rl" "Linewise" ediff-regions-linewise)
    ("rw" "Wordwise" ediff-regions-wordwise)
    ]
   ["Windows"
    ("wl" "Linewise" ediff-windows-linewise)
    ("ww" "Wordwise" ediff-windows-wordwise)
    ]
   ]
  )
(global-set-key (kbd "C-c d e") #'transient/ediff)

;; add transient popup for various editing commands
(with-eval-after-load 'acme
  (transient-define-prefix transient/edit ()
    "Editing commands."
    ["Edit"
     ["Completion"
      ("/" "Dyn. abbrev" dabbrev-expand :transient t) ; built-in
      ;; ("TAB" "Company" company-complete) ; autoloaded from company-mode.el
      ]
     ["Line"
      ("O" "New line above" my-open-line-above :transient t)
      ("o" "New line below" my-open-line-below :transient t)
      ("J" "Join lines" my-join-next-line :transient t)
      ]
     ["Multi-cursor"     ; functions autoloaded from multiple-cursors.el
      ("C" "Edit lines" mc/edit-lines)
      ("V" "Rect select" set-rectangular-region-anchor)
      ("<" "Previous" mc/mark-previous-like-this :transient t)
      (">" "Next" mc/mark-next-like-this :transient t)
      ]
     ["Other"
      ("A" (lambda ()
             (transient--make-description
              "Acme mouse"
              acme-mode))
       acme-mode :transient t)            ; emulate Acme mouse user interface
      (";" "Iedit" iedit-mode)            ; autoloaded from iedit.el
      ("=" "Expand region" er/expand-region) ; autoloaded from expand-region.el
      ]
     ]
    )
  (global-set-key (kbd "C-c e") #'transient/edit))

(defun transient/frame--previous-frame ()
  "Select previous frame."
  (interactive)
  (other-frame -1))

;; add transient popup for frame commands
(transient-define-prefix transient/frame ()
  "Frame management commands."
  ["Frame"
   ["Select"
    ("n" "Next" other-frame)
    ("p" "Previous" transient/frame--previous-frame)
    ("s" "By name" select-frame-by-name)
    ]
   ["Layout"
    ("0" "Delete frame" delete-frame)
    ("1" "Delete other frames" delete-other-frames)
    ("2" "Create new frame" make-frame-command)
    ]
   ["Resize"
    ("M" "Toggle maximized" toggle-frame-maximized :transient t)
    ("f" "Toggle fullscreen" toggle-frame-fullscreen :transient t)
    ]
   ]
  )
(global-set-key (kbd "C-c F r") #'transient/frame)

;; add transient popup for help commands
(transient-define-prefix transient/help ()
  "Various help commands."
  ["Help"
   ["Apropos"
    ("aa" "Symbol" apropos)
    ("ac" "Command" apropos-command)
    ("ad" "Documentation" apropos-documentation)
    ("al" "Library" apropos-library)
    ("av" "Variable" apropos-variable)
    ("aV" "Value" apropos-value)
    ]
   ["Describe"
    ("db" "Bindings" describe-bindings)
    ("dc" "Char" describe-char)
    ("df" "Function" describe-function)
    ("dF" "Face" describe-face)
    ("dk" "Key" describe-key)
    ("dm" "Mode" describe-mode)
    ("dp" "Package" describe-package)
    ("ds" "Syntax" describe-syntax)
    ("dv" "Variable" describe-variable)
    ]
   ["Info"
    ("ia" "Apropos" info-apropos)
    ("ib" "Browse" info)
    ("if" "File" info-lookup-file)
    ("ik" "Keywords" info-finder)
    ("is" "Symbol" info-lookup-symbol)
    ]
   ["Other"
    ("lf" "List faces" list-faces-display)
    ("ve" "View messages" view-echo-area-messages)
    ("vl" "View lossage" view-lossage)
    ("w" "Where is" where-is)
    ]
   ]
  )
(global-set-key (kbd "C-c H h") #'transient/help)

;; add transient for keyboard macros
(transient-define-prefix transient/keyboard-macros ()
  "Keyboard macro commands. Tries to adhere to \"C-x C-k\" bindings."
  ["Keyboard Macros"
   ["Actions"
    ("C-s" "Start" kmacro-start-macro)           ; also "C-x ("
    ("C-k" "Call last" kmacro-end-or-call-macro) ; also "C-x )"
    ("C-r" "Call last on region" apply-macro-to-region-lines)
    ]
   ["Ring"
    ("C-n" "Cycle next" kmacro-cycle-ring-next :transient t)
    ("C-p" "Cycle prev" kmacro-cycle-ring-previous :transient t)
    ("C-v" "View last" kmacro-view-macro :transient t)
    ("C-d" "Delete head" kmacro-delete-ring-head :transient t)
    ]
   ["Edit"
    ("e" "Named" edit-kbd-macro)
    ("RET" "Last" kmacro-edit-macro)
    ("l" "Lossage" kmacro-edit-lossage)
    ("SPC" "Step" kmacro-step-edit-macro)
    ]
   ]
  [
   ["Bind/Name"
    ("b" "Bind to key" kmacro-bind-to-key)
    ("n" "Name last" kmacro-name-last-macro)
    ("x" "To register" kmacro-to-register)
    ]
   ["Other"
    ("i" "Insert named" insert-kbd-macro)
    ]
   ]
  )
(global-set-key (kbd "C-c k") #'transient/keyboard-macros)

(defun transient/marks-and-markers--xref-pop-marker-stack-all ()
  "Pop back to where `xref-find-definitions' was first invoked.
\\[xref-find-definitions] is the current binding for `xref-find-definitions'."
  (interactive)
  (let ((ring xref--marker-ring))
    (when (ring-empty-p ring)
      (user-error "Marker stack is empty"))
    (let ((marker (ring-remove ring nil))) ;; oldest marker
      (switch-to-buffer (or (marker-buffer marker)
                            (user-error "The marked buffer has been deleted")))
      (goto-char (marker-position marker))
      (set-marker marker nil nil)
      (run-hooks 'xref-after-return-hook)
      (xref-clear-marker-stack)))) ;; clear the rest of the marker stack

(defun transient/marks-and-markers--push-mark ()
  "Push location of point into the mark ring."
  (interactive)
  (push-mark))
(defun transient/marks-and-markers--pop-mark ()
  "Pop the top location the mark ring and jump to it."
  (interactive)
  (set-mark-command t))
(defun transient/marks-and-markers--push-marker ()
  "Push location of point onto the marker stack."
  (interactive)
  (xref-push-marker-stack))
(defun transient/marks-and-markers--clear-marker-stack ()
  "Clear the marker stack."
  (interactive)
  (xref-clear-marker-stack)
  (message "Cleared `xref--marker-ring'"))

(transient-define-prefix transient/marks-and-markers ()
  "Commands for manipulating and managing marks and markers."
  ["Marks/Markers"
   ["Mark"
    ("SPC" "Push" transient/marks-and-markers--push-mark)
    ("RET" "Pop" transient/marks-and-markers--pop-mark :transient t)
    ("DEL" "Pop global" pop-global-mark :transient t)
    ]
   ["Marker"
    ("." "Push" transient/marks-and-markers--push-marker)
    ("," "Pop" xref-pop-marker-stack :transient t)
    ("<" "Pop all" transient/marks-and-markers--xref-pop-marker-stack-all)
    ("c" "Clear stack" transient/marks-and-markers--clear-marker-stack)
    ]
   ]
  )
(global-set-key (kbd "C-c M") #'transient/marks-and-markers)

;; add transient popup for Emacs package management
(transient-define-prefix transient/package ()
  "Emacs package management commands."
  ["Package management"
   ("l" "List packages" list-packages)
   ("i" "Install package" package-install)
   ("k" "Delete package" package-delete)
   ("r" "Reinstall package" my-package-reinstall)
   ("R" "Use-package report" use-package-report) ; requires use-package-compute-statistics set to non-nil before use-package declarations
   ]
  )
(global-set-key (kbd "C-c P k") #'transient/package)

;; add transient for Emacs profiler
(transient-define-prefix transient/profiler ()
  "Emacs profiler commands."
  [:description (lambda ()
                  (concat "Profiler | "
                          (transient--make-description
                           "CPU"
                           (profiler-running-p))
                          " "
                          (transient--make-description
                           "Memory"
                           (profiler-memory-running-p))))
   ("s" "Start/Reset" profiler-start :transient t)
   ("p" "Report" profiler-report)
   ("e" "Stop" profiler-stop :transient t)
   ]
  )
(global-set-key (kbd "C-c P f") #'transient/profiler)

;; add transient popup for register commands
(transient-define-prefix transient/registers ()
  "Register commands."
  ["Registers"
   [("SPC" "Save point" point-to-register)
    ("w" "Save windows" window-configuration-to-register)
    ("f" "Save frames" frameset-to-register)
    ("j" "Jump" jump-to-register)
    ]
   [("s" "Copy region" copy-to-register)
    ("a" "Append region" append-to-register)
    ("p" "Prepend region" prepend-to-register)
    ("r" "Copy rectangle" copy-rectangle-to-register)
    ]
   [("DEL" "Clear" my-clear-register)
    ("i" "Insert" insert-register)
    ("l" "List" list-registers)
    ("v" "View" view-register)
    ]
   ]
  )
(global-set-key (kbd "C-c R") #'transient/registers)

;; add transient popup for search tools
(transient-define-prefix transient/search ()
  "Search commands."
  ["Search"
   ["Grep"
    ("gr" "Recursive" rgrep)
    ("gz" "Recursive (*.gz)" rzgrep)
    ("gg" "With user args" grep)
    ("gf" "Via find" grep-find)
    ]
   ["Occur in buffers"
    ("oo" "Current" occur)
    ("ob" "Matching" multi-occur-in-matching-buffers)
    ("om" "All" multi-occur)
    ]
   ["Query/Replace"
    ("rs" "String" query-replace)
    ("rr" "Regexp" query-replace-regexp)
    ]
   ["Other"
    ("." "Find defn" xref-find-definitions)
    ("M-." "Apropos defn" xref-find-apropos)
    ("w" "EWW web search" eww)
    ]
   ]
  )
(global-set-key (kbd "C-c S s") #'transient/search)

;; add transient popup for Emacs server management
(transient-define-prefix transient/server ()
  "Emacs server-related commands."
  ;; suffix actions don't exit the transient popup by default
  :transient-suffix 'transient--do-stay
  [:description (lambda ()
                  (transient--make-description
                   "Emacs server-mode"
                   server-mode))
   ("s" "Toggle server-mode" server-mode)
   ("r" "Restart server" restart-emacs-server)
   ("e" "Next server editing buffer" server-edit)
   ("k" "Stop server and delete connection file" server-force-delete)
   ]
  )
(global-set-key (kbd "C-c S v") #'transient/server)

;; add transient popup for shell tools
(with-eval-after-load 'eat
  (transient-define-prefix transient/shell ()
    "Various shell tools."
    ["Shell tools"
     ["Shell"
      ("E" "Eat" eat)
      ("e" "Eshell" my-eshell-with-name)
      ("a" "ANSI Term" ansi-term)
      ]
     ["Send to Eshell"
      ("l" "Line" my-eshell-send-line)
      ("r" "Region" my-eshell-send-region)
      ]
     ["Send to tmux"
      ("tl" "Line" tmux-send-line)
      ("tr" "Region" tmux-send-region)
      ]
     ]
    )
  (global-set-key (kbd "C-c t") #'transient/shell))

(defun transient/system--display-current-datetime ()
  "Display the current time in the minibuffer."
  (interactive)
  (message (format-time-string "%Y-%b-%d %l:%M:%S%p %Z %A")))

(defun transient/system--display-emacs-pid ()
  "Display the process id of current Emacs process in the minibuffer."
  (interactive)
  (message (format "%d" (emacs-pid))))

(defun transient/system--display-emacs-build-config ()
  "Display the Emacs build configuration in the minibuffer."
  (interactive)
  (message (mapconcat 'identity
                      `("Emacs build configuration"
                        ,(format "  Build target:     %s"
                                 system-configuration)
                        ,(format "  Enabled features: %s"
                                 system-configuration-features))
                      "\n")))

;; add transient popup for system process management and info, and
;; Emacs build and runtime info
(transient-define-prefix transient/system ()
  "System process management, general info and Emacs runtime commands."
  ["System, process and Emacs runtime"
   ["Emacs"
    ("eb" "Build config" transient/system--display-emacs-build-config)
    ("ei" "Init time" emacs-init-time)
    ("ep" "Emacs PID" transient/system--display-emacs-pid)
    ("eu" "Uptime" emacs-uptime)
    ("ev" "Version" emacs-version)
    ]
   ["System"
    ("sp" "Proced" proced)
    ("st" "Datetime" transient/system--display-current-datetime :transient t)
    ("sw" "World time" display-time-world)
    ]
   ]
  )
(global-set-key (kbd "C-c S y") #'transient/system)

;; add transient popup for system process management and info, and
;; Emacs build and runtime info
(transient-define-prefix transient/vc ()
  "Version control commands."
  ["Version control"
   ["Commit log"
    ("L" "Root log" vc-print-root-log)
    ("l" "File log" vc-print-log)
    ("g" "File annotate" vc-annotate)
    ("h" "Region history" vc-region-history)
    ("/" "Search" vc-log-search)
    ]
   ["Revision"
    ("D" "Root diff" vc-root-diff)
    ("=" "File diff" vc-diff)
    ("_" "File Ediff" vc-ediff)
    ("~" "File rev other" vc-revision-other-window)
    ]
   ["Other"
    ("d" "Dir" vc-dir)
    ("a" "Update changelog" vc-update-change-log)
    ("SPC" "Refresh file state" vc-refresh-state)
    ("M-c" "Create repo" vc-create-repo)
    ("M-r" "Resolve conflicts" vc-resolve-conflicts)
    ]
   ]
  [
   ["Operations"
    ("v" "Next file action" vc-next-action)
    ("i" "Register file" vc-register)
    ("G" "Ignore file" vc-ignore)
    ("-" "Delete file" vc-delete-file)
    ("R" "Rename file" vc-rename-file)
    ]
   ["Branch"
    ("I" "Pull preview" vc-log-incoming)
    ("O" "Push preview" vc-log-outgoing)
    ("P" "Push" vc-push)
    ("+" "Pull" vc-pull)
    ("m" "Merge" vc-merge)
    ("r" "Switch tag/branch" vc-retrieve-tag)
    ("s" "Create tag/branch" vc-create-tag)
    ("u" "Revert" vc-revert)
    ]
   ]
  )
(global-set-key (kbd "C-c v") #'transient/vc)

;; make sure functions used by visual transient are loaded
(require 'follow)
(require 'hilit-chg)
(require 'hl-line)
(require 'display-line-numbers)
(require 'face-remap)
(require 'whitespace)

(require 'censor)
(require 'too-long-lines-mode)

(defvar-local transient/visual--face-remap-cookies '()
  "Alist storing cookies for `face-remap-add-relative' calls.")

(defun transient/visual--toggle-lighten-face (face)
  "Toggle brightness of FACE color for emphasis or emphasis."
  (let ((face-remap-cookie-old (alist-get
                                face
                                transient/visual--face-remap-cookies)))
    (if face-remap-cookie-old
        (progn
          (face-remap-remove-relative face-remap-cookie-old)
          (setq transient/visual--face-remap-cookies
                (assq-delete-all
                 face
                 transient/visual--face-remap-cookies)))
      (let* ((light-color (color-lighten-name
                           (face-attribute face :foreground)
                           50))         ; lighten color by 50 percent
             (face-remap-cookie-new (face-remap-add-relative
                                     face
                                     :foreground light-color)))
        (push `(,face . ,face-remap-cookie-new)
              transient/visual--face-remap-cookies)))))

(defun transient/visual--toggle-lighten-font-lock-comment-faces ()
  "Toggle brightness of font-lock comment faces.

Font-lock comment faces are `font-lock-comment-face' and
`font-lock-comment-delimiter-face'."
  (interactive)
  (transient/visual--toggle-lighten-face 'font-lock-comment-face)
  (transient/visual--toggle-lighten-face 'font-lock-comment-delimiter-face))

(defun transient/visual--toggle-lighten-font-lock-doc-face ()
  "Toggle brightness of `font-lock-doc-face'."
  (interactive)
  (transient/visual--toggle-lighten-face
   'font-lock-doc-face))

(defun transient/visual--display-toggle-trailing-whitespace ()
  "Toggle the display of trailing whitespace."
  (interactive)
  ;; `show-trailing-whitespace' is buffer-local by default
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (message "show-trailing-whitespace: %s"
           (if show-trailing-whitespace "yes" "no")))

(defun transient/visual--toggle-ligatures ()
  "Toggle ligatures.
Currently only works for Emacs Mac port."
  (interactive)
  (cond ((fboundp 'mac-auto-operator-composition-mode)
         (mac-auto-operator-composition-mode))
        (t (message "Not implemented for this Emacs build."))))

(defun transient/visual--text-scale-reset ()
  "Resets buffer `text-scale-mode-amount' to zero."
  (interactive)
  (text-scale-set 0))

(defun transient/visual--my-pulse-line ()
  "Pulse the point's current line using `my-pulse-line'."
  (interactive)
  (my-pulse-line))

;; add transient popup for visual commands
(transient-define-prefix transient/visual ()
  "Visual commands and toggles."
  :transient-suffix 'transient--do-stay
  ["Visual"
   ["Display"
    ("H" (lambda ()
           (transient--make-description
            "Highlight changes"
            highlight-changes-mode))
     highlight-changes-mode)
    ("l" (lambda ()
           (transient--make-description
            "Line numbers"
            display-line-numbers-mode))
     display-line-numbers-mode)
    ("t" (lambda ()
           (transient--make-description
            "Truncate lines"
            truncate-lines))
     toggle-truncate-lines)
    ("w" (lambda ()
           (transient--make-description
            "Trailing whitespace"
            show-trailing-whitespace))
     transient/visual--display-toggle-trailing-whitespace)
    ("W" (lambda ()
           (transient--make-description
            "Whitespace"
            whitespace-mode))
     whitespace-mode)
    ("L" (lambda ()
           (transient--make-description
            "Hide long lines"
            too-long-lines-mode))
     too-long-lines-mode)
    ("$" (lambda ()
           (concat "Selective display ["
                   (if selective-display
                       (number-to-string selective-display)
                     " ")
                   "]"))
     set-selective-display)
    ("f" (lambda ()
           (transient--make-description
            "Hideshow (folding)"
            hs-minor-mode))
     hs-minor-mode)
    ("P" (lambda ()
           (transient--make-description
            "Prettify symbols"
            prettify-symbols-mode))
     prettify-symbols-mode)
    ("B" (lambda ()
           (transient--make-description
            "Buffer-specific face"
            buffer-face-mode))
     buffer-face-mode)
    ("C-l" "Ligatures" transient/visual--toggle-ligatures)
    ]
   ["Cursor"
    ("b" (lambda ()
           (transient--make-description
            "Blink"
            blink-cursor-mode))
     blink-cursor-mode)
    ("h" (lambda ()
           (transient--make-description
            "Highlight line"
            hl-line-mode))
     hl-line-mode)
    ("p" (lambda ()
           (transient--make-description
            "Show paren"
            show-paren-mode))
     show-paren-mode)
    ("T" (lambda ()
           (transient--make-description
            "Transient mark"
            transient-mark-mode))
     transient-mark-mode)
    ]
   ["Color"
    ("cf" (lambda ()
            (transient--make-description
             "Font locking"
             font-lock-mode))
     font-lock-mode)
    ("cc" (lambda ()
            (transient--make-description
             "Comments"
             (and (null (assq 'font-lock-comment-face
                              transient/visual--face-remap-cookies))
                  (null (assq 'font-lock-comment-delimiter-face
                              transient/visual--face-remap-cookies)))))
     transient/visual--toggle-lighten-font-lock-comment-faces)
    ("cd" (lambda ()
            (transient--make-description
             "Docstrings"
             (null (assq 'font-lock-doc-face
                         transient/visual--face-remap-cookies))))
     transient/visual--toggle-lighten-font-lock-doc-face)
    ]
   ]
  [
   [:description (lambda ()
                   (transient--make-description
                    "Narrow"
                    (buffer-narrowed-p)))
    ("nd" "Defun" narrow-to-defun)
    ("nr" "Region" narrow-to-region)
    ("np" "Page" narrow-to-page)
    ("nw" "Widen" widen)
    ]
   [:description (lambda ()
                   (concat "Zoom ["
                           (if text-scale-mode
                               (number-to-string text-scale-mode-amount)
                             " ")
                           "]"))
    ("+" "Increase" text-scale-increase)
    ("-" "Decrease" text-scale-decrease)
    ("0" "Reset" transient/visual--text-scale-reset)
    ]
   ["Layout"
    ("m" (lambda ()
           (transient--make-description
            "Menu bar"
            menu-bar-mode))
     menu-bar-mode)
    ("s" (lambda ()
           (transient--make-description
            "Vscroll bar"
            (frame-parameter nil 'vertical-scroll-bars)))
     toggle-scroll-bar)
    ("S" (lambda ()
           (transient--make-description
            "Hscroll bar"
            (frame-parameter nil 'horizontal-scroll-bars)))
     toggle-horizontal-scroll-bar)
    ]
   ["Other"
    ("C" (lambda ()
           (transient--make-description
            "Scroll lock"
            (and (boundp 'scroll-lock-mode)
                 scroll-lock-mode)))
     scroll-lock-mode)
    ("v" (lambda ()
           (transient--make-description
            "Visual line"
            visual-line-mode))
     visual-line-mode)
    ("F" (lambda ()
           (transient--make-description
            "Follow"
            follow-mode))
     follow-mode)
    ("x" (lambda ()
           (transient--make-description
            "Censor"
            censor-mode))
     censor-mode)
    ("X" (lambda ()
           (transient--make-description
            "Censor (global)"
            global-censor-mode))
     global-censor-mode)
    ("SPC" "Pulse line" transient/visual--my-pulse-line)
    ]
   ]
  )
(global-set-key (kbd "C-c V") #'transient/visual)

(require 'eww)
(require 'elpher)

;; add transient for launching web browsers
(transient-define-prefix transient/web ()
  "Web launcher commands."
  ["Web launchers"
   ["Emacs Web Wowser"
    ("eo" "Open" eww)
    ("eO" "Open in new buffer" eww-open-in-new-buffer)
    ("ef" "Open file" eww-open-file)
    ("eb" "List buffers" eww-list-buffers)
    ("eB" "List bookmarks" eww-list-bookmarks)
    ("eh" "List histories" eww-list-histories)
    ("es" "Search" eww-search-words)
    ]
   ["Elpher Gopher/Gemini client"
    ("go" "Open" elpher-go)
    ("gh" "Open home" elpher)
    ("gB" "List bookmarks" elpher-show-bookmarks)
    ]
   ])
(global-set-key (kbd "C-c W b") #'transient/web)

;; `next-multiframe-window' & `previous-multiframe-window' renamed to
;; `next-window-any-frame' & `previous-window-any-frame' in Emacs 27
(when (version< emacs-version "27")
  (defalias 'next-window-any-frame 'next-multiframe-window)
  (defalias 'previous-window-any-frame 'previous-multiframe-window))

(defun transient/window--transpose-windows (selector)
  "Call SELECTOR and transpose buffers between current and selected windows."
  (let ((from-win (selected-window))
        (from-buf (window-buffer)))
    (funcall selector)
    (set-window-buffer from-win (window-buffer))
    (set-window-buffer (selected-window) from-buf)))

(defun transient/window--transpose-window-up ()
  "Transpose buffers between current and the window above it."
  (interactive)
  (transient/window--transpose-windows 'windmove-up))
(defun transient/window--transpose-window-down ()
  "Transpose buffers between current and the window below it."
  (interactive)
  (transient/window--transpose-windows 'windmove-down))
(defun transient/window--transpose-window-left ()
  "Transpose buffers between current and the window to its left."
  (interactive)
  (transient/window--transpose-windows 'windmove-left))
(defun transient/window--transpose-window-right ()
  "Transpose buffers between current and the window to its right."
  (interactive)
  (transient/window--transpose-windows 'windmove-right))

(defun transient/window--toggle-dedicated-p ()
  "Toggle whether selected window is dedicated to its displayed buffer."
  (interactive)
  (let* ((win (selected-window))
         (flag (not (window-dedicated-p win))))
    (set-window-dedicated-p win flag)))

;; add transient popup for window commands
(transient-define-prefix transient/window ()
  "Window management commands."
  :transient-suffix 'transient--do-stay
  ["Window"
   ["Navigate"
    ("n" "Next" next-window-any-frame)
    ("p" "Previous" previous-window-any-frame)
    ("o" "Other" other-window)
    ("<up>" "↑" windmove-up)
    ("<down>" "↓" windmove-down)
    ("<left>" "←" windmove-left)
    ("<right>" "→" windmove-right)
    ]
   ["Transpose"
    ("S-<up>" "↑" transient/window--transpose-window-up)
    ("S-<down>" "↓" transient/window--transpose-window-down)
    ("S-<left>" "←" transient/window--transpose-window-left)
    ("S-<right>" "→" transient/window--transpose-window-right)
    ("[" "Rotate bwd" my-rotate-buffers-backward)
    ("]" "Rotate fwd" my-rotate-buffers-forward)
    ]
   ["Layout"
    ("0" "Delete window" delete-window)
    ("1" "Delete other windows" delete-other-windows)
    ("2" "Split horiz" split-window-right)
    ("3" "Split vert" split-window-below)
    ("4" "Kill buffer and window" kill-buffer-and-window)
    ("u" "Winner undo" winner-undo)
    ("r" "Winner redo" winner-redo)
    ]
   ["Resize"
    ("-" "Shrink vert" shrink-window)
    ("^" "Enlarge vert" enlarge-window)
    ("{" "Shrink horiz" shrink-window-horizontally)
    ("}" "Enlarge horiz" enlarge-window-horizontally)
    ("M" "Maximize" maximize-window)
    ("m" "Minimize" minimize-window)
    ("+" "Balance" balance-windows)
    ]
   ]
  [
   ["Other"
    ("d" (lambda ()
           (transient--make-description
            "Dedicate"
            (window-dedicated-p (selected-window))))
     transient/window--toggle-dedicated-p)]]
  )
(global-set-key (kbd "C-c W n") #'transient/window)

;; add transient popup for workspace commands
(transient-define-prefix transient/workspace ()
  "Various workspace commands."
  ["Workspace"
   ["Desktop"
    ("dc" "Clear" desktop-clear)
    ("ds" "Save" desktop-save)
    ("dr" "Read" desktop-read)
    ("dR" "Revert" desktop-revert)
    ("dd" "Change Dir" desktop-change-dir)
    ]
   ["Tabs"
    ("<backtab>" "Previous" tab-bar-switch-to-prev-tab :transient t)
    ("<tab>" "Next" tab-bar-switch-to-next-tab :transient t)
    ("1" "Only" tab-bar-close-other-tabs :transient t)
    ("2" "New" tab-bar-new-tab :transient t)
    ("0" "Close" tab-bar-close-tab :transient t)
    ("u" "Undo close" tab-bar-undo-close-tab :transient t)
    ("r" "Rename" tab-bar-rename-tab :transient t)
    ("m" "Move" tab-bar-move-tab :transient t)
    ("M" "Move (frame)" tab-bar-move-tab-to-frame :transient t)
    ("<return>" "Switch" tab-bar-switch-to-tab)
    ]
   ]
  )
(global-set-key (kbd "C-c W s") #'transient/workspace)

;; add transient popup for writing commands

(defun transient/writing--ispell-dwim ()
  "Dispatch to different Ispell spelling correction commands by major mode.

If the major mode derives from `prog-mode', call interactively
`ispell-comments-and-strings'.

If the major mode derives from `message-mode', call interactively
`ispell-message'.

Otherwise call interactively `ispell'.

Note that `ispell-comments-and-strings' and `ispell-message' do
not support restricting to a region."
  (interactive)
  (let ((fun (cond
              ((derived-mode-p 'prog-mode) #'ispell-comments-and-strings)
              ((derived-mode-p 'message-mode) #'ispell-message)
              (t #'ispell))))
    (call-interactively fun)))

(transient-define-prefix transient/writing ()
  "Writing commands."
  ["Writing"
   ["Spelling"
    ("F" (lambda ()
           (transient--make-description "Flyspell mode"
                                        flyspell-mode))
     flyspell-mode :transient t)
    ("P" "Flyspell prog mode" flyspell-prog-mode :transient t)
    ("B" "Check buffer" flyspell-buffer :transient t)
    ("n" "Next error" flyspell-goto-next-error :transient t)
    ("c" "Correct word" ispell-word :transient t)
    ("C" "Correct buffer/region" transient/writing--ispell-dwim :transient t)
    ("D" "Change dictionary" ispell-change-dictionary :transient t)
    ]
   ]
  )

(global-set-key (kbd "C-c W r") #'transient/writing)

;; Transient commands / Major mode transients

; ;; major-mode specific transient for csv-mode
; (with-eval-after-load 'csv-mode
;   (transient-define-prefix transient/csv-mode ()
;     "`csv-mode' commands."
;     ["CSV"
;      ["Sort"
;       ("s" "Lexicographic" csv-sort-fields)
;       ("n" "Numerically" csv-sort-numeric-fields)
;       ("r" "Reverse" csv-reverse-region)
;       ("d" "Toggle descending" csv-toggle-descending :transient t)
;       ]
;      ["Edit"
;       ("t" "Transpose" csv-transpose)
;       ("k" "Cut" csv-kill-fields)
;       ("y" "Paste" csv-yank-fields)
;       ("z" "Paste as new table" csv-yank-as-new-table)
;       ]
;      ["Visual"
;       ("A" "Align visible" csv-align-visible-fields :transient t)
;       ("a" "Align" csv-align-fields :transient t)
;       ("u" "Unalign" csv-unalign-fields :transient t)
;       ("h" "Toggle header" csv-header-line :transient t)
;       ("v" "Toggle separator" csv-toggle-invisibility :transient t)
;       ]
;      ]
;     )
;   (define-key csv-mode-map (kbd "C-c m") #'transient/csv-mode))

;; major-mode specific transient for debugger-mode
(with-eval-after-load 'debug
  (transient-define-prefix transient/debugger-mode ()
    "`debugger-mode' commands."
    ["Emacs debugger"
     ["Movement"
      ("n" "Next line" next-line :transient t)
      ("p" "Previous line" previous-line :transient t)
      ]
     ["Breakpoints"
      ("b" "Set" debugger-frame)
      ("u" "Unset" debugger-frame-clear)
      ]
     ["Evaluate"
      ("e" "Sexp" debugger-eval-expression)
      ("R" "Sexp and record" debugger-record-expression)
      ]
     ]
    [
     [
      "Stepping"
      ("d" "Step through" debugger-step-through)
      ("c" "Continue" debugger-continue)
      ("j" "Jump" debugger-jump)
      ("q" "Exit" top-level)
      ]
     ["Other"
      ("RET" "Follow at point" backtrace-help-follow-symbol)
      ("r" "Specify return value" debugger-return-value)
      ("l" "List debug functions" debugger-list-functions)
      ("v" "Toggle locals" backtrace-toggle-locals)
      ("h" "Help" describe-mode)
      ]
     ]
    )
  (define-key debugger-mode-map (kbd "C-c m") #'transient/debugger-mode))

;; major-mode specific transient for dired-mode
(with-eval-after-load 'dired
  (defun transient/dired-mode--dired-kill-and-next-subdir ()
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
  (transient-define-prefix transient/dired-mode ()
    "`dired-mode' commands."
    ["Dired"
     ["File open"
      ("RET" "Open" dired-find-file)
      ("o" "Open other" dired-find-file-other-window)
      ("F" "Open marked" dired-do-find-marked-files)
      ("z" "Open external" dired--open-file-at-pt)
      ("v" "View file" dired-view-file)
      ("+" "Create dir" dired-create-directory)
      ("=" "Diff" dired-diff)
      ]
     ["File operations"
      ("C" "Copy" dired-do-copy)
      ("D" "Delete" dired-do-delete)
      ("x" "Delete marked" dired-do-flagged-delete)
      ("S" "Symlink" dired-do-symlink)
      ("Y" "Symlink to" dired-do-relsymlink)
      ("c" "Compress to" dired-do-compress-to)
      ("Z" "Compress" dired-do-compress)
      ]
     ["File modification"
      ("R" "Rename" dired-do-rename)
      ("%R" "Rename by regexp" dired-do-rename-regexp)
      ("G" "chgrp" dired-do-chgrp)
      ("M" "chmod" dired-do-chmod)
      ("O" "chown" dired-do-chown)
      ("T" "Touch" dired-do-touch)
      ("M-r" "Wdired" dired-toggle-read-only)
      ]
     ["Mark"
      ("m" "File at pt" dired-mark :transient t)
      ("E" "By extension" dired-mark-extension :transient t)
      ("t" "Toggle marks" dired-toggle-marks :transient t)
      ("u" "Unmark" dired-unmark :transient t)
      ("U" "Unmark all" dired-unmark-all-marks :transient t)
      ]
     ]
    [
     ["Search/Filter"
      ("A" "Query" dired-do-find-regexp)
      ("Q" "Query-replace" dired-do-find-regexp-and-replace)
      ("{" "Find by name" find-name-dired)
      ("}" "Find by query" find-grep-dired)
      ]
     ["View"
      ("(" "Toggle details" dired-hide-details-mode :transient t)
      (")" "Toggle omit" dired-omit-mode :transient t)
      ("i" "Insert subdir" dired-maybe-insert-subdir :transient t)
      ("K" "Kill subdir" transient/dired-mode--dired-kill-and-next-subdir :transient t)
      ("s" "Sort by date" dired-sort-toggle-or-edit :transient t)
      ]
     ["Other"
      ("y" "Show file type" dired-show-file-type :transient t)
      ("g" "Refresh" revert-buffer :transient t)
      ("l" "Redisplay" dired-do-redisplay :transient t)
      ("C-o" "Display other" dired-display-file)
      ("h" "Help" describe-mode)
      ]
     ]
    )
  (define-key dired-mode-map (kbd "C-c m") #'transient/dired-mode))

;; major-mode specific transient for edebug-mode
(with-eval-after-load 'edebug
  (transient-define-prefix transient/edebug-mode ()
    "`edebug-mode' commands."
    ["Edebug"
     ["Modes"
      ("SPC" "Step" edebug-step-mode)
      ("n" "Next" edebug-next-mode)
      ("g" "Go" edebug-go-mode)
      ("G" "Go (nonstop)" edebug-Go-nonstop-mode)
      ("t" "Trace" edebug-Trace-fast-mode)
      ("c" "Continue" edebug-continue-mode)
      ("C" "Continue (fast)" edebug-Continue-fast-mode)
      ]
     ["Stepping"
      ("f" "Forward sexp" edebug-forward-sexp)
      ("h" "Continue to here" edebug-goto-here)
      ("I" "Instrument callee" edebug-instrument-callee)
      ("i" "Step in" edebug-step-in)
      ("o" "Step out" edebug-step-out)
      ]
     ["Breakpoints"
      ("b" "Set" edebug-set-breakpoint)
      ("u" "Unset" edebug-unset-breakpoint)
      ("B" "Next" edebug-next-breakpoint)
      ("x" "Set (cond-at-pt)" edebug-set-conditional-breakpoint)
      ("X" "Set (global cond)" edebug-set-global-break-condition)
      ]
     ]
    [
     ["Evaluation"
      ("r" "Previous result" edebug-previous-result)
      ("e" "Sexp" edebug-eval-expression)
      ("C-e" "Last sexp" edebug-eval-last-sexp)
      ("E" "Visit eval list" edebug-visit-eval-list)
      ]
     ["Views"
      ("v" "Outside" edebug-view-outside)
      ("w" "Where" edebug-where)
      ("p" "Bounce point" edebug-bounce-point)
      ("W" "Toggle save windows" edebug-toggle-save-windows)
      ]
     ["Quitting/Stopping"
      ("q" "Top level" top-level)
      ("Q" "Top level (nonstop)" edebug-top-level-nonstop)
      ("a" "Abort recursive edit" abort-recursive-edit)
      ("S" "Stop" edebug-stop)
      ]
     ]
    [
     ["Other"
      ("d" "Backtrace" edebug-pop-to-backtrace)
      ("=" "Frequency count" edebug-temp-display-freq-count)
      ("?" "Help" edebug-help)
      ]
     ]
    )
  (define-key edebug-mode-map (kbd "C-c m") #'transient/edebug-mode))

;; major-mode specific transient for eww-mode
(with-eval-after-load 'eww
  (transient-define-prefix transient/eww-mode ()
    "`eww-mode' commands."
    ["Emacs Web Wowser"
     ["Navigation"
      ("G" "Search" eww)
      ("o" "Open file" eww-open-file)
      ("l" "Back" eww-back-url)
      ("r" "Forward" eww-forward-url)
      ("g" "Reload" eww-reload)
      ("s" "Switchb" eww-switch-to-buffer)
      ("S" "Buffers" eww-list-buffers)
      ("H" "History" eww-list-histories)
      ]
     ["Bookmarks"
      ("b" "Add" eww-add-bookmark)
      ("B" "List" eww-list-bookmarks)
      ]
     ["Toggle"
      ("F" "Fonts" eww-toggle-fonts :transient t)
      ("I" "Images" eww--toggle-images :transient t)
      ("M-C" "Colors" eww-toggle-colors :transient t)
      ("D" "Text direction" eww-toggle-paragraph-direction :transient t)
      ]
     ["Other"
      ("d" "Downlink link" eww-download)
      ("w" "Copy url" eww-copy-page-url)
      ("R" "View readable" eww-readable)
      ("v" "View source" eww-view-source)
      ("C" "Cookies" url-cookie-list)
      ("&" "Open external" eww-browse-with-external-browser)
      ]
     ]
    )
  (define-key eww-mode-map (kbd "C-c m") #'transient/eww-mode))

(transient-define-prefix transient/ibuffer-mode/mark ()
  "`ibuffer-mode' mark commands."
  :transient-suffix 'transient--do-stay
  ["Ibuffer → Mark"
   [("*" "Unmark all" ibuffer-unmark-all)
    ("M" "By mode" ibuffer-mark-by-mode)
    ("m" "Modified" ibuffer-mark-modified-buffers)
    ("u" "Unsaved" ibuffer-mark-unsaved-buffers)
    ]
   [("s" "Special" ibuffer-mark-special-buffers)
    ("r" "Read-only" ibuffer-mark-read-only-buffers)
    ("/" "Dired" ibuffer-mark-dired-buffers)
    ("e" "Disassociated" ibuffer-mark-dissociated-buffers)
    ]
   [("h" "Help" ibuffer-mark-help-buffers)
    ("z" "Compressed" ibuffer-mark-compressed-file-buffers)
    ]
   ]
  )

(transient-define-prefix transient/ibuffer-mode/action ()
  "`ibuffer-mode' action commands."
  ["Ibuffer → Action"
   ["Run"
    ("E" "Eval in buffers" ibuffer-do-eval)
    ("W" "View buffers and eval" ibuffer-do-view-and-eval)
    ("F" "Command on files" ibuffer-do-shell-command-file)
    ("X" "Pipe to command" ibuffer-do-shell-command-pipe)
    ("N" "Pipe to command and replace" ibuffer-do-shell-command-pipe-replace)
    ]
   ["Search"
    ("O" "Occur" ibuffer-do-occur)
    ("U" "Replace regexp" ibuffer-do-replace-regexp)
    ("Q" "Query/Replace" ibuffer-do-query-replace)
    ("I" "Query/Replace regexp" ibuffer-do-query-replace-regexp)
    ]
   ["Properties"
    ("R" "Rename uniquely" ibuffer-do-rename-uniquely)
    ("M" "Toggle modified" ibuffer-do-toggle-modified)
    ("T" "Toggle read-only" ibuffer-do-toggle-read-only)
    ]
   ]
  [
   ["Other"
    ("A" "View" ibuffer-do-view)
    ("H" "View (other)" ibuffer-do-view-other-frame)
    ("V" "Revert" ibuffer-do-revert)
    ("P" "Print" ibuffer-do-print)
    ]
   ]
  )

(transient-define-prefix transient/ibuffer-mode/sort ()
  "`ibuffer-mode' sort commands."
  :transient-suffix 'transient--do-stay
  ["Ibuffer → Sort"
   [("a" "Alphabetic" ibuffer-do-sort-by-alphabetic)
    ("f" "Filename/Process" ibuffer-do-sort-by-filename/process)
    ("m" "Mode" ibuffer-do-sort-by-major-mode)
    ]
   [("s" "Size" ibuffer-do-sort-by-size)
    ("v" "Recency" ibuffer-do-sort-by-recency)
    ("i" "Invert" ibuffer-invert-sorting)
    ]
   ]
  )

(transient-define-prefix transient/ibuffer-mode/filter ()
  "`ibuffer-mode' filter commands."
  :transient-suffix 'transient--do-stay
  ["Ibuffer → Filter"
   ["Predicates"
    ("a" "Add saved" ibuffer-add-saved-filters)
    ("c" "By content" ibuffer-filter-by-content)
    ("e" "By predicate" ibuffer-filter-by-predicate)
    ("f" "By filename" ibuffer-filter-by-filename)
    ("m" "By mode" ibuffer-filter-by-used-mode)
    ("M" "By derived mode" ibuffer-filter-by-derived-mode)
    ("n" "By name" ibuffer-filter-by-name)
    (">" "By size gt" ibuffer-filter-by-size-gt)
    ("<" "By size lt" ibuffer-filter-by-size-lt)
    ]
   ["Operators"
    ("&" "AND" ibuffer-and-filter)
    ("|" "OR" ibuffer-or-filter)]
   ["Stack"
    ("p" "Pop" ibuffer-pop-filter)
    ("\\" "Clear" ibuffer-clear-filter-groups)
    ]
   ["Presets"
    ("R" "Saved" ibuffer-switch-to-saved-filter-groups)
    ("/" "Disable" ibuffer-filter-disable)
    ]
   ]
  )

(defun transient/ibuffer-mode--activate-dwim ()
  "Toggle filter group or visit buffer under point in `ibuffer-mode'."
  (interactive)
  (condition-case nil
             (ibuffer-toggle-filter-group)
           (error (ibuffer-visit-buffer))))

;; major-mode specific transient for ibuffer-mode
(transient-define-prefix transient/ibuffer-mode ()
  "`ibuffer-mode' commands."
  :transient-suffix 'transient--do-stay
  ["Ibuffer"
   ["Navigation"
    ("n" "Next line" ibuffer-forward-line)
    ("p" "Previous line" ibuffer-backward-line)
    ("RET" "Open" transient/ibuffer-mode--activate-dwim :transient nil)
    ("o" "Open (other)" ibuffer-visit-buffer-other-window :transient nil)
    ]
   ["Actions"
    ("m" "Mark" ibuffer-mark-forward)
    ("u" "Unmark" ibuffer-unmark-forward)
    ("*" "→ Mark" transient/ibuffer-mode/mark)
    ("S" "Save" ibuffer-do-save)
    ("D" "Delete" ibuffer-do-delete)
    ("a" "→ Action" transient/ibuffer-mode/action)
    ]
   ["View"
    ("`" "Switch format" ibuffer-switch-format)
    ("g" "Refresh" ibuffer-update)
    ("s" "→ Sort" transient/ibuffer-mode/sort)
    ("/" "→ Filter" transient/ibuffer-mode/filter)
    ]
   ]
  )
(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "C-c m") #'transient/ibuffer-mode))

;; major-mode specific transient for json-mode
(with-eval-after-load 'json-mode
  (transient-define-prefix transient/json-mode ()
    "`json-mode' commands."
    :transient-suffix 'transient--do-stay
    ["JSON mode"
     ("p" "Copy path" json-mode-show-path)
     ("b" "Toggle bool value" json-toggle-boolean)
     ("i" "Increment num" json-increment-number-at-point)
     ("d" "Decrement num" json-decrement-number-at-point)
     ("k" "Nullify sexp" json-nullify-sexp)
     ("B" "Beautify" json-mode-beautify)
     ("R" "Reformat (jq)" json-jq-format-buffer-or-region)
     ]
    )
  (define-key json-mode-map (kbd "C-c m") #'transient/json-mode))

;; major-mode specific transient for markdown-mode
(with-eval-after-load 'markdown-mode
  (transient-define-prefix transient/markdown-mode ()
    "`markdown-mode' commands."
    :transient-suffix 'transient--do-stay
    ["Markdown mode (other bindings: 'C-c C-c', 'C-c C-s', 'C-c C-x')"
     ["Navigate"
      ("n" "Next" markdown-outline-next)
      ("p" "Previous" markdown-outline-previous)
      ("f" "Next (same level)" markdown-outline-next-same-level)
      ("b" "Previous (same level)" markdown-outline-previous-same-level)
      ]
     ["Move outline"
      ("<left>" "Promote" markdown-promote)
      ("<right>" "Demote" markdown-demote)
      ("<up>" "Move up" markdown-move-up)
      ("<down>" "Move down" markdown-move-down)
      ]
     ["Shift region"
      ("<" "Outdent" markdown-outdent-region)
      (">" "Indent" markdown-indent-region)
      ]
     ]
    [
     ["User interface"
      ("E" "Toggle math" markdown-toggle-math)
      ("F" "Toggle code font" markdown-toggle-fontify-code-blocks-natively)
      ("I" "Toggle images" markdown-toggle-inline-images)
      ("L" "Toggle show URL" markdown-toggle-url-hiding)
      ("M" "Toggle show markup" markdown-toggle-markup-hiding)
      ]
     ["Other"
      ("d" "Do" markdown-do :transient nil)
      ("o" "Follow" markdown-follow-thing-at-point :transient nil)
      ("N" "Cleanup list nums" markdown-cleanup-list-numbers :transient nil)
      ("'" "Edit code block" markdown-edit-code-block :transient nil)
      ]
     ]
    )
  (define-key gfm-mode-map (kbd "C-c m") #'transient/markdown-mode)
  (define-key markdown-mode-map (kbd "C-c m") #'transient/markdown-mode))

;; major-mode specific transient for python-mode
(with-eval-after-load 'python
  ;; technically also depends on reformatter but more correctly
  ;; using it to define `python-black-format-buffer-or-region' and
  ;; `python-black-format-on-save-mode'
  (transient-define-prefix transient/python-mode ()
    "`python-mode' commands."
    ["Python"
     ["REPL"
      ("p" "Start" run-python)
      ("s" "Send string" python-shell-send-string)
      ("x" "Send defun" python-shell-send-defun)
      ("r" "Send region" python-shell-send-region)
      ("c" "Send buffer" python-shell-send-buffer)
      ("l" "Send file" python-shell-send-file)
      ("z" "Switch to" python-shell-switch-to-shell)
      ]
     ["Format"
      ("TAB" "Fill paragraph" python-fill-paragraph :transient t)
      ("<" "Indent left" python-indent-shift-left :transient t)
      (">" "Indent right" python-indent-shift-right :transient t)
      ("y" "Region or buffer" python-black-format-buffer-or-region)
      ("Y" (lambda ()
             (interactive)
             (transient--make-description
              "Buffer on save"
              python-black-format-on-save-mode))
       python-black-format-on-save-mode :transient t)
      ]
     ["Other"
      ("j" "Imenu" imenu)
      ("v" "Check error" python-check)
      ("f" "Symbol quick help" python-eldoc-at-point)
      ("d" "Symbol describe" python-describe-at-point)
      ("D" "Python debugger" pdb)
      ]
     ]
    )
  (define-key python-mode-map (kbd "C-c m") #'transient/python-mode))

;; major-mode specific transient for smerge-mode
(with-eval-after-load 'smerge-mode
  (transient-define-prefix transient/smerge-mode ()
    "`smerge-mode' commands."
    ;; have suffixes not exit the transient by default
    :transient-suffix 'transient--do-stay
    ["Smerge"
     ["Move"
      ("n" "Next" smerge-next)
      ("p" "Previous" smerge-prev)]
     [
      "Keep"
      ("b" "Base" smerge-keep-base)
      ("u" "Upper" smerge-keep-upper)
      ("l" "Lower" smerge-keep-lower)
      ("a" "All" smerge-keep-all)
      ("RET" "Current" smerge-keep-current)
      ]
     ["Diff"
      ("<" "Upper/Base" smerge-diff-base-upper)
      ("=" "Upper/Lower" smerge-diff-upper-lower)
      (">" "Base/Lower" smerge-diff-base-lower)
      ("R" "Refine" smerge-refine)
      ("E" "Ediff" smerge-ediff)
      ]
     [
      "Other"
      ("C" "Combine" smerge-combine-with-next)
      ("r" "Resolve" smerge-resolve)
      ("k" "Kill current" smerge-kill-current)
      ;; emulate Vim's "ZZ" command to save and close current file
      ("ZZ" "Save and bury buffer" my-save-and-bury-buffer
       :transient nil)
      ]
     ]
    )
  (define-key smerge-mode-map (kbd "C-c m") #'transient/smerge-mode))

;; major-mode specific transient for term-mode
(with-eval-after-load 'term
  (defun transient/term-mode--toggle-char-mode-line-mode ()
    "Toggle between `term-char-mode' and `term-line-mode' in `term-mode'."
    (interactive)
    (if (term-in-line-mode)
        (progn (term-char-mode) (message "line → char"))
      (progn (term-line-mode) (message "char → line"))))
  (transient-define-prefix transient/term-mode ()
    "`term-mode' commands."
    ["Term"
     ("m" "Toggle between `term-char-mode' and `term-line-mode'"
      transient/term-mode--toggle-char-mode-line-mode :transient t)
     ]
    )
  (define-key term-mode-map (kbd "C-c m") #'transient/term-mode)
  (define-key term-raw-map (kbd "C-c m") #'transient/term-mode))

;; major-mode specific transient for ztreedir-mode
(with-eval-after-load 'ztree-dir
  (transient-define-prefix transient/ztreedir-mode ()
    "`ztreedir-mode' commands."
    :transient-suffix 'transient--do-stay
    ["Ztree directory"
     ["Movement"
      ("n" "Next" ztree-next-line)
      ("p" "Previous" ztree-previous-line)
      ("DEL" "Up directory" ztree-move-up-in-tree)
      ]
     ["Actions"
      ("RET" "Hard action" ztree-perform-action :transient nil)
      ("SPC" "Soft action" ztree-perform-soft-action :transient nil)
      ("x" "Expand subtree" ztree-toggle-expand-subtree)
      ("d" "Dired at point" ztree-dir-open-dired-at-point :transient nil)
      ]
     ["View"
      ("g" "Refresh" ztree-refresh-buffer)
      ("H" "Toggle show filtered files" ztree-dir-toggle-show-filtered-files)
      (">" "Narrow tree" ztree-dir-narrow-to-dir)
      ("<" "Widen tree" ztree-dir-widen-to-parent)
      ]
     ]
    )
  (define-key ztreedir-mode-map (kbd "C-c m") #'transient/ztreedir-mode))

;; major-mode specific transient for ztreediff-mode
(with-eval-after-load 'ztree-diff
  (transient-define-prefix transient/ztreediff-mode ()
    "`ztreediff-mode' commands."
    :transient-suffix 'transient--do-stay
    ["Ztree difference"
     ["Movement"
      ("n" "Next" ztree-next-line)
      ("p" "Previous" ztree-previous-line)
      ("TAB" "Jump side" ztree-jump-side)
      ("DEL" "Up directory" ztree-move-up-in-tree)
      ]
     ["Actions"
      ("RET" "Hard action" ztree-perform-action :transient nil)
      ("SPC" "Soft action" ztree-perform-soft-action :transient nil)
      ("x" "Expand subtree" ztree-toggle-expand-subtree)
      ("d" "Diff files" ztree-diff-simple-diff-files :transient nil)
      ("v" "View file" ztree-diff-view-file)
      ("C" "Copy" ztree-diff-copy)
      ("D" "Delete" ztree-diff-delete-file)
      ]
     ["View"
      ("g" "Refresh" ztree-refresh-buffer)
      ("r" "Rescan (partial)" ztree-diff-partial-rescan)
      ("R" "Rescan (full)" ztree-diff-full-rescan)
      ("h" "Toggle show equal files" ztree-diff-toggle-show-equal-files)
      ("H" "Toggle show filtered files" ztree-diff-toggle-show-filtered-files)
      ]
     ]
    )
  (define-key ztreediff-mode-map (kbd "C-c m") #'transient/ztreediff-mode))

;; Transient commands / Minor mode transients

;; add transient for Flymake
(with-eval-after-load 'flymake
  (with-eval-after-load 'flymake-quickdef
    (require 'flymake-proc)
    (defun transient/flymake-mode--flymake-devskim-toggle ()
      "Wrapper for calling `flymake-devskim-toggle' if defined."
      (interactive)
      (call-interactively #'flymake-devskim-toggle))
    (transient-define-prefix transient/flymake-mode ()
      "`flymake-mode' commands."
      :transient-suffix 'transient--do-stay
      [:description (lambda ()
                      (transient--make-description
                       "Flymake"
                       flymake-mode))
       ["Error"
        ("n" "Next" flymake-goto-next-error)
        ("p" "Previous" flymake-goto-prev-error)
        ("l" "List" my-toggle-flymake-diagnostics :transient nil)
        ]
       ["Check"
        ("c" "Start" flymake-start)
        ("k" "Stop" flymake-proc-stop-all-syntax-checks)
        ]
       ["Other"
        ("m" "Toggle mode" flymake-mode)
        ("r" "Reporting backends" flymake-reporting-backends)
        ("d" "Disabled backends" flymake-disabled-backends)
        ("L" "Log" flymake-switch-to-log-buffer)
        ("C" "Compile (no check)" flymake-proc-compile)
        ("D" (lambda ()
               (transient--make-description
                "Devskim"
                (memq 'flymake-devskim-backend flymake-diagnostic-functions)))
         transient/flymake-mode--flymake-devskim-toggle)
        ]
       ]
      ))
  (global-set-key (kbd "C-c F m") #'transient/flymake-mode))

(provide 'init)
;;; init.el ends here
