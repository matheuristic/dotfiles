;;; init-master.el --- Emacs config master file -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Master file for coordinating Emacs configuration

;; Best possible startup times can be measured using in Linux
;; $ emacs -q --eval='(message "%s" (emacs-init-time))'
;; or in Mac OS X
;; $ open -n /Applications/Emacs.app --args -q --eval='(message "%s" (emacs-init-time))'

;; Startup time can be optimized using the following steps:
;; 1. profile using the `esup' package ("M-x esup")
;; 2. defer loading of packages when possible, e.g. use-package's `:defer N'
;;    with N=1 sec for important packages and N=2 for less important ones
;; 3. avoid helper functions that cause eager loads

;; ~/.emacs.d/init.el should source this file, e.g.

;; ---
;; ;; user packages in ~/.emacs.d/lisp
;; (defvar lisp-dir (expand-file-name "lisp" user-emacs-directory))
;; (unless (file-exists-p lisp-dir) (make-directory lisp-dir))
;; (add-to-list 'load-path lisp-dir)
;; (dolist (project (directory-files lisp-dir t "\\w+"))
;;   (if (file-directory-p project) (add-to-list 'load-path project)))
;;
;; ;; third-party packages in ~/.emacs.d/site-lisp and its subdirectories
;; (defvar site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
;; (unless (file-exists-p site-lisp-dir) (make-directory site-lisp-dir))
;; (add-to-list 'load-path site-lisp-dir)
;; (dolist (project (directory-files site-lisp-dir t "\\w+"))
;;   (if (file-directory-p project) (add-to-list 'load-path project)))
;;
;; (require 'init-master)
;; ---

;;; Code:

;; optimizations for reducing startup time (reverted at the end of file)
;; * increase garbage collection thresholds
;; * set file-name-handler-alist to nil as it is scanned when files are loaded
(defvar tmp-file-name-handler-alist file-name-handler-alist) ;; save to tmp var
(setq gc-cons-threshold (* 50 1000 1000) ;; in bytes, default is 800k
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(defgroup init-master-el nil
  "Basic settings."
  :group 'convenience)

(defcustom init-master-backup-dir "~/.backup"
  "Path to backups directory."
  :type 'string
  :group 'init-master-el)

(defcustom init-master-regenerate-outdated-bytecode nil
  "Whether to automatically regenerate outdated bytecode."
  :type 'boolean
  :group 'init-master-el)

;; always store Customize settings in a separate file
;; default to ~/.emacs/custom.el if none is specified
(when (or (not (boundp 'custom-file))
          (not custom-file))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

;; set backup directory
(when init-master-backup-dir
  (when (not (file-directory-p init-master-backup-dir))
    (make-directory init-master-backup-dir t))
  (setq backup-directory-alist `(("." . ,init-master-backup-dir))
        version-control t ;; use version numbers for backups
        kept-new-versions 3 ;; number of newest versions to keep
        kept-old-versions 0 ;; number of oldest versions to keep
        delete-old-versions t ;; don't ask before deleting old backups
        backup-by-copying t)) ;; backup by copying instead of renaming

;; regenerate outdated bytecode
(when init-master-regenerate-outdated-bytecode
  (setq load-prefer-newer t))

;; use package.el with given ELPA-compatible package repositories
(setq package-enable-at-startup nil
      package-archives '(("GNU"          . "https://elpa.gnu.org/packages/")
                         ("MELPA Stable" . "https://stable.melpa.org/packages/")
                         ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities '(("GNU"          . 10)
                                   ("MELPA Stable" . 5)
                                   ("MELPA"        . 0)))

(require 'package)
(package-initialize)

;; bootstrap use-package, provides configuration macros
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; preload use-package and bind-key
(eval-when-compile
  (require 'use-package)
  (require 'bind-key)
  (setq use-package-always-ensure t))

;; copies env vars from shell
(if (eq system-type 'darwin)  ;; only for Mac OS X GUI mode
    (use-package exec-path-from-shell
      :init (if (memq window-system '(mac ns))
                (exec-path-from-shell-initialize))))

;; customize how mode names appear in the mode line
(use-package delight)

;; visit large files without loading it entirely
(use-package vlf
  :config (require 'vlf-setup))

;; sensitive files layer
(require 'init-sensitive)

;; UI layer
(require 'init-ui)

;; syntax checking layer
(require 'init-syntax)

;; Dired layer
(require 'init-dired)

;; terminal emulation layer
(require 'init-term)

;; version control layer
(require 'init-vc)

;; project management layer
(require 'init-proj)

;; filetypes layers
(require 'init-org)
(require 'init-lang)

;; zettelkasten layer
(require 'init-zettel)

;; revert earlier optimizations for reducing startup time
(setq gc-cons-threshold (* 2 1000 1000)
      gc-cons-percentage 0.1
      file-name-handler-alist tmp-file-name-handler-alist)
(makunbound 'tmp-file-name-handler-alist) ;; unbind tmp var

;; load Customize settings
(load custom-file 'noerror)

(provide 'init-master)

;;; init-master.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
