;;; censor.el --- Minor mode for censoring buffer content -*- lexical-binding: t; -*-

;; Modified work copyright (c) 2020 matheuristic
;; Original work copyright (c) 2020 Abhinav Tushar

;; Author: matheuristic
;; Version: 0.1
;; Package-Requires: (emacs "26")
;; Keywords: buffers, censor, overlays
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

;; Minor mode for censoring buffer content.
;;
;; `censor-mode' toggles censoring of the current buffer.
;;
;; `global-censor-mode' toggles censoring of all buffers that match
;; the regexps or satisfy the predicate functions in `censor-include'.
;;
;; Censoring is purely visual, so yanking text works normally. This
;; mode is useful in screen sharing or pair coding sessions to ensure
;; content in sensitive buffers is hidden should they get displayed.
;;
;; censor.el is forked from conceal https://github.com/lepisma/conceal
;; and modified to support buffer-local censoring in addition to the
;; global buffer censoring from the original package, and to support a
;; `recentf-exclude' style definition of what buffers to censor when
;; the global minor mode is enabled.
;;
;; This file is not a part of GNU Emacs.

;;; Code:

(require 'overlay)

(defgroup censor nil
  "Options concerning the censor minor mode."
  :tag "Censor"
  :group 'appearance
  :group 'convenience
  :prefix "censor-"
  :link '(url-link :tag "GitHub repository"
           "https://github.com/matheuristic/emacs-config")
  :link '(emacs-commentary-link :tag "Commentary" "censor"))

(defcustom censor-replace-string "▇"
  "String used for censoring each character of the original text.
Applied when `censor-mode' is enabled and buffer satisfies
`censor-buffer-p'."
  :group 'censor
  :type 'string)

(defcustom censor-include '("\\.gpg$")
  "List of regexps and predicates for buffers to be censored.
Applied when `global-censor-mode' is enabled. When a buffer
satisfies any of the predicates or has a name matching any of the
regexps or it is included in the globale censor list. A predicate
is a function that is passed a buffer to check and that must
return non-nil to include it in the censor list."
  :group 'censor
  :type '(repeat (choice regexp function)))

(defconst censor-case-fold-search
  (memq system-type '(windows-nt cygwin))
  "Non-nil if censor buffer name regexp checks should ignore case.")

(defun censor-buffer-p (buffer)
  "Return non-nil if BUFFER is to be censored when `global-censor-mode' is on.
That is, if it matches any of the `censor-include' checks."
  (let ((case-fold-search censor-case-fold-search)
        (checks censor-include)
        (censorit nil))
    (while (and checks (not censorit))
      (let ((check (car checks)))
        ;; If predicate function throws an error, bias toward not
        ;; censoring the buffer
        (setq censorit (ignore-errors
                         (if (stringp check)
                             ;; regexp
                             (string-match check
                                           (buffer-file-name buffer))
                           ;; predicate
                           (funcall check buffer)))
              checks (cdr checks))))
    censorit))

(defvar-local censor-overlays nil
  "Buffer local variable holding the line overlays.")

(defun censor-text (text)
  "Return censored TEXT by replacing all characters with `censor-replace-string'."
  (replace-regexp-in-string "[[:graph:]]" censor-replace-string text))

(defun censor-current-line ()
  "Hide current line and return overlay."
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (ov (make-overlay start end)))
    (overlay-put ov 'display (censor-text (buffer-substring-no-properties start end)))
    ov))

(defun censor-current-buffer ()
  "Censor the current buffer."
  (let ((ovs))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (push (censor-current-line) ovs)
        (forward-line 1)))
    (setq censor-overlays ovs)))

(defun censor-clear-buffer ()
  "Uncensor the current buffer."
  (dolist (ov censor-overlays)
    (delete-overlay ov))
  (setq censor-overlays nil))

(defun censor-mode-enable ()
  "Censors the current buffer if it satisfies `censor-buffer-p'."
  (let ((buffer (current-buffer)))
    (with-current-buffer buffer
      (censor-current-buffer))))

(defun censor-mode-disable ()
  "Uncensors the current buffer it is censored."
  (let ((buffer (current-buffer)))
    (with-current-buffer buffer
      (censor-clear-buffer))))

;;;###autoload
(define-minor-mode censor-mode
  "Buffer-local minor mode to censor content in specific buffers."
  :lighter " Censor"
  :init-value nil
  :global nil
  (if censor-mode (censor-mode-enable) (censor-mode-disable)))

;;;###autoload
(define-globalized-minor-mode global-censor-mode censor-mode censor--on)

(defun censor--on ()
  "Called in each buffer when `global-censor-mode' is enabled."
  (unless (or (minibufferp)
              (and (daemonp) (null (frame-parameter nil 'client)))
              (not (censor-buffer-p (current-buffer))))
    (censor-mode 1)))

(provide 'censor)

;;; censor.el ends here
