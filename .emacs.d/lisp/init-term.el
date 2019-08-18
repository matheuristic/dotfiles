;;; init-term.el --- Emacs config shell layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Configure eshell and term

;;; Code:

(require 'init-ui-hydra)

;; make shell prompts read-only
(setq comint-prompt-read-only t)

;; kill eshell and term buffers when their sessions end
(defun my-term-handle-exit--term-kill-buffer-on-exit (&rest args)
  "Kill eshell or term buffer on session exit."
  (kill-buffer))
(advice-add 'term-handle-exit :after #'my-term-handle-exit--term-kill-buffer-on-exit)

;; Eshell
(use-package eshell
  :ensure nil ;; built-in
  :commands (eshell eshell-command)
  :bind ("C-c s-e s" . my-eshell-with-name)
  :init
  (setq eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t
        eshell-where-to-jump 'begin)
  ;; adapted from https://arte.ebrahimi.org/blog/named-eshell-buffers
  (defun my-eshell-with-name ()
    "Open new or switch to existing named eshell buffer."
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
        (progn
          (eshell 42)
          (rename-buffer (concat "*eshell*<" my-es-buf-name ">"))))))
  :config
  (require 'em-term)
  (require 'em-smart)
  ;; run visual commands in term sessions
  (dolist (cmd '("htop" "lftp" "ssh" "tail" "watch" "vim"))
    (add-to-list 'eshell-visual-commands cmd))
  (dolist (subcmd '(("git" "log" "diff" "show")
                    ("sudo" "vi" "vim")
                    ("vagrant" "ssh")))
    (add-to-list 'eshell-visual-subcommands subcmd)))

;; term
(use-package term
  :ensure nil ;; built-in
  :commands (ansi-term term)
  :bind (:map term-mode-map
         ("C-c s-m" . my-hydra/term/body)
         :map term-raw-map
         ("C-c s-m" . my-hydra/term/body))
  :config (defhydra my-hydra/term (:color teal :columns 4)
            "Term"
            ("m" (lambda () (interactive)
                   (if (term-in-line-mode)
                       (progn (term-char-mode) (message "line → char"))
                     (progn (term-line-mode) (message "char → line")))) "toggle-mode")
            ("q" nil "quit" :exit t)))

(provide 'init-term)

;;; init-term.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
