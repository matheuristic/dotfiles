;; (require 'yasnippet)
;; (require 'yasnippet-snippets)

;; helper function for function_docstring_google snippet
;; adapted from: https://emacs.stackexchange.com/questions/19422/library-for-automatically-inserting-python-docstring-in-google-style
(defun python-args-to-docstring-google (text)
  "Return docstring format for the python arguments in text"
  (let* ((indent (concat "\n" (make-string (+ (current-column) python-indent-offset) ?\ )))
         (args (python-split-args text))
         (nr 0)
         (format-arg (lambda(arg)
                       (concat (nth 0 arg)
                               (format " (${%d:type})" (cl-incf nr))
                               ":"
                               (format " ${%d:description}" (cl-incf nr))
                               (if (nth 1 arg) (concat "  \(default " (nth 1 arg) "\)") ""))))
         (formatted-params (mapconcat format-arg args indent)))
    (unless (string= formatted-params "")
      (mapconcat 'identity
                 (list "Args:" formatted-params)
                 indent))))
