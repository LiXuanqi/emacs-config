;;; xq-format.el --- Format-on-save defaults -*- lexical-binding: t; -*-

(use-package apheleia
  :defer t
  :init
  (setq apheleia-log-only-errors t)
  :config
  ;; Keep Python formatting predictable and explicit.
  (setf (alist-get 'ruff-format apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath "-")
        (alist-get 'python-mode apheleia-mode-alist) 'ruff-format
        (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff-format))

(defun xq/format-enable-python ()
  "Enable Apheleia format-on-save for Python buffers."
  (when (fboundp 'apheleia-mode)
    (apheleia-mode 1)))

(add-hook 'python-mode-hook #'xq/format-enable-python)
(add-hook 'python-ts-mode-hook #'xq/format-enable-python)

(provide 'xq-format)
;;; xq-format.el ends here
