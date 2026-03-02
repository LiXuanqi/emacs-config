;;; xq-lang-go.el --- Go language defaults -*- lexical-binding: t; -*-

(with-eval-after-load 'project
  (add-to-list 'project-vc-extra-root-markers "go.mod"))

(defun xq/lang-go-setup ()
  "Enable Go defaults and lsp-mode in current buffer."
  (setq-local tab-width 4
              indent-tabs-mode t)
  (when (fboundp 'lsp-deferred)
    (lsp-deferred)))

(provide 'xq-lang-go)
;;; xq-lang-go.el ends here
