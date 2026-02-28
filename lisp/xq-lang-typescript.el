;;; xq-lang-typescript.el --- TypeScript language defaults -*- lexical-binding: t; -*-

(defun xq/lang-typescript-setup ()
  "Enable TypeScript defaults and lsp-mode in current buffer."
  (setq-local tab-width 2)
  (when (fboundp 'lsp-deferred)
    (lsp-deferred)))

(provide 'xq-lang-typescript)
;;; xq-lang-typescript.el ends here
