;;; xq-lang.el --- Language and LSP entrypoint -*- lexical-binding: t; -*-

(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-log-io nil
        lsp-file-watch-threshold 10000))

(use-package lsp-pyright
  :after lsp-mode
  :defer t)

(use-package rust-mode
  :defer t
  :mode "\\.rs\\'")

;; Lazy-load language-specific setup only when the language mode starts.
(autoload 'xq/lang-python-setup "xq-lang-python" nil t)
(add-hook 'python-mode-hook #'xq/lang-python-setup)
(add-hook 'python-ts-mode-hook #'xq/lang-python-setup)

(autoload 'xq/lang-rust-setup "xq-lang-rust" nil t)
(add-hook 'rust-mode-hook #'xq/lang-rust-setup)
(add-hook 'rust-ts-mode-hook #'xq/lang-rust-setup)

(autoload 'xq/lang-typescript-setup "xq-lang-typescript" nil t)
(add-hook 'typescript-mode-hook #'xq/lang-typescript-setup)
(add-hook 'typescript-ts-mode-hook #'xq/lang-typescript-setup)
(add-hook 'tsx-ts-mode-hook #'xq/lang-typescript-setup)

(provide 'xq-lang)
;;; xq-lang.el ends here
