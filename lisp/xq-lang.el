;;; xq-lang.el --- Language and LSP entrypoint -*- lexical-binding: t; -*-

(use-package eglot
  :defer t
  :init
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer"))))

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

(provide 'xq-lang)
;;; xq-lang.el ends here
