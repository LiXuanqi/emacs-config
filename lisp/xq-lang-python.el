;;; xq-lang-python.el --- Python language defaults -*- lexical-binding: t; -*-

(defcustom xq/lsp-pyright-diagnostic-mode "openFilesOnly"
  "Pyright diagnostic scope.

Use \"openFilesOnly\" to reduce file watching pressure in large projects."
  :type '(choice (const "workspace") (const "openFilesOnly"))
  :group 'tools)

(defun xq/lang-python-configure-lsp ()
  "Configure Python-specific lsp-mode behavior."
  (setq lsp-pyright-langserver-command "pyright"
        lsp-pyright-langserver-command-args '("--stdio")
        lsp-pyright-diagnostic-mode xq/lsp-pyright-diagnostic-mode))

(with-eval-after-load 'project
  (add-to-list 'project-vc-extra-root-markers "pyproject.toml"))

(with-eval-after-load 'lsp-mode
  (xq/lang-python-configure-lsp))

(defun xq/lang-python-setup ()
  "Enable Python defaults and lsp-mode in current buffer."
  (setq-local python-indent-offset 4
              tab-width 4)
  (require 'lsp-pyright nil t)
  (when (fboundp 'lsp-deferred)
    (lsp-deferred)))

(provide 'xq-lang-python)
;;; xq-lang-python.el ends here
