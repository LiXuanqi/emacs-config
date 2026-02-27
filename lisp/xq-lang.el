;;; xq-lang.el --- Language and LSP entrypoint -*- lexical-binding: t; -*-

(defcustom xq/eglot-server-memory-mb 2048
  "Max memory in MB for Node-based Eglot language servers.

When non-nil, this value is passed through `NODE_OPTIONS` using
`--max-old-space-size` for Node.js language servers like Pyright."
  :type '(choice (const :tag "Use server default" nil) integer)
  :group 'tools)

(defcustom xq/pyright-analysis-exclude
  ["**/.git"
   "**/.hg"
   "**/.svn"
   "**/.venv"
   "**/venv"
   "**/.mypy_cache"
   "**/.pytest_cache"
   "**/__pycache__"
   "**/node_modules"
   "**/dist"
   "**/build"
   "**/target"]
  "Glob patterns that Pyright should exclude from analysis/watch scope."
  :type '(vector string)
  :group 'tools)

(defcustom xq/pyright-diagnostic-mode "openFilesOnly"
  "Pyright diagnostic scope.

Use \"openFilesOnly\" to reduce file watching pressure in large projects."
  :type '(choice (const "workspace") (const "openFilesOnly"))
  :group 'tools)

(defun xq/lang-pyright-command ()
  "Build the Pyright command used by Eglot."
  (append
   (when (and xq/eglot-server-memory-mb
              (> xq/eglot-server-memory-mb 0))
     (list "env"
           (format "NODE_OPTIONS=--max-old-space-size=%d"
                   xq/eglot-server-memory-mb)))
   '("pyright-langserver" "--stdio")))

(defun xq/lang-configure-pyright-workspace ()
  "Configure Pyright settings delivered through Eglot."
  (setq-default
   eglot-workspace-configuration
   (cons
    `(:python . (:analysis (:diagnosticMode ,xq/pyright-diagnostic-mode
                            :exclude ,xq/pyright-analysis-exclude)))
    (assq-delete-all :python eglot-workspace-configuration))))

(use-package eglot
  :defer t
  :init
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5))

(with-eval-after-load 'project
  (add-to-list 'project-vc-extra-root-markers "pyproject.toml"))

(with-eval-after-load 'eglot
  (xq/lang-configure-pyright-workspace)
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) . ,(xq/lang-pyright-command)))
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
