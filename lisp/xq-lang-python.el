;;; xq-lang-python.el --- Python language defaults -*- lexical-binding: t; -*-

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

(defun xq/lang-python-configure-eglot ()
  "Configure Python-specific Eglot behavior."
  (setq-default
   eglot-workspace-configuration
   (cons
    `(:python . (:analysis (:diagnosticMode ,xq/pyright-diagnostic-mode
                            :exclude ,xq/pyright-analysis-exclude)))
    (assq-delete-all :python eglot-workspace-configuration)))
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) . ,(xq/lang-pyright-command))))

(with-eval-after-load 'project
  (add-to-list 'project-vc-extra-root-markers "pyproject.toml"))

(with-eval-after-load 'eglot
  (xq/lang-python-configure-eglot))

(defun xq/lang-python-setup ()
  "Enable Python defaults and Eglot in current buffer."
  (setq-local python-indent-offset 4
              tab-width 4)
  (when (fboundp 'eglot-ensure)
    (eglot-ensure)))

(provide 'xq-lang-python)
;;; xq-lang-python.el ends here
