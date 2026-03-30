;;; xq-keybinds.el --- Leader keybindings -*- lexical-binding: t; -*-

(declare-function xq/module-enabled-p "xq-overrides" (feature))
(declare-function xq/leader-def "ext:general" (&rest args))

(use-package general
  :after evil
  :config
  (general-auto-unbind-keys)
  (general-create-definer xq/leader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC")
  (xq/leader-def
    "w"  '(:ignore t :which-key "window")
    "w|" '(split-window-right :which-key "split right")
    "w-" '(split-window-below :which-key "split below")
    "wh" '(windmove-left :which-key "focus left")
    "wj" '(windmove-down :which-key "focus down")
    "wk" '(windmove-up :which-key "focus up")
    "wl" '(windmove-right :which-key "focus right")
    "s"  '(:ignore t :which-key "search")
    "ss" '(consult-line :which-key "line")
    "sg" '(consult-ripgrep :which-key "ripgrep")
    "sb" '(consult-buffer :which-key "buffer")
    "sf" '(consult-find :which-key "find file")
    "si" '(consult-imenu :which-key "imenu")
    "p"  '(:ignore t :which-key "project")
    "pp" '(project-switch-project :which-key "switch project")
    "pf" '(project-find-file :which-key "find file")
    "pd" '(project-find-dir :which-key "find dir")
    "l"  '(:ignore t :which-key "lsp")
    "ll" '(lsp :which-key "start")
    "ld" '(xref-find-definitions :which-key "definition")
    "lD" '(lsp-find-declaration :which-key "declaration")
    "li" '(lsp-find-implementation :which-key "implementation")
    "lt" '(lsp-find-type-definition :which-key "type definition")
    "lr" '(xref-find-references :which-key "references")
    "lR" '(lsp-rename :which-key "rename")
    "la" '(lsp-execute-code-action :which-key "code action")
    "lf" '(lsp-format-buffer :which-key "format buffer")
    "lo" '(lsp-organize-imports :which-key "organize imports")
    "le" '(flymake-show-buffer-diagnostics :which-key "diagnostics")
    "ls" '(lsp-describe-session :which-key "session"))

  (when (xq/module-enabled-p 'xq-git)
    (xq/leader-def
      "g"  '(:ignore t :which-key "git")
      "gg" '(magit-status :which-key "status")
      "gl" '(magit-log-current :which-key "log current")
      "gb" '(magit-blame-addition :which-key "blame")))

  (when (xq/module-enabled-p 'xq-code-anchor)
    (xq/leader-def
      "go" '(:ignore t :which-key "open remote")
      "goo" '(code-anchor-open :which-key "open")
      "gog" '(code-anchor-open-github :which-key "github")
      "gos" '(code-anchor-open-sourcegraph :which-key "sourcegraph")))

  (when (xq/module-enabled-p 'xq-terminal)
    (xq/leader-def
      "t"  '(:ignore t :which-key "terminal")
      "tt" '(xq/terminal-vterm-here :which-key "vterm (project)")))

  (when (xq/module-enabled-p 'xq-org)
    (xq/leader-def
      "n"  '(:ignore t :which-key "notes")
      "nn" '(org-roam-dailies-goto-today :which-key "today daily")
      "na" '(org-agenda :which-key "agenda")
      "nr" '(:ignore t :which-key "roam")
      "nrf" '(org-roam-node-find :which-key "find node")
      "nri" '(org-roam-node-insert :which-key "insert node")
      "nrc" '(org-roam-capture :which-key "capture node")
      "nrt" '(org-roam-dailies-capture-today :which-key "daily todo")
      "nrb" '(org-roam-buffer-toggle :which-key "toggle buffer"))))

(provide 'xq-keybinds)
;;; xq-keybinds.el ends here
