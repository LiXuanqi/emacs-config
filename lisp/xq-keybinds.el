;;; xq-keybinds.el --- Leader keybindings -*- lexical-binding: t; -*-

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
    "wl" '(windmove-right :which-key "focus right")))

(provide 'xq-keybinds)
;;; xq-keybinds.el ends here
