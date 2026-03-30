;;; xq-evil.el --- Evil mode setup -*- lexical-binding: t; -*-

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  ;; Keep `vterm' in its native terminal-oriented keymap instead of
  ;; installing Evil bindings that intercept shell input.
  (setq evil-collection-mode-list
        (delq 'vterm evil-collection-mode-list))
  (evil-collection-init))

(provide 'xq-evil)
;;; xq-evil.el ends here
