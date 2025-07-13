;;; ui-tweaks-config.el --- Custom UI tweaks and configurations -*- lexical-binding: t; -*-

(setq inhibit-startup-message t) ; No welcome messsage

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
;; Set up the visible bell
;; (setq visible-bell t)



;;; Line number and column number
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
(add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; Doom theme
(use-package doom-themes
    :config (load-theme 'doom-one t))

(use-package doom-modeline
    :init (doom-modeline-mode 1)
)


(provide 'ui-tweaks-config)
;;; ui-tweaks-config.el ends here
