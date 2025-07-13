;;; init.el --- Config entry file -*- lexical-binding: t; -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

(load "~/crafted-emacs/modules/crafted-init-config")

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'crafted-ui-packages)
(require 'crafted-evil-packages)
(require 'crafted-ide-packages)
(require 'crafted-completion-packages)
(require 'crafted-lisp-packages)
(require 'crafted-org-packages)

;;; Additional packages
(require 'ui-tweaks-packages)
(require 'ai-packages)
(add-to-list 'package-selected-packages 'general)
(add-to-list 'package-selected-packages 'rust-mode)

;; install the packages
(package-install-selected-packages :noconfirm)

;;; Custom func
(defun xq/open-emacs-config ()
  "Open an interactive file finder for Emacs config directory."
  (interactive)
  (let ((default-directory (expand-file-name "~/.config/emacs/")))
    (call-interactively 'find-file)))

;;; Config
(require 'crafted-defaults-config)

(require 'crafted-ui-config)
(setq evil-want-C-u-scroll t)

(require 'crafted-evil-config)

(require 'crafted-ide-config)
(crafted-ide-eglot-auto-ensure-all)
(crafted-ide-configure-tree-sitter '(css html python rust typescript javascript))

(require 'crafted-completion-config)
(require 'crafted-lisp-config)

(require 'crafted-org-config)
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))


;;; My config
(require 'ui-tweaks-config)
(require 'ai-config)


;;; Which-key config
(which-key-mode 1)

;;; General.el leader key setup
(use-package general
  :config
  (general-evil-setup)
  
  ;; Set up leader key
  (general-create-definer my/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")
  
  ;; Window management
  (my/leader-keys
    "w" '(:ignore t :which-key "window")
    "w-" '(evil-window-split :which-key "Split window -")
    "w|" '(evil-window-vsplit :which-key "Split window |")
    "wj" '(evil-window-down :which-key "Move to down window")
    "wk" '(evil-window-up :which-key "Move to up window")
    "wh" '(evil-window-left :which-key "Move to left window")
    "wl" '(evil-window-right :which-key "Move to right window")
    
    ;; File management
    "f"  '(:ignore t :which-key "files")
    "fp" '(xq/open-emacs-config :which-key "open emacs config")
    "ff" '(find-file :which-key "find file")
    "fr" '(recentf-open-files :which-key "recent files")

    ;; Switch 
    "s"  '(:ignore t :which-key "switch")
    "sb" '(consult-buffer :which-key "switch buffer")
    
    ;; Project management
    "p"  '(:ignore t :which-key "project")
    "pp" '(project-switch-project :which-key "switch project")
    "pf" '(project-find-file :which-key "find file in project")
    "pd" '(project-find-dir :which-key "find directory in project")
    "pb" '(project-switch-to-buffer :which-key "switch to project buffer")
    "ps" '(project-search :which-key "search in project")
    "pt" '(project-dired :which-key "project root dired")))

;;; Rust config
;; Reassign the rust-mode keybindings to the rust-ts-mode map.
(with-eval-after-load 'rust-ts-mode
  (require 'rust-mode)
  (keymap-set rust-ts-mode-map "C-c C-c C-u" #'rust-compile)
  (keymap-set rust-ts-mode-map "C-c C-c C-k" #'rust-check)
  (keymap-set rust-ts-mode-map "C-c C-c C-t" #'rust-test)
  (keymap-set rust-ts-mode-map "C-c C-c C-r" #'rust-run)
  (keymap-set rust-ts-mode-map "C-c C-c C-l" #'rust-run-clippy)
  (keymap-set rust-ts-mode-map "C-c C-f" #'rust-format-buffer)
  (keymap-set rust-ts-mode-map "C-c C-n" #'rust-goto-format-problem))

;; treat underscore as a word character
(modify-syntax-entry ?_ "w")


(provide 'init)
