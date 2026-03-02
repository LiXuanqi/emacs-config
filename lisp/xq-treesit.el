;;; xq-treesit.el --- Tree-sitter defaults -*- lexical-binding: t; -*-

(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  ;; Grammar sources for M-x treesit-install-language-grammar.
  (setq treesit-language-source-alist
        '((python "https://github.com/tree-sitter/tree-sitter-python")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")))

  ;; Prefer tree-sitter major modes when available.
  (setq major-mode-remap-alist
        (append '((python-mode . python-ts-mode)
                  (css-mode . css-ts-mode)
                  (js-mode . js-ts-mode)
                  (javascript-mode . js-ts-mode)
                  (json-mode . json-ts-mode)
                  (typescript-mode . typescript-ts-mode)
                  (go-mode . go-ts-mode)
                  (rust-mode . rust-ts-mode))
                major-mode-remap-alist)))

(provide 'xq-treesit)
;;; xq-treesit.el ends here
