;;; xq-code-anchor.el --- Open code locations in remote forges -*- lexical-binding: t; -*-

(use-package code-anchor
  :straight (:type git :host github :repo "lixuanqi/code-anchor")
  :commands (code-anchor-open
             code-anchor-open-github
             code-anchor-open-sourcegraph)
  :init
  (setq code-anchor-preferred-host 'github))

(provide 'xq-code-anchor)
;;; xq-code-anchor.el ends here
