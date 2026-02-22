;;; xq-lang-python.el --- Python language defaults -*- lexical-binding: t; -*-

(defun xq/lang-python-setup ()
  "Enable Python defaults and Eglot in current buffer."
  (setq-local python-indent-offset 4
              tab-width 4)
  (when (fboundp 'eglot-ensure)
    (eglot-ensure)))

(provide 'xq-lang-python)
;;; xq-lang-python.el ends here
