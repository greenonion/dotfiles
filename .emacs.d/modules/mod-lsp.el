;; ---------------------
;; Configuration for LSP
;; ---------------------
(provide 'mod-lsp)

(use-package lsp-mode
  :straight t
  :hook (ruby-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-enable-snippet nil))

(use-package helm-lsp
  :straight t
  :commands helm-lsp-workspace-symbol)

(use-package company-lsp
  :straight t
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))
