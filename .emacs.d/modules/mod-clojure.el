;; -----------------------------
;; Clojure configuration
;; -----------------------------
(provide 'mod-clojure)

;; Clojure
;; -------

(defun my/setup-clojure-hook ()
  "Set up Clojure"
  (eldoc-mode 1)
  (subword-mode t)
  (paredit-mode 1)
  (global-set-key (kbd "C-c t") 'clojure-jump-between-tests-and-code))

(use-package clojure-mode
  :straight t
  :init
  (add-hook #'clojure-mode-hook #'my/setup-clojure-hook)
  :config
  (use-package clojure-mode-extra-font-locking :straight t)
  (define-clojure-indent
    ;; Compojure routes
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)
    ;; Midje
    (facts 2)
    (fact 2)))

(defun my/setup-cider ()
  (interactive)
  (setq cider-history-file "~/.nrepl-history"
        cider-hide-special-buffers t
        cider-repl-history-size 10000
        cider-prefer-local-resources t
        cider-popup-stacktraces-in-repl t)
  (paredit-mode 1)
  (eldoc-mode 1))

(defun my/trailing-whitespace ()
  (interactive)
  (setq-local show-trailing-whitespace nil))

(defun my/component-reset ()
  (interactive)
  (save-some-buffers)
  (cider-interactive-eval "(reloaded.repl/reset)"))

(use-package cider
  :straight t
  :defer 30
  :init
  (add-hook #'cider-mode-hook #'my/setup-cider)
  (add-hook #'cider-repl-mode-hook #'my/setup-cider)
  (add-hook #'cider-repl-mode-hook #'my/trailing-whitespace)
  (add-hook #'cider-mode-hook #'my/setup-clojure-hook)
  (add-hook #'cider-repl-mode-hook #'my/setup-clojure-hook)
  (add-hook #'cider-mode-hook #'company-mode)
  (add-hook #'cider-repl-mode-hook #'company-mode)
  (global-set-key (kbd "C-c r") 'my/component-reset)
  :config
  (setq cider-repl-display-help-banner nil))
