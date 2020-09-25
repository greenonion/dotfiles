;; ------------------
;; Completion
;; ------------------
(provide 'mod-completion)

(use-package dabbrev
  :straight t
  :init
  (setq dabbrev-case-fold-search nil))

(use-package hippie-exp
  :straight t
  :init
  ;; force hippie-expand completions to be case-sensitive
  (defadvice hippie-expand (around hippie-expand-case-fold activate)
    "Try to do case-sensitive matching (not effective with all functions)."
    (let ((case-fold-search nil))
      ad-do-it))

  :config
  (setq hippie-expand-try-functions-list
        '(;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-list
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-line
          ;; Try to complete the current line to an entire line in a different
          ;; buffer.
          try-expand-line-all-buffers
          ;; Try to complete as an Emacs Lisp symbol, as many characters as
          ;; unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol)))

;; auto-completion
;; ---------------

(use-package company
  :straight t
  :diminish company-mode
  :bind (("C-c ." . company-complete)
         ("C-." . company-complete))
  :hook (init-hook . company-mode)
  :init
  (use-package company-quickhelp
    :straight t
    :hook (company-mode . company-quickhelp-mode)
    :config (setq company-quickhelp-delay 2))
  (use-package company-statistics
    :straight t
    :hook (after-init . company-statistics-mode))
  :config
  (setq company-idle-delay 0.25
        company-minimum-prefix-length 3
        company-tooltip-limit 14
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-show-numbers t
        company-require-match 'never
        company-global-modes
        '(not erc-mode message-mode help-mode gud-mode eshell-mode)
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-backends '(company-capf)
        company-auto-complete-chars nil
        company-transformers '(company-sort-by-occurrence))
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-d" . company-show-doc-buffer)
             ("<tab>" . company-complete)))

(use-package company-dabbrev
  :init
  (setq company-dabbrev-ignore-case nil
        ;; don't downcase dabbrev suggestions
        company-dabbrev-downcase nil
        company-dabbrev-downcase nil))

(use-package company-dabbrev-code
  :init
  (setq company-dabbrev-code-modes t
        company-dabbrev-code-ignore-case nil))

(use-package smart-tab
  :straight (smart-tab :branch "main" :repo "https://git.genehack.net/genehack/smart-tab.git")
  :defer t
  :diminish ""
  :init
  (global-smart-tab-mode 1)
  (setq smart-tab-using-hippie-expand t)
  :config
  (progn
    (add-to-list 'smart-tab-disabled-major-modes 'mu4e-compose-mode)
    (add-to-list 'smart-tab-disabled-major-modes 'erc-mode)
    (add-to-list 'smart-tab-disabled-major-modes 'shell-mode)))
