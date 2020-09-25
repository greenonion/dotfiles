;; ------------------
;; Writing
;; ------------------
(provide 'mod-writing)

;; Markup
;; --------

(use-package markdown-mode
 :straight t
 :mode (("\\`README\\.md\\'" . gfm-mode)
        ("github\\.com.*\\.txt\\'" . gfm-mode)
        ("\\.md\\'"          . markdown-mode)
        ("\\.markdown\\'"    . markdown-mode))
 :hook (markdown-mode . flyspell-mode)
 :init
 (setq markdown-enable-wiki-links t
       markdown-italic-underscore t
       markdown-make-gfm-checkboxes-buttons t
       markdown-gfm-additional-languages '("sh")))

(use-package yaml-mode
  :straight t)

;; Whitespace
;; ----------

;; show a ruler at the 100 character mark (if on emacs 27 and above)
(use-package display-fill-column-indicator
  :unless (version< emacs-version "27")
  :hook (prog-mode . display-fill-column-indicator-mode)
  :diminish display-fill-column-indicator-mode
  :config
  (setq-default display-fill-column-indicator-column fill-column))

(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))

(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal.
      '(;;(space-mark 32 [183] [46])
        ;; (newline-mark 10 [172 10]) ;; the paragraph sign
        (newline-mark 10 [172 10]) ;; mathematical "not"
        (tab-mark 9 [187 9] [92 9])))

(defun my/turn-on-whitespace-mode ()
  (interactive)
  (setq-local whitespace-line-column fill-column)
  (whitespace-mode +1)
  (diminish 'whitespace-mode)
  (whitespace-newline-mode 1)
  (diminish 'whitespace-newline-mode))

(add-hook 'prog-mode-hook #'my/turn-on-whitespace-mode)
