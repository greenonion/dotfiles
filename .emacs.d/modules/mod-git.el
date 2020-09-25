;; ---------------------------------
;; Configuration for git
;; ---------------------------------
(provide 'mod-git)

;; magit
;; -----

(use-package magit
  :straight t
  :bind ("C-x g" . magit-status)
  :init (add-hook 'magit-mode-hook 'hl-line-mode)
  :config
  (setenv "GIT_PAGER" "")
  (if (file-exists-p  "/usr/local/bin/emacsclient")
      (setq magit-emacsclient-executable "/usr/local/bin/emacsclient")
    (setq magit-emacsclient-executable (executable-find "emacsclient"))))

;; git-gutter
;; ----------

(use-package git-gutter
  :straight t
  :defer t
  :bind (("C-x =" . git-gutter:popup-hunk)
         ("C-x P" . git-gutter:previous-hunk)
         ("C-c N" . git-gutter:next-hunk)
         ("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk)
         ("C-c G" . git-gutter:popup-hunk))
  :diminish git-gutter-mode
  :init
  (add-hook 'prog-mode-hook 'git-gutter-mode)
  (add-hook 'org-mode-hook 'git-gutter-mode))

;; ediff

(use-package ediff
  :straight t
  :init
  (setq
   ;; Always split nicely for wide screens
   ediff-split-window-function 'split-window-horizontally)
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff
     ediff-current-difference nil 'C nil
     (concat
      (ediff-get-region-contents
       ediff-current-difference 'A ediff-control-buffer)
      (ediff-get-region-contents
       ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map ()
    (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map))
