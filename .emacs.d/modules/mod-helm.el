;; ------------------
;; Helm + Projectile = <3
;; ------------------
(provide 'mod-helm)

;; helm flx should get loaded before helm
(use-package helm-flx
  :straight t
  :init
  (setq helm-flx-for-helm-find-files nil)
  (helm-flx-mode 1))

(use-package helm-config
  :straight helm
  :demand t
  :diminish helm-mode
  :bind
  (("C-x C-f" . helm-find-files)
   ("M-y" . helm-show-kill-ring)
   ("C-x C-i" . helm-semantic-or-imenu)
   ("M-x" . helm-M-x)
   ("C-x b" . helm-mini)
   ("C-x C-o" . helm-occur)
   ("C-h a" . helm-apropos))
  :config
  (use-package helm-files
    :config (setq helm-ff-file-compressed-list '("gz" "bz2" "zip" "tgz" "xz" "txz")))
  (use-package helm-buffers)
  (use-package helm-mode
    :diminish helm-mode
    :init (helm-mode 1))
  (use-package helm-misc)
  (use-package helm-elisp)
  (use-package helm-imenu)
  (use-package helm-semantic)
  (use-package helm-ring)
  (use-package helm-projectile
    :straight t
    :bind (("C-x f" . helm-projectile)
           ("C-c p f" . helm-projectile-find-file)
           ("C-c p s" . helm-projectile-switch-project)))
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  ;; Via: https://www.reddit.com/r/emacs/comments/3asbyn/new_and_very_useful_helm_feature_enter_search/
  (setq helm-echo-input-in-header-line t)
  (defun helm-hide-minibuffer-maybe ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

  (setq
   ;; truncate long lines in helm completion
   helm-truncate-lines t
   ;; do not display invisible candidates
   helm-quick-update t
   ;; open helm buffer inside current window, don't occupy whole other window
   helm-split-window-in-side-p t
   ;; move to end or beginning of source when reaching top or bottom
   ;; of source
   helm-move-to-line-cycle-in-source t
   ;; fuzzy matching
   helm-recentf-fuzzy-match t
   helm-locate-fuzzy-match nil ;; locate fuzzy is worthless
   helm-M-x-fuzzy-match t
   helm-buffers-fuzzy-matching t
   helm-semantic-fuzzy-match nil
   helm-imenu-fuzzy-match nil
   helm-apropos-fuzzy-match nil
   helm-lisp-fuzzy-completion nil
   helm-completion-in-region-fuzzy-match nil
   ;; autoresize to 25 rows
   helm-autoresize-min-height 25
   helm-autoresize-max-height 25)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  ;; ggrep is gnu grep on OSX
  (when (executable-find "ggrep")
    (setq helm-grep-default-command
          "ggrep -a -d skip %e -n%cH -e %p %f"
          helm-grep-default-recurse-command
          "ggrep -a -d recurse %e -n%cH -e %p %f")))

(use-package helm-swoop
  :straight t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop))
  :config
  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t
        ;; If this value is t, split window inside the current window
        helm-swoop-split-with-multiple-windows t
        ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
        helm-swoop-split-direction 'split-window-vertically
        ;; If nil, you can slightly boost invoke speed in exchange for text color
        helm-swoop-speed-or-color nil))

(use-package helm-flyspell
  :straight t
  :after helm
  :bind (:map flyspell-mode-map
              ("C-;" . helm-flyspell-correct)))

;; projectile
;; ----------

(use-package projectile
  :straight t
  :defer 5
  :hook (after-init . projectile-mode)
  :commands projectile-mode
  :diminish projectile-mode
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (bind-key "C-c p b" #'projectile-switch-to-buffer #'projectile-command-map)
  (bind-key "C-c p K" #'projectile-kill-buffers #'projectile-command-map)

  ;; global ignores
  (add-to-list 'projectile-globally-ignored-files ".tern-port")
  (add-to-list 'projectile-globally-ignored-files "GTAGS")
  (add-to-list 'projectile-globally-ignored-files "GPATH")
  (add-to-list 'projectile-globally-ignored-files "GRTAGS")
  (add-to-list 'projectile-globally-ignored-files "GSYMS")
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  ;; always ignore .class files
  (add-to-list 'projectile-globally-ignored-file-suffixes ".class")
  (use-package helm-projectile
    :straight t
    :init
    (use-package grep) ;; required for helm-ag to work properly
    (setq projectile-completion-system 'helm)
    ;; no fuzziness for projectile-helm
    (setq helm-projectile-fuzzy-match nil)
    (helm-projectile-on))
  ;;(projectile-global-mode)
  )
