;; ------------------
;; Look and feel
;; ------------------
(provide 'mod-appearance)

;; flash cursor whenever you adjust position.
(use-package beacon
  :straight t
  :diminish beacon-mode
  :init (beacon-mode 1))

;; add a face for parentheses, used by themes to darken the parens
(use-package paren-face
  :straight t
  :init (global-paren-face-mode))

;; don’t use dialog boxes, just ask inside Emacs
(setq use-dialog-box nil)

;; play with it depending on font
;; (setq-default line-spacing 0.2)

(setq-default display-line-number-width 3
              blink-matching-paren nil
              redisplay-dont-pause t
              jit-lock-stealth-time 0.2
              max-mini-window-height 0.3
              tooltip-reuse-hidden-frame t
              word-wrap nil
              ;; Margins
              left-margin-width 1
              right-margin-width 1)

;; use pixel-based scrolling
(when (fboundp 'pixel-scroll-mode)
  (pixel-scroll-mode 1))

;; visual line wrapping
(diminish 'visual-line-mode)
(dolist (hook '(text-mode-hook
                Man-mode-hook))
  (add-hook hook 'visual-line-mode))

;; Theme
;; -----
(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
;  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-one")
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Fonts
;; -----
(defun my/setup-osx-fonts ()
  (interactive)
  (when (eq system-type 'darwin)
;;    (set-face-attribute 'default t :font "Iosevka 14")
    (set-frame-font "Iosevka SS08" nil t)
    (set-face-attribute 'default nil :height 140 :weight 'normal)
    ;;(set-face-attribute 'fixed-pitch nil :height 120 :weight 'normal)

    ;; Anti-aliasing
    (setq mac-allow-anti-aliasing t)))

(when (eq system-type 'darwin)
  (add-hook 'after-init-hook #'my/setup-osx-fonts))

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Mode line
;; ---------

;; display time and load on modeline
(setq
 ;; don't display info about mail
 display-time-mail-function (lambda () nil)
 ;; update every 15 seconds (default is 60)
 display-time-default-load-average nil
 display-time-interval 15
 display-time-format "%d-%m-%Y %R")
(display-time-mode 1)
(display-battery-mode 1)

(use-package smart-mode-line
  :straight t
  :init
  (setq sml/theme 'respectful)
  (sml/setup)
  :config
  (setq sml/shorten-directory t
        sml/shorten-modes t))

;; Smooth scrolling
;; ----------------

(use-package smooth-scrolling
  :straight t
  :disabled t
  :config
  (setq smooth-scroll-margin 3
        scroll-margin 3
        scroll-conservatively 101
        scroll-preserve-screen-position t
        auto-window-vscroll nil))

(setq scroll-margin 3
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      hscroll-margin 5
      hscroll-step 5)

;; Highlighting the current line

(setq my/hl-line-enabled t)

(defun my/turn-on-hl-line ()
  (interactive)
  (when my/hl-line-enabled
    (hl-line-mode 1)))

(defun my/turn-off-hl-line ()
  (interactive)
  (hl-line-mode -1))

;; Turn it on by default (if enabled!) in prog-mode

(add-hook 'prog-mode-hook #'my/turn-on-hl-line)

;; Fringe
;; ------

(defvar my/fringe-width 12
  "The fringe width to use.")

(defun my/set-fringe-background ()
  "Set the fringe background to the same color as the regular background."
  (setq my/fringe-background-color
        (face-background 'default))
  (custom-set-faces
   `(fringe ((t (:background ,my/fringe-background-color))))))

(add-hook 'after-init-hook #'my/set-fringe-background)

;; show where the buffer ends on the right-hand fringe
(setq-default indicate-buffer-boundaries 'right
              indicate-empty-lines t
              fringe-indicator-alist
              (delq (assq 'continuation fringe-indicator-alist)
                    fringe-indicator-alist)
              fringes-outside-margins t
              visual-line-fringe-indicators
              '(left-curly-arrow right-curly-arrow)
              ;; Keep cursors and highlights in current window only
              cursor-in-non-selected-windows nil)

;; Setup border
(push (cons 'internal-border-width my/fringe-width) default-frame-alist)

;; Standardize fringe width
(push (cons 'left-fringe  my/fringe-width) default-frame-alist)
(push (cons 'right-fringe my/fringe-width) default-frame-alist)

(when (fboundp 'fringe-mode)
  (fringe-mode my/fringe-width))

(defun my/disable-minibuffer-window-fringes ()
  "Disable the window fringes for minibuffer window."
  (set-window-fringes (minibuffer-window) 0 0 nil))

(add-hook 'emacs-startup-hook 'my/disable-minibuffer-window-fringes)
(add-hook 'minibuffer-setup-hook 'my/disable-minibuffer-window-fringes)

;; Window borders
(when (boundp 'window-divider-mode)
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  (window-divider-mode 1))

;; Colors in compilation buffer
;; (use-package ansi-color
;;   :config
;;   (defun my-colorize-compilation-buffer ()
;;     (when (eq major-mode 'compilation-mode)
;;       (ansi-color-apply-on-region compilation-filter-start (point))))
;;   :hook (compilation-filter . my-colorize-compilation-buffer))

(use-package xterm-color
  :straight t
  :config
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'my/advice-compilation-filter))

;; Rainbow delimiters
;; ------------------

;; Only highlight unbananced delimiters
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error))

;; Compact whitespace in docstrings
(use-package compact-docstrings
  :straight t
  :diminish compact-docstrings-mode
  :hook (prog-mode . compact-docstrings-mode))
