;; Credits to dakrone for the inspiration.
;;
;; Turn on debugging
(setq debug-on-error t)
(setq debug-on-quit t)

;; Keep track of loading time
(defconst emacs-start-time (current-time))

;; Initialize straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; We will be using use-package to install packages
(straight-use-package 'use-package)
;; Set to t to debug package loading or nil to disable
(defvar use-package-verbose)
(setq use-package-verbose t)

(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loaded packages in %.3fs" elapsed))

;; Keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(require 'cl-lib)

;; Basics and settings used everywhere
;; -----------------------------------

(setq user-full-name "Nikos Fertakis"
      user-mail-address "nikos.fertakis@gmail.com")

;; prefer UTF-8 everywhere
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; turn on syntax highlighting for all buffers
(global-font-lock-mode t)

;; raise maximum number of logs
(setq message-log-max 16384)

;; configure the GC
(setq gc-cons-threshold (* 100 1024 1024)) ;; 100 mb

(setq read-process-output-max (* 1024 1024)) ;; 1 mb

;; Allow font-lock-mode to do background parsing
(setq jit-lock-stealth-time 1
      ;; jit-lock-stealth-load 200
      jit-lock-chunk-size 1000
      jit-lock-defer-time 0.05)

;; toggle line number limit
(setq line-number-display-limit-width 10000)

;; make gnutls a bit safer
(setq gnutls-min-prime-bits 4096)

;; delete selected region on typing
(delete-selection-mode 1)

(setq large-file-warning-threshold (* 25 1024 1024))

(transient-mark-mode 1)

(setq-default indicate-empty-lines nil)
(setq-default indicate-buffer-boundaries nil)

;; Turn off all kinds of modes, I don't need the menu bar, or the tool bar:
;; This triggers a bug in mac-emacs
;; (when (functionp 'menu-bar-mode)
;;   (menu-bar-mode -1))
(when (functionp 'set-scroll-bar-mode)
  (set-scroll-bar-mode 'nil))
(when (functionp 'mouse-wheel-mode)
  (mouse-wheel-mode -1))
(when (functionp 'tooltip-mode)
  (tooltip-mode -1))
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (functionp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; don't blink, please
(when (functionp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; don't beep and dont show startup message
(setq ring-bell-function (lambda ()))
(setq inhibit-startup-screen t
      initial-major-mode 'fundamental-mode)

;; show line and column numbers in mode-line
(line-number-mode 1)
(column-number-mode 1)

;; ignore case in file name completion
(setq read-file-name-completion-ignore-case t)

;; y or n should suffice
(defalias 'yes-or-no-p 'y-or-n-p)

;; confirm when killing only on graphical session
(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

;; cursor follows visual lines (when text wraps around)
(setq line-move-visual t)

;; hide mouse while typing
(setq make-pointer-invisible t)

;; set tab width to 2
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)

;; fix some weird color escape sequences
(setq system-uses-terminfo nil)

;; resolve symlinks
(setq-default find-file-visit-truename t)

;; require newline at the end of files
(setq require-final-newline t)

;; search (and search/replace) using regex by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)

;; single space ends a sentence
(setq sentence-end-double-space nil)

;; split windows
(setq split-height-threshold nil)
(setq split-width-threshold 180)

;; rescan for imenu changes
(set-default 'imenu-auto-rescan t)

;; seed random number generator
(random t)

;; switch to unified diffs
(setq diff-switches "-u")

;; turn on auto-fill mode in text buffers
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; never kill the *scratch* buffer
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;; automatically revert file if changed on disk
(global-auto-revert-mode 1)

;; be quiet about reverting files
(setq auto-revert-verbose nil)

;; General utilities and environment
;; ---------------------------------

;; Setting up $PATH and other vars
(use-package exec-path-from-shell
  :straight t
  :init
  (progn
    (setq exec-path-from-shell-variables '("JAVA_HOME"
                                           "PATH"
                                           "WORKON_HOME"
                                           "MANPATH"
                                           "LANG"))
    (exec-path-from-shell-initialize)))

;; uniquify buffers
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package diminish
  :init
  (progn
    (diminish 'auto-fill-function "")))

;; start server if not running but only for gui
(require 'server nil t)
(use-package server
  :if window-system
  :init
  (when (not (server-running-p server-name))
    (server-start)))

;; GUI-specific
(when (window-system)
  (setenv "EMACS_GUI" "t"))

;; display time and load on modeline
(setq
 ;; don't display info about mail
 display-time-mail-function (lambda () nil)
 ;; update every 15 seconds (default is 60)
 display-time-interval 15
 display-time-format "%d-%m-%Y %R")
(display-time-mode 1)

;; quit as fast as possible
(defun mu/quit-emacs-unconditionally ()
  (interactive)
  (my-quit-emacs '(4)))

(define-key special-event-map (kbd "<sigusr1>") #'my/quit-emacs-unconditionally)

(setq tls-program
      ;; Defaults:
      ;; '("gnutls-cli --insecure -p %p %h"
      ;;   "gnutls-cli --insecure -p %p %h --protocols ssl3"
      ;;   "openssl s_client -connect %h:%p -no_ssl2 -ign_eof")
      '("gnutls-cli -p %p %h"
        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

;; OS-specific settings
;; --------------------

(defun mac-switch-meta nil
    "Switch meta between option and command."
    (interactive)
    (if (eq mac-option-modifier nil)
        (progn
          (setq mac-option-modifier 'meta)
          (setq mac-command-modifier 'super))
      (progn
        (setq mac-option-modifier nil)
        (setq mac-command-modifier 'meta))))

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)

  (global-set-key [(super v)] 'yank)
  (global-set-key [(super c)] 'kill-ring-save)
  (global-set-key [(super z)] 'undo)
  (global-set-key [(super l)] 'goto-line)

  (setq ns-use-native-fullscreen nil)
  ;; brew install coreutils
  (if (executable-find "gls")
      (progn
        (setq insert-directory-program "gls")
        (setq dired-listing-switches "-lFaGh1v --group-directories-first"))
    (setq dired-listing-switches "-ahlF"))

  (defun copy-from-osx ()
    "Handle copy/paste intelligently on osx."
    (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
      (if (and (eq system-type 'darwin)
               (file-exists-p pbpaste))
          (let ((tramp-mode nil)
                (default-directory "~"))
            (shell-command-to-string pbpaste)))))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "/usr/bin/pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx
        interprogram-paste-function 'copy-from-osx)

  (defun move-file-to-trash (file)
    "Use `trash' to move FILE to the system trash.
When using Homebrew, install it using \"brew install trash\"."
    (call-process (executable-find "trash")
                  nil 0 nil
                  file))

  ;; Trackpad scrolling
  (global-set-key [wheel-up] 'previous-line)
  (global-set-key [wheel-down] 'next-line))

;; save minibuffer histories
(setq savehist-additional-variables
      ;; also save my search entries
      '(search-ring regexp-search-ring)
      savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
;; remember where I've left off in each file
(save-place-mode 1)

;; delete auto-save files
(setq delete-auto-save-files t)
(setq backup-directory-alist
      '(("." . "~/.emacs_backups")))

;; delete old backups silently
(setq delete-old-versions t)

;; *****
;; Theme
;; *****

(setq ns-use-srgb-colorspace t)

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

;; Shell settings

;; use cat for shell pager
(setenv "PAGER" "cat")

(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output nil) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 ;; '(comint-completion-autolist t)     ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 '(comint-prompt-read-only nil)         ; if this is t, it breaks shell-command
 '(comint-get-old-input (lambda () ""))      ; what to run when i press enter on a
                                        ; line above the current prompt
 )

(defun my/shell-kill-buffer-sentinel (process event)
  (when (memq (process-status process) '(exit signal))
    (kill-buffer)))

(defun my/kill-process-buffer-on-exit ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'my/shell-kill-buffer-sentinel))

(dolist (hook '(ielm-mode-hook term-exec-hook comint-exec-hook))
  (add-hook hook 'my/kill-process-buffer-on-exit))

(defun set-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
  (set (make-local-variable 'scroll-conservatively) 10))

(defadvice comint-previous-matching-input
    (around suppress-history-item-messages activate)
  "Suppress the annoying 'History item : NNN' messages from shell history isearch.
If this isn't enough, try the same thing with
comint-replace-by-expanded-history-before-point."
  (let ((old-message (symbol-function 'message)))
    (unwind-protect
        (progn (fset 'message 'ignore) ad-do-it)
      (fset 'message old-message))))

(add-hook 'shell-mode-hook 'set-scroll-conservatively)
;; truncate buffers continuously
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
;; interpret and use ansi color codes in shell output windows
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Eshell settings

(defun my/setup-eshell ()
  (interactive)
  ;; turn off semantic-mode in eshell buffers
  (semantic-mode -1)
  ;; turn off hl-line-mode
  (hl-line-mode -1)
  (setq-local show-trailing-whitespace nil)
  (define-key eshell-mode-map (kbd "M-l")
    'helm-eshell-history)
  (when (fboundp smartscan-mode)
    (smartscan-mode -1)))

(use-package eshell
  :commands (eshell eshell-command)
  :config
  (defalias 'emacs 'find-file)
  (defalias 'sec 'sudoec)
  (setenv "PAGER" "cat")
  (use-package esh-opt
    :config
    (use-package em-cmpl)
    (use-package em-prompt)
    (use-package em-term)

    (setq eshell-cmpl-cycle-completions nil
          ;; auto truncate after 12k lines
          eshell-buffer-maximum-lines 12000
          ;; history size
          eshell-history-size 500
          ;; buffer shorthand -> echo foo > #'buffer
          eshell-buffer-shorthand t
          ;; my prompt is easy enough to see
          eshell-highlight-prompt nil
          ;; treat 'echo' like shell echo
          eshell-plain-echo-behavior t)

    ;; Visual commands
    (setq eshell-visual-commands '("vi" "screen" "top" "less" "more" "lynx"
                                   "ncftp" "pine" "tin" "trn" "elm" "vim"
                                   "nmtui" "alsamixer" "htop" "el" "elinks"
                                   ))
    (setq eshell-visual-subcommands '(("git" "log" "diff" "show")))

    (defun my/truncate-eshell-buffers ()
      "Truncates all eshell buffers"
      (interactive)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (eq major-mode 'eshell-mode)
            (eshell-truncate-buffer)))))

    ;; After being idle for 5 seconds, truncate all the eshell-buffers if
    ;; needed. If this needs to be canceled, you can run `(cancel-timer
    ;; my/eshell-truncate-timer)'
    (setq my/eshell-truncate-timer
          (run-with-idle-timer 5 t #'my/truncate-eshell-buffers))

    (when (not (functionp 'eshell/rgrep))
      (defun eshell/rgrep (&rest args)
        "Use Emacs grep facility instead of calling external grep."
        (eshell-grep "rgrep" args t)))

    (defun eshell/cds ()
      "Change directory to the project's root."
      (eshell/cd (locate-dominating-file default-directory ".git")))

    (defun eshell/l (&rest args) "Same as `ls -lh'"
           (apply #'eshell/ls "-lh" args))
    (defun eshell/ll (&rest args) "Same as `ls -lh'"
           (apply #'eshell/ls "-lh" args))
    (defun eshell/la (&rest args) "Same as `ls -alh'"
           (apply #'eshell/ls "-alh" args))

    (defun eshell/ec (pattern)
      (if (stringp pattern)
          (find-file pattern)
        (mapc #'find-file (mapcar #'expand-file-name pattern))))

    (defun eshell/clear ()
      "Clear the eshell buffer"
      (interactive)
      (let ((eshell-buffer-maximum-lines 0))
        (eshell-truncate-buffer))))

  (add-hook 'eshell-mode-hook #'my/setup-eshell)

  ;; See eshell-prompt-function below
  (setq eshell-prompt-regexp "^[^#$\n]* [#$] ")

  ;; So the history vars are defined
  (require 'em-hist)
  (if (boundp 'eshell-save-history-on-exit)
      ;; Don't ask, just save
      (setq eshell-save-history-on-exit t))

  ;; See: https://github.com/kaihaosw/eshell-prompt-extras
  (use-package eshell-prompt-extras
    :init
    (progn
      (setq eshell-highlight-prompt nil
            epe-git-dirty-char " Ϟ"
            ;; epe-git-dirty-char "*"
            eshell-prompt-function 'epe-theme-lambda
            )))

  (defun eshell/magit ()
    "Function to open magit-status for the current directory"
    (interactive)
    (magit-status default-directory)
    nil))

;; Docker mode
;; -----------

(use-package dockerfile-mode
  :straight t
  :mode (("Dockerfile\\'" . dockerfile-mode)))

;; Tramp
;; -----
(use-package tramp
  :defer 5
  :config
  ;; Turn of auto-save for tramp files
  (defun tramp-set-auto-save ()
    (auto-save-mode -1))
  (with-eval-after-load 'tramp-cache
    (setq tramp-persistency-file-name "~/.emacs.d/etc/tramp"))
  (setq tramp-default-method "ssh"
        tramp-default-user-alist '(("\\`su\\(do\\)?\\'" nil "root"))
        tramp-adb-program "adb"
        ;; use the settings in ~/.ssh/config instead of Tramp's
        tramp-use-ssh-controlmaster-options nil
        backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               (not (let ((method (file-remote-p name 'method)))
                      (when (stringp method)
                        (member method '("su" "sudo"))))))))

  (use-package tramp-sh
    :config
    (add-to-list 'tramp-remote-path "/usr/local/sbin")
    (add-to-list 'tramp-remote-path "/opt/java/current/bin")
    (add-to-list 'tramp-remote-path "~/bin")))

;; Deadgrep
;; --------

(use-package deadgrep
  :straight t
  :defer 5
  :config
  (global-set-key (kbd "<f5>") #'deadgrep))


;; Spell check and flyspell settings
;; ---------------------------------

;; Standard location of personal dictionary
(setq ispell-personal-dictionary "~/.flydict")

;; Taken from dakrone who took it mostly from
;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
(when (executable-find "aspell")
  (setq ispell-program-name (executable-find "aspell"))
  (setq ispell-extra-args
        (list "--sug-mode=fast" ;; ultra/fast/normal/bad-spellers
              "--lang=en_GB" ;; TODO: can this be toggled for Greek?
              "--ignore=4")))

;; hunspell
;; (when (executable-find "hunspell")
;;   (setq ispell-program-name (executable-find "hunspell"))
;;   (setq ispell-extra-args '("-d en_GB")))

;; blindly copy-pasting here:
(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

(use-package flyspell
  :defer t
  :diminish ""
  :init (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  (use-package helm-flyspell
    :straight t
    :init
    (define-key flyspell-mode-map (kbd "M-S") #'helm-flyspell-correct)))

;; whitespace mode
(setq whitespace-style '(tabs newline space-mark
                              tab-mark newline-mark
                              face lines-tail trailing))

;; display pretty things for newlines and tabs
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. e.g. (insert-char 182 1)
      ;; 32 SPACE, 183 MIDDLE DOT
      '((space-mark nil)
        ;; 10 LINE FEED
        ;;(newline-mark 10 [172 10])
        (newline-mark nil)
        ;; 9 TAB, MIDDLE DOT
        (tab-mark 9 [183 9] [92 9])))

;; always turn on whitespace mode in programming buffers
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'whitespace-mode-hook (lambda () (diminish 'whitespace-mode)))
;; indicate trailing empty lines in the GUI
(setq-default show-trailing-whitespace t)

;; *********************
;; Programming Languages
;; *********************

;; remove some backends from vc-mode
(setq vc-handled-backends '(git))

;; highlight FIXME and TODO
(defun my/add-watchwords ()
  "Highlight FIXME, TODO, and NOCOMMIT in code TODO"
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME:?\\|TODO:?\\)\\>"
          1 '((:foreground "#d7a3ad") (:weight bold)) t))))

(add-hook 'prog-mode-hook #'my/add-watchwords)

;; highlight lines
(add-hook 'prog-mode-hook #'hl-line-mode)

;; hide the lighter in subword mode
(use-package subword
  :hook (prog-mode . subword-mode)
  :diminish "")

;; Language Server Protocol
;; ------------------------

(use-package lsp-mode
  :straight t
  :hook (ruby-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-enable-snippet nil))

(use-package helm-lsp
  :straight t
  :commands helm-lsp-workspace-symbol)
;; (use-package lsp-ui :commands lsp-ui-mode)

(use-package company-lsp
  :straight t
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

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
  :hook
  (clojure-mode . my/setup-clojure-hook)
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

;; Coffeescript
;; ------------

(use-package coffee-mode
  :straight t
  :mode (("\\.coffee.erb\\'" . coffee-mode))
  :config
  (setq coffee-tab-width 2)
  ;; remove the "Generated by CoffeeScript" header
  (add-to-list 'coffee-args-compile "--no-header"))

;; Shell
;; -----
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

;; Racket
;; ------

(use-package scheme-mode
  :hook (scheme-mode . paredit-mode))

(use-package geiser
  :straight t
  :hook
  (geiser-repl-mode . my/trailing-whitespace))

;; Elisp
;; -----

(defun my/turn-on-paredit-and-eldoc ()
  (interactive)
  (paredit-mode 1)
  (eldoc-mode 1))

(add-hook 'emacs-lisp-mode-hook #'my/turn-on-paredit-and-eldoc)
(add-hook 'ielm-mode-hook #'my/turn-on-paredit-and-eldoc)

(use-package eldoc
  :diminish eldoc-mode
  :config
  (setq eldoc-idle-deplay 0.3)
  (set-face-attribute 'eldoc-highlight-function-argument nil
                      :underline t :foreground "green"
                      :weight 'bold))

;; change the faces for elisp regex grouping
(set-face-foreground 'font-lock-regexp-grouping-backslash "#ff1493")
(set-face-foreground 'font-lock-regexp-grouping-construct "#ff8c00")

(defun ielm-other-window ()
  "Run ielm on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*ielm*"))
  (call-interactively 'ielm))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'ielm-other-window)
(define-key lisp-interaction-mode-map (kbd "C-c C-z") 'ielm-other-window)

(use-package elisp-slime-nav
  :straight t
  :diminish elisp-slime-nav-mode
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

;; pretty print results
(bind-key "M-:" 'pp-eval-expression)

(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(bind-key "C-x C-e" 'sanityinc/eval-last-sexp-or-region emacs-lisp-mode-map)

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

;; Rest Client
;; -----------

(use-package restclient
  :straight t
  :mode ("\\.rest\\'" . restclient-mode))

;; Web Mode
;; --------

(defun my/web-mode-hook ()
  ;; HTML offset indentation
  (setq web-mode-markup-indent-offset 2)
  ;; CSS offset indentation
  (setq web-mode-css-indent-offset 2)
  ;; Script/code offset indentation
  (setq web-mode-code-indent-offset 2))

(use-package web-mode
  :straight t
  :mode (("\\.erb\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.hbs\\'" . web-mode)
         ("\\.js[x]?\\'" . web-mode))
  :init
  (add-hook 'web-mode-hook  'my/web-mode-hook)
  ;; (lambda ()
  ;;   (when (string-equal "jsx" (file-name-extension buffer-file-name))
  ;;     (setup-tide-mode)))
  )

;; Elasticsearch

(use-package es-mode
  :straight t
  :config (use-package ob-elasticsearch)
  :mode "\\.es$")

;; Python
;; ------

(use-package python
  :defer t
  :init
  (setq mode-name "Python"))

(use-package anaconda-mode
  :straight t
  :defer t
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  :config
  (eldoc-mode 1))

;; Ruby
;; ----

(use-package ruby-mode
  :mode (("\\.rake\\'" . ruby-mode)
         ("Rakefile\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("Gemfile\\'" . ruby-mode)
         ("Guardfile\\'" . ruby-mode)
         ("Capfile\\'" . ruby-mode)
         ("\\.cap\\'" . ruby-mode))
  :hook
  (ruby-mode . company-mode)
  :config
  (progn
    (inf-ruby-minor-mode +1)
    (setq ruby-insert-encoding-magic-comment nil)))

;; We use rubocop via solargraph
;; (use-package rubocop
;;   :hook (ruby-mode . rubocop-mode)
;;   :diminish "")

(use-package ruby-tools
  :straight t
  :hook
  (ruby-mode . ruby-tools-mode)
  :diminish "")

(use-package rbenv
  :straight t
  :defer 25
  :init
  ;; I don't really care about the active Ruby in the modeline
  (progn
    (setq rbenv-show-active-ruby-in-modeline nil)
    (global-rbenv-mode t)))

(use-package rspec-mode
  :straight t
  :defer 20
  :diminish rspec-mode
  :commands rspec-mode
  :config
  (setq rspec-use-rvm nil))

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)

(use-package inf-ruby
  :init
  (add-hook 'after-init-hook 'inf-ruby-switch-setup))

;; Rust
;; ----

(use-package rust-mode
  :straight t
  :mode (("\\.rs\\'" . rust-mode))
  :hook
  (rust-mode . company-mode)
  (rust-mode . lsp))

(use-package flycheck-rust
  :straight t
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package toml-mode
  :straight t)

;; Javascript
;; ----------

(use-package add-node-modules-path
  :straight t
  :config (add-hook 'flycheck-mode-hook #'add-node-modules-path))

;; (setq-default js-indent-level 2)

;; (use-package js2-mode
;;   :mode "\\.js\\'"
;;   :config
;;   (js2-imenu-extras-setup)
;;   (setq-default js-auto-indent-flag nil)
;;   (electric-layout-mode nil)
;;   (define-key js2-mode-map (kbd "M-.") nil))

;; (use-package xref-js2
;;   :init
;;   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))

;; (use-package tide
;;   :ensure t
;;   :after (typescript-mode company flycheck)
;;   :hook (js2-mode
;;          (before-save . tide-format-before-save))
;;   :config
;;   (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))

;; (use-package rjsx-mode
;;   :mode "components\\/.*\\.js\\'")

;; Org-mode
;; --------

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)))

;; ****
;; Misc
;; ****

;; modeline
;; --------

(use-package smart-mode-line
  :straight t
  :init
  (progn
    (setq sml/theme 'respectful)
    (sml/setup))
  :config
  (setq sml/shorten-directory t
        sml/shorten-modes t))

;; fringe
;; ------

(defun my/set-fringe-background ()
  "Set the fringe background to the same color as the regular background."
  (interactive)
  (setq my/fringe-background-color
        (face-background 'default))
  (custom-set-faces
   `(fringe ((t (:background ,my/fringe-background-color))))))

(add-hook 'after-init-hook #'my/set-fringe-background)

;; Indicate where a buffer starts and stops
(setq-default indicate-buffer-boundaries 'right)

;; ediff
;; -----

(use-package ediff
  :straight t
  :config
  (progn
    (setq
     ;; Always split nicely for wide screens
     ediff-split-window-function 'split-window-horizontally)))

;; Requires emacs 27 and above

(when (not (version< emacs-version "27.0"))
  (use-package display-fill-column-indicator
    :straight (:type built-in)
    :hook
    (prog-mode . display-fill-column-indicator-mode)
    :diminish ""
    :config
    (setq display-fill-column-indicator-column 80)))

;; smooth-scrolling
;; ----------------

(use-package smooth-scrolling
  :straight t
  :config
  (setq smooth-scroll-margin 3
        scroll-margin 3
        scroll-conservatively 101
        scroll-preserve-screen-position t
        auto-window-vscroll nil))

;; paredit
;; -------

(use-package paredit
  :straight t
  :commands paredit-mode
  :diminish "()"
  :config
  (bind-key "M-)" #'paredit-forward-slurp-sexp paredit-mode-map)
  (bind-key "C-(" #'paredit-forward-barf-sexp paredit-mode-map)
  (bind-key "C-)" #'paredit-forward-slurp-sexp paredit-mode-map)
  (bind-key ")" #'paredit-close-parenthesis paredit-mode-map)
  (bind-key "M-\"" #'my/other-window-backwards paredit-mode-map)
  (bind-key "C-M-f" #'paredit-forward paredit-mode-map)
  (bind-key "C-M-b" #'paredit-backward paredit-mode-map))

;; electric modes
;; --------------

;; Automatically instert pairs of characters
(electric-pair-mode 1)
(setq electric-pair-preserve-balance t
      electric-pair-delete-adjacent-pairs t
      electric-pair-open-newline-between-pairs nil)
(show-paren-mode 1)

;; Auto-indentation
(electric-indent-mode 1)

;; Ignore electric indentation for python and yaml
(defun electric-indent-ignore-mode (char)
  "Ignore electric indentation for python-mode"
  (if (equal major-mode 'yaml-mode)
      'no-indent
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-mode)

;; Automatic layout
(electric-layout-mode 1)

;; smartparens
;; -----------

;; TODO
(use-package smartparens
  :straight t
  :disabled t
  :defer 5
  :diminish smartparens-mode)

;; flycheck
;; --------

(use-package flycheck
  :straight t
  :defer 5
  :bind (("M-g M-n" . flycheck-next-error)
         ("M-g M-p" . flycheck-previous-error)
         ("M-g M-=" . flycheck-list-errors))
  :init (global-flycheck-mode)
  :diminish flycheck-mode
  :config
  (progn
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                               ruby-reek
                                               javascript-jshint
                                               json-jsonlist))
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (use-package flycheck-pos-tip
      :straight t
      :init (flycheck-pos-tip-mode))
    (use-package helm-flycheck
      :straight t
      :init (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))))

;; with-editor
;; -----------

(use-package with-editor
  :straight t
  :init
  (progn
    (add-hook 'shell-mode-hook 'with-editor-export-editor)
    (add-hook 'eshell-mode-hook 'with-editor-export-editor)))

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

;; projectile
;; ----------

(use-package projectile
  :straight t
  :defer 5
  :commands projectile-global-mode
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
    :init
    (use-package grep) ;; required for helm-ag to work properly
    (setq projectile-completion-system 'helm)
    ;; no fuzziness for projectile-helm
    (setq helm-projectile-fuzzy-match nil)
    (helm-projectile-on))
  (projectile-global-mode))

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
  :diminish ""
  :init
  (add-hook 'prog-mode-hook 'git-gutter-mode)
  (add-hook 'org-mode-hook 'git-gutter-mode))

;; anzu
;; ----

(use-package anzu
  :straight t
  :defer t
  :bind ("M-%" . anzu-query-replace-regexp)
  :config
  (progn
    (use-package thingatpt)
    (setq anzu-mode-lighter "")
    (set-face-attribute 'anzu-mode-line nil :foreground "yellow")))

(add-hook 'prog-mode-hook #'anzu-mode)
(add-hook 'org-mode-hook #'anzu-mode)

;; helm-swoop
;; ----------

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

;; helm
;; ----

(use-package helm
  :straight t)

(use-package helm-config
  :demand t
  :diminish helm-mode
  :bind
  (("C-x C-f" . helm-find-files)
   ("M-y" . helm-show-kill-ring)
   ("C-x C-i" . helm-semantic-or-imenu)
   ("M-x" . helm-M-x)
   ("C-x b" . helm-mini)
   ("C-x C-o" . helm-occur))
  :config
  (use-package helm-files
    :config (setq helm-ff-file-compressed-list '("gz" "bz2" "zip" "tgz" "xz" "txz")))
  (use-package helm-buffers)
  (use-package helm-mode
    :diminish helm-mode
    :init (helm-mode 1))
  (use-package helm-misc)
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
   helm-semantic-fuzzy-match t
   helm-imenu-fuzzy-match t
   helm-completion-in-region-fuzzy-match t)

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

(use-package helm-flx
  :straight t
  :init (helm-flx-mode +1))

;; markdown-mode
;; -------------

(use-package markdown-mode
  :straight t
  :init (add-hook 'markdown-mode-hook #'whitespace-mode)
  :commands (markdown-mode gfm-mode)
  :mode (("\\README\\.md\\'" . gfm-mode)
         ("github\\.com.*\\.txt\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))

;; auto-completion (company)
;; -------------------------

(use-package company
  :straight t
  :defer t
  :diminish company-mode
  :bind ("C-." . company-complete)
  :init (add-hook #'prog-mode-hook #'company-mode)
  :config
  (setq company-idle-delay 0.25
        company-minimum-prefix-length 2
        company-tooltip-limit 14
        ;; company-selection-wrap-around t
        company-tooltip-align-annotations t
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

;; smart-tab
;; ---------

(use-package smart-tab
  :straight (smart-tab :branch "main" :repo "https://git.genehack.net/genehack/smart-tab.git")
  :defer t
  :diminish ""
  :init (global-smart-tab-mode 1)
  :config
  (progn
    (add-to-list 'smart-tab-disabled-major-modes 'mu4e-compose-mode)
    (add-to-list 'smart-tab-disabled-major-modes 'erc-mode)
    (add-to-list 'smart-tab-disabled-major-modes 'shell-mode)))

;; shrink-whitespace
;; -----------------

(use-package shrink-whitespace
  :straight t
  :bind (("M-SPC" . shrink-whitespace)
         ("M-S-SPC" . shrink-whitespace)))

;; undo-tree
;; ---------

(use-package undo-tree
  :straight t
  :init (global-undo-tree-mode t)
  :defer t
  :diminish ""
  :config
  (progn
    (define-key undo-tree-map (kbd "C-x u") 'undo-tree-visualize)
    (define-key undo-tree-map (kbd "C-/") 'undo-tree-undo)))

;; paren-face
;; ----------

(use-package paren-face
  :init (global-paren-face-mode))

;; ido-mode
;; --------

(use-package ido
  :config
  (use-package ido-completing-read+
    :init (ido-ubiquitous-mode 1))
  (use-package flx-ido
    :init (flx-ido-mode 1)
    :config (setq ido-use-faces nil))
  (use-package ido-vertical-mode
    :disabled t
    :init (ido-vertical-mode t))
  (setq ido-use-virtual-buffers nil
        ;; this settings causes weird TRAMP connections, don't set it!
        ;; ido-enable-tramp-completion nil
        ido-enable-flex-matching t
        ido-auto-merge-work-directories-length nil
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10))

;; iedit
;; -----

;; Use it to edit every instance of a word in the buffer.
(use-package iedit
  :straight t
  :bind ("C-;" . iedit-mode))

;; beacon
;; ------

;; flash cursor whenever you adjust position.
(use-package beacon
  :straight t
  :diminish beacon-mode
  :init (beacon-mode 1))

;; nix
;; ---
(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")

;; smartscan
;; ---------

;; Jump between the same variable in multiple places.
;; (use-package smartscan
;;   :straight t
;;   :init (add-hook #'prog-mode-hook #'smartscan-mode)
;;   :config
;;   (bind-key "M-'" #'other-window smartscan-map))

;; vlf
;; ---

;; View large files
(use-package vlf
  :straight t
  :config
  (require 'vlf-setup))

;; rainbow-delimiters-mode
;; -----------------------

;; Use different colors per set of parenthesis. Only in lisps.
(use-package rainbow-delimiters
  :straight t
  :init
  (add-hook #'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook #'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook #'scheme-mode-hook #'rainbow-delimiters-mode))

;; *******************
;; Extra Functionality
;; *******************

(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "Open file with emacsclient with cursors positioned on requested line.
Most of console-based utilities prints filename in format
'filename:linenumber'.  So you may wish to open filename in that format.
Just call:

  emacsclient filename:linenumber

and file 'filename' will be opened and cursor set on line 'linenumber'"
  (ad-set-arg 0
              (mapcar (lambda (fn)
                        (let ((name (car fn)))
                          (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                              (cons
                               (match-string 1 name)
                               (cons (string-to-number (match-string 2 name))
                                     (string-to-number (or (match-string 3 name) ""))))
                            fn))) files)))

;; Better C-a
;; See http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
;; Code borrowed from Prelude

(defun smart-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'smart-move-beginning-of-line)

;; Cleaning a buffer
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defvar bad-cleanup-modes '(python-mode yaml-mode)
  "List of modes where `cleanup-buffer' should not be used")

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer. If the
buffer is one of the `bad-cleanup-modes' then only whitespace is stripped."
  (interactive)
  (unless (member major-mode bad-cleanup-modes)
    (progn
      (indent-buffer)
      (untabify-buffer)))
  (whitespace-cleanup))

;; Perform general cleanup.
(global-set-key (kbd "C-c n") #'cleanup-buffer)

;; Clean whitespace after saving
(add-hook 'before-save-hook 'whitespace-cleanup)

;; ************
;; Key Bindings
;; ************

;; Join on killing lines
(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))

(global-set-key (kbd "C-k") 'kill-and-join-forward)

;; Join line to next line
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Window switching
;; (defun my/other-window-backwards ()
;;   (interactive)
;;   (other-window -1))

;;(global-set-key (kbd "C-s-'") 'previous-multiframe-window)
(global-set-key (kbd "C-s-'") 'previous-multiframe-window)
(global-set-key (kbd "C-s-n") 'next-multiframe-window)
(global-set-key (kbd "C-s-p") 'previous-multiframe-window)
;; (global-set-key (kbd "M-'") 'other-window)
;; (global-set-key (kbd "M-\"") 'my/other-window-backwards)
;; (global-set-key (kbd "H-'") 'other-window)
(global-set-key [C-tab] 'next-multiframe-window)
(global-set-key [C-S-tab] 'previous-multiframe-window)

;; Next two functions are borrowed from emacs prelude.

(defun my/smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun my/smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.

With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (my/prelude-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(global-set-key (kbd "s-o") 'my/smart-open-line-above)
(global-set-key (kbd "M-o") 'my/smart-open-line)

;; **************
;; Finalize Setup
;; **************

(setq debug-on-error nil)
(setq debug-on-quit nil)

;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading settings...done (%.3fs)" elapsed))
(put 'narrow-to-region 'disabled nil)
