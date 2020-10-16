;; ------------------
;; Core Configuration
;; ------------------
(provide 'mod-core)

(setq user-full-name "Nikos Fertakis"
      user-mail-address "nikos.fertakis@gmail.com")

;; UTF-8 everywhere
(set-language-environment "UTF-8")
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; turn on syntax highlighting for all buffers
(global-font-lock-mode t)

;; raise maximum number of logs in *Messages* buffer
(setq message-log-max 16384)

;; Configure the GC
;; ----------------

;; see https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/

;; 20 mb
(defvar my/default-gc-threshold (* 20 1024 1024)
  "Default `gc-cons-threshold' after startup")

;; 100 mb
(defvar my/large-gc-threshold (* 100 1024 1024)
  "Large `gc-cons-threshold' to avoid gc when needed")

;; set to `t' to display GC messages
(setq garbage-collection-messages nil)

;; avoid gc during startup (in early-init.el)
;;(setq gc-cons-threshold my/large-gc-threshold)
;; set back to default after startup finishes
(add-hook 'after-init-hook
          (lambda ()
            (message "Resetting garbage collection..")
            (setq gc-cons-threshold my/default-gc-threshold)))

;; also prevent GC while in the minibuffer
(defun my/minibuffer-setup-hook ()
  (setq gc-cons-threshold (* 100 1024 1024)))

(defun my/minibuffer-exit-hook ()
  (setq gc-cons-threshold my/default-gc-threshold))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook)

;; TODO: This is suggested for LSP but maybe not make it global?
;; See https://github.com/hlissner/doom-emacs/pull/2590/files
;;(setq read-process-output-max (* 1024 1024))
;; 1 mb

;; wait a bit more than the default 0.5 sec before assuming idle
(setq idle-update-delay 2)

;; toggle line number limit
(setq line-number-display-limit-width 10000)

;; make gnutls a bit safer
(setq gnutls-min-prime-bits 4096)

;; delete selected region on typing
(delete-selection-mode 1)

;; warn me if a file is at least 25mb
(setq large-file-warning-threshold (* 25 1024 1024))

;; disable mark when changing buffer or focus
(transient-mark-mode 1)

;; don't indicate empty lines
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
(setq inhibit-startup-screen t)

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
;; column threshold
(setq-default fill-column 80)

;; fix some weird color escape sequences
(setq system-uses-terminfo nil)

;; resolve symlinks
(setq-default find-file-visit-truename t)

;; require newline at the end of files
(setq require-final-newline t)

;; uniquify buffer names
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; search (and search/replace) using regex by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)

;; don't prompt before killing buffer
(global-set-key (kbd "C-x k") #'kill-this-buffer)

;; single space ends a sentence
(setq sentence-end-double-space nil)

;; split windows
(setq split-height-threshold nil)
(setq split-width-threshold 180)

;; rescan for imenu changes
(set-default 'imenu-auto-rescan t)

;; seed random number generator
(random t)

;; switch to unified diffs by default
(setq diff-switches "-u")

;; autofill vs visual fill in text buffers (will a newline be created automatically?)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;;(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(use-package diminish
 :straight t
 :init
 (diminish 'auto-fill-function ""))

;; set the internal calculator not to go to scientific form quite so quickly
(setq calc-display-sci-low -5)

;; never kill the *scratch* buffer
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;; start server if not running but only for gui
;; Lame, server has bad autoloads :(
(require 'server nil t)
(use-package server
  :if window-system
  :init
  (when (not (server-running-p server-name))
    (server-start)))

;; use sane values for tls
(setq tls-program
      ;; Defaults:
      ;; '("gnutls-cli --insecure -p %p %h"
      ;;   "gnutls-cli --insecure -p %p %h --protocols ssl3"
      ;;   "openssl s_client -connect %h:%p -no_ssl2 -ign_eof")
      '(;;"gnutls-cli -p %p %h"
        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

;; save histories
(setq savehist-file (concat user-emacs-directory "savehist"))
(savehist-mode 1)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(search-ring regexp-search-ring kill-ring))

;; remember where I've left off in each file
(save-place-mode 1)

;; restore window placement
(add-hook 'after-init-hook #'winner-mode)

;; settings for what to do with temporary files
;; delete auto-save files
(setq delete-auto-save-files t)
;; Create the directory for backups if it doesn't exist
(when (not (file-exists-p "~/.emacs_backups"))
  (make-directory "~/.emacs_backups"))

(setq-default backup-directory-alist
              '((".*" . "~/.emacs_backups")))
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs_backups/" t)))

;; delete old backups silently
(setq delete-old-versions t)

;; delete trailing whitespace from touched lines only
(use-package ws-butler
  :straight t
  :diminish ws-butler-mode
  :hook ((prog-mode org-mode text-mode) . ws-butler-mode))

;; enable automatic indentation
;; (use-package auto-indent-mode
;;   :straight t)

;; sane undo defaults
(use-package undo-tree
  :straight t
  :init (global-undo-tree-mode t)
  :defer t
  :diminish ""
  :config
  (progn
    (define-key undo-tree-map (kbd "C-x u") 'undo-tree-visualize)
    (define-key undo-tree-map (kbd "C-/") 'undo-tree-undo)))

;; shrink whitespace conditionally
(use-package shrink-whitespace
  :straight t
  :bind (("M-SPC" . shrink-whitespace)
         ("M-S-SPC" . shrink-whitespace)))

;; display search match information

(use-package anzu
  :straight t
  :defer t
  :bind ("M-%" . anzu-query-replace-regexp)
  :hook ((prog-mode org-mode) . anzu-mode)
  :config
  (progn
    (use-package thingatpt)
    (setq anzu-mode-lighter ""
          ;; spaceline already takes care of this
          ;;anzu-cons-mode-line-p nil
          )
    (set-face-attribute 'anzu-mode-line nil :foreground "yellow")))

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
  (delete-trailing-whitespace))

;; Perform general cleanup.
(global-set-key (kbd "C-c n") #'cleanup-buffer)

;; be smart about selecting text
(use-package expand-region
  :straight t
  :defer t
  :bind (("C-=" . er/expand-region)))

;; multiple cursors (e.g. like rectangle selection)
(use-package multiple-cursors
  :straight t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; view large files
(use-package vlf
  :straight t
  :init (require 'vlf-setup))

;; use M-x proced for process management, make it auto-update
(setq-default proced-auto-update-flag t)
(setq-default proced-auto-update-interval 5)

;; disable bidirectional display for perf
(setq-default bidi-display-reordering nil)

;; don’t bother saving things to the kill-ring twice, remove duplicates
(setq kill-do-not-save-duplicates t)

;; preserve the window location when opening things
(setq switch-to-buffer-preserve-window-point t)

;; Use a sane re-builder syntax so I don’t have to have crazy escapes, see: https://masteringemacs.org/article/re-builder-interactive-regexp-builder
(setq reb-re-syntax 'string)

;; ignore case when performing completion
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)

;; automatically revert file if changed on disk
(global-auto-revert-mode 1)
;; be quiet about reverting files
(setq auto-revert-verbose nil)

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
              "--lang=en_US" ;; TODO: can this be toggled for Greek?
              "--ignore=4")))

;; hunspell
(when (executable-find "hunspell")
  (setq ispell-program-name (executable-find "hunspell"))
  (setq ispell-extra-args '("-d en_US"
                            "-p ~/.flydict")))

;; blindly copy-pasting here:
(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

;; in non-programming modes use M-. to spellcheck the word
(use-package flyspell
  :defer t
  :diminish flyspell-mode
  :hook (prog-mode . flyspell-prog-mode))

;; handle excessively long lines
(when (require 'so-long nil :noerror)
  ;; 750 columns means it's too long! (default is 250)
  (setq so-long-threshold 750)
  (add-to-list 'so-long-minor-modes 'rainbow-delimiters-mode)
  (add-to-list 'so-long-minor-modes 'paren-face-mode)
  (add-to-list 'so-long-minor-modes 'electric-indent-mode)
  (add-to-list 'so-long-minor-modes 'electric-pair-mode)
  (add-to-list 'so-long-minor-modes 'electric-layout-mode)
  (add-to-list 'so-long-minor-modes 'idle-highlight-mode)
  (add-to-list 'so-long-minor-modes 'show-paren-mode)
  (add-to-list 'so-long-minor-modes 'git-gutter-mode)
  (so-long-enable)

  (defun my/so-long-hook () "Used in `so-long-hook'.")

  (add-hook 'so-long-hook #'my/so-long-hook))
