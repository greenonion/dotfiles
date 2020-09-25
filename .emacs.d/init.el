;; Credits to dakrone for the inspiration.
;;
;; Turn on debugging
(setq debug-on-error t)
(setq debug-on-quit t)

(require 'cl-lib)

(defvar after-startup-hook nil
  "Hooks to run after we're done.")

;; Keep track of loading time
(defconst emacs-start-time (current-time)
  "Time Emacs was started.")

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

;; Keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Modules
;; -------

(add-to-list 'load-path "~/.emacs.d/modules/")

(defmacro try-load (module)
  "Try to load the given module, logging an error if unable to load"
  `(condition-case ex
       (require ,module)
     ('error
      (message "Unable to load [%s] module: %s" ,module ex))))

(try-load 'mod-core)
(try-load 'mod-helm)
(try-load 'mod-appearance)
(try-load 'mod-navigation)
(try-load 'mod-completion)
(try-load 'mod-programming)
(try-load 'mod-git)
(try-load 'mod-clojure)
(try-load 'mod-shell)
(try-load 'mod-remote)
(try-load 'mod-lsp)
(try-load 'mod-writing)
(try-load 'mod-web)

;; **************
;; Finalize Setup
;; **************

;; Hooks
(add-hook 'after-startup-hook
          (lambda ()
            (message "Epiphyte has been loaded")))

(defun my/time-since-start ()
  (float-time (time-subtract (current-time)
                             emacs-start-time)))

(add-hook 'after-my-hook
          `(lambda ()
             (let ((elapsed (my/time-since-start)))
               (message "Loading %s...done (%.3fs)"
                        ,load-file-name elapsed))) t)
(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (my/time-since-start)))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)
(run-hooks 'after-my-hook)

(setq initial-scratch-message ";; Welcome to Epiphyte")

;; turn debugging back off
(setq debug-on-error nil)
(setq debug-on-quit nil)
