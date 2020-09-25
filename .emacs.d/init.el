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

;; *******************
;; Extra Functionality
;; *******************

;; (defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
;;   "Open file with emacsclient with cursors positioned on requested line.
;; Most of console-based utilities prints filename in format
;; 'filename:linenumber'.  So you may wish to open filename in that format.
;; Just call:

;;   emacsclient filename:linenumber

;; and file 'filename' will be opened and cursor set on line 'linenumber'"
;;   (ad-set-arg 0
;;               (mapcar (lambda (fn)
;;                         (let ((name (car fn)))
;;                           (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
;;                               (cons
;;                                (match-string 1 name)
;;                                (cons (string-to-number (match-string 2 name))
;;                                      (string-to-number (or (match-string 3 name) ""))))
;;                             fn))) files)))

;; ************
;; Key Bindings
;; ************

;; Join on killing lines
;; (defun kill-and-join-forward (&optional arg)
;;   "If at end of line, join with following; otherwise kill line.
;; Deletes whitespace at join."
;;   (interactive "P")
;;   (if (and (eolp) (not (bolp)))
;;       (delete-indentation t)
;;     (kill-line arg)))

;; (global-set-key (kbd "C-k") 'kill-and-join-forward)

;; ;; Join line to next line
;; (global-set-key (kbd "M-j")
;;                 (lambda ()
;;                   (interactive)
;;                   (join-line -1)))

;; **************
;; Finalize Setup
;; **************

;; Hooks
(add-hook 'after-startup-hook
          (lambda ()
            (message "Emacs is here")))

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

;; turn debugging back off
(setq debug-on-error nil)
(setq debug-on-quit nil)

;;(put 'narrow-to-region 'disabled nil)
