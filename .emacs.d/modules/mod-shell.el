;; -----------------------
;; Configuration for shell
;; -----------------------
(provide 'mod-shell)

;; Setting up $PATH and other vars
(use-package exec-path-from-shell
  :straight t
  :defer t
  :init
  (progn
    (setq exec-path-from-shell-variables '("JAVA_HOME"
                                           "PATH"
                                           "WORKON_HOME"
                                           "MANPATH"
                                           "NVM_PATH"
                                           "LANG"))
    (exec-path-from-shell-initialize)))

(use-package with-editor
  :straight t
  :init
  (progn
    (add-hook 'shell-mode-hook 'with-editor-export-editor)
    (add-hook 'eshell-mode-hook 'with-editor-export-editor)))

;; use cat for shell pager
(setenv "PAGER" "cat")

(setq comint-scroll-to-bottom-on-input t ;; always insert at the bottom
      ;; always add output at the bottom
      comint-scroll-to-bottom-on-output nil
      ;; scroll to show max possible output
      comint-scroll-show-maximum-output t
      ;; no duplicates in command history
      comint-input-ignoredups t
      ;; insert space/slash after file completion
      comint-completion-addsuffix t
      ;; if this is t, it breaks shell-command
      comint-prompt-read-only nil)

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

(add-hook 'shell-mode-hook #'set-scroll-conservatively)
;; truncate buffers continuously
(add-hook 'comint-output-filter-functions #'comint-truncate-buffer)
;; interpret and use ansi color codes in shell output windows
(add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)
