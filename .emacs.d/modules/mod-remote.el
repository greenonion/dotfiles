;; ------------------------------------
;; Configuration for remote connections
;; ------------------------------------
(provide 'mod-remote)

(use-package tramp
  :defer 5
  :config
  ;; Turn off auto-save for tramp files
  (defun tramp-set-auto-save ()
    (auto-save-mode -1))
  (with-eval-after-load 'tramp-cache
    (setq tramp-persistency-file-name "~/.emacs.d/tramp"))
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
    (add-to-list 'tramp-remote-path "/opt/gradle/current/bin")
    (add-to-list 'tramp-remote-path "~/bin")))
