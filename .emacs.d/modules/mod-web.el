;; --------------------------
;; Configuration for web mode
;; --------------------------
(provide 'mod-web)

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
         ("\\.json\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.hbs\\'" . web-mode)
         ("\\.js[x]?\\'" . web-mode))
  :init
  (add-hook 'web-mode-hook  'my/web-mode-hook))
