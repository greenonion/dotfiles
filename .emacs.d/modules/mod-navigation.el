;; ------------------
;; General Navigation
;; ------------------
(provide 'mod-navigation)

;; Window switching
;; ----------------

(global-set-key (kbd "C-s-'") 'previous-multiframe-window)
(global-set-key (kbd "C-s-n") 'next-multiframe-window)
(global-set-key (kbd "C-s-p") 'previous-multiframe-window)
(global-set-key [C-tab] 'next-multiframe-window)
(global-set-key [C-S-tab] 'previous-multiframe-window)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Moving selected text with meta and arrow keys
(use-package move-text
  :straight t
  :init (move-text-default-bindings))

;; Search engines

(use-package engine-mode
  :straight (engine-mode :branch "main")
  :defer 10
  :init (engine-mode 1)
  :config

  (defengine google
    "https://google.com/search?q=%s"
    :keybinding "g")

  (defengine elasticsearch
    "https://github.com/elastic/elasticsearch/search?q=%s&type="
    :keybinding "e")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"))

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
