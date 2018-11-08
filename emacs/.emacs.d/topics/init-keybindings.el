(use-package bind-key
  :straight (:host github :repo "jwiegley/use-package" :files ("bind-key.el"))

  :init
  (keyboard-translate ?\C-i ?\H-i)

  :config
  (bind-keys*
   ;; movement
   ("C-w" . previous-line)
   ("C-s" . next-line)
   ("C-a" . backward-char)
   ("C-d" . forward-char)

   ("C-q" . backward-word)
   ("C-e" . forward-word)

   ("M-q" . move-beginning-of-line)
   ("M-e" . move-end-of-line)

   ("C-r" . backward-paragraph)
   ("C-f" . forward-paragraph)

   ;; deletion
   ("H-i" . delete-indentation)
   ("C-M-s" . kill-region)
   ("M-c" . kill-ring-save)
   ("M-s" . kill-line)
   ("C-M-x" . append-next-kill)

   ("C-M-a" . delete-backward-char)
   ("C-M-d" . delete-forward-char)

   ("C-M-q" . backward-kill-word)
   ("C-M-e" . kill-word)

   ("C-M-w" . yank)

   ("M-w" . undo)

   ;; general utilities
   ("M-r" . set-mark-command)

   ("C-/" . comment-or-uncomment-region)

   ("M-f" . isearch-forward)
   ("C-M-f" . replace-regexp)

   ("C-' C-'" .  buffer-menu)
   ("C-' C-;" .  find-file)

   ("C-; ;" . (lambda () (interactive)
                (select-window (split-window-right))
                (buffer-menu)))
   ("C-; '" . (lambda () (interactive)
                (select-window (split-window-below))
                (buffer-menu)))
   ("C-l" . universal-argument)
   ("<escape>" . keyboard-escape-quit))  ; hit it once instead of 3x

  (bind-keys :map isearch-mode-map
             ("C-f" . isearch-repeat-forward)
             ("C-r" . isearch-repeat-backward))

  (global-unset-key (kbd "C-u"))
  (bind-keys :map universal-argument-map
             ("C-l" . universal-argument-more)
             ("C-u" . nil)))


(provide 'init-keybindings)
