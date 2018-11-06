(use-package vc-hooks
  :config
  (setq vc-follow-symlinks t))                     ; follow symlinks to their targets

(use-package magit
  :straight (:host github :repo "magit/magit"
                   :files ("lisp/magit"
                           "lisp/magit*.el"
                           "lisp/git-rebase.el"
                           "Documentation/magit.texi"
                           "Documentation/AUTHORS.md"
                           "LICENSE"))
  :bind ("C-g" . magit-status))

(use-package diff-hl
  :disabled  ; laggy

  :straight (:host github :repo "dgutov/diff-hl")
  :init
  (global-diff-hl-mode)
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package git-gutter
  :straight (:host github :repo "syohex/emacs-git-gutter")
  :hook
  (vc-mode-hook . global-git-gutter-mode))

(use-package git-gutter-fringe
  :straight (:host github :repo "syohex/emacs-git-gutter-fringe")
  :after git-gutter)


(provide 'init-vc)
