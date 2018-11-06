(use-package company
  :straight (:host github :repo "company-mode/company-mode")
  :bind ("C-j" . company-complete))

(use-package company-jedi
  :straight (:host github :repo "syohex/emacs-company-jedi")
  :hook
  (python-mode . (lambda ()
                   (add-to-list 'company-backends 'company-jedi))))

(use-package which-key
  :straight (:host github :repo "justbur/emacs-which-key")
  :init (which-key-mode))

;; `icomplete' provides minibuffer completion
(use-package icomplete
  :init
  (use-package minibuffer
    :init
    (setq read-buffer-completion-ignore-case t)    ; ignore case when completing buffer names
    :config
    (setq read-file-name-completion-ignore-case t)) ; ignore case when completing file names

  :config
  (icomplete-mode t))


(provide 'init-completions)
