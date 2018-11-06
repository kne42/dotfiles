(use-package anaconda-mode
  :straight (:host github :repo "proofit404/anaconda-mode")
  :hook python-mode)

(use-package sgml-mode
  :magic-fallback ("<!DOCTYPE html>" . html-mode))

(use-package scss-mode
  :straight (:host github :repo "antonj/scss-mode")
  :custom
  (sass-scss-command "/usr/local/bin/scss")
  :config
  (setq exec-path (cons (expand-file-name "/usr/local/bin") exec-path)))

(use-package yaml-mode
  :straight (:host github :repo "yoshiki/yaml-mode")
  :mode "\\.yml\\'")


(provide 'init-modes)
