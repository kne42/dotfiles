(use-package all-the-icons
  :straight (:host github :repo "domtronn/all-the-icons.el"
                   :files (:defaults "data")))

(use-package neotree
  :straight (:host github :repo "jaypei/emacs-neotree")
  :demand

  :bind ("C-' C-l" . neotree-toggle)
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t))

(use-package spaceline
  :straight (:host github :repo "TheBB/spaceline"))

(use-package spaceline-all-the-icons
  :straight (:host github :repo "domtronn/spaceline-all-the-icons.el")

  :after (spaceline all-the-icons)
  :config (spaceline-all-the-icons-theme))

(use-package doom-themes
  :straight (:host github :repo "hlissner/emacs-doom-themes"
                   :files (:defaults "themes/*.el"))

  :after (neotree all-the-icons)
  :init
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme
  (doom-themes-neotree-config))  ; all-the-icons fonts must be installed!


(provide 'init-display)
