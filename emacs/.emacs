;;-----------------------------------------------------------------------------
;; Bootstrapping
;;-----------------------------------------------------------------------------

;; disable GNU Elpa
(setq package-archives nil)


;; install `straight'
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


;; install `use-package' macro
;; provides the keyword `:straight'
(setq use-package-compute-statistics t)
(straight-use-package
 '(use-package :host github :repo "jwiegley/use-package"))


;;;; https://github.com/raxod502/radian/blob/develop/emacs/radian.el
;;; Prevent Emacs-provided Org from being loaded

;; The following is a temporary hack until straight.el supports
;; building Org, see:
;;
;; * https://github.com/raxod502/straight.el/issues/211
;; * https://github.com/raxod502/radian/issues/410
;;
;; There are three things missing from our version of Org: the
;; functions `org-git-version' and `org-release', and the feature
;; `org-version'. We provide all three of those ourself, therefore.

;; `git' provides convenience functions for running Git.
(use-package git
  :straight (:host github :repo "rejeep/git.el"))

(defun org-git-version ()
  "The Git version of org-mode.
  Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
  Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

;; Our real configuration for Org comes much later. Doing this now
;; means that if any packages that are installed in the meantime
;; depend on Org, they will not accidentally cause the Emacs-provided
;; (outdated and duplicated) version of Org to be loaded before the
;; real one is registered.
(straight-use-package 'org)


;;-----------------------------------------------------------------------------
;; General Utilities
;;-----------------------------------------------------------------------------

;; determine launch info
(defconst *is-a-mac*
  (eq system-type 'darwin)
  "Is this running on OS X?")

(defconst *is-carbon-emacs*
  (and *is-a-mac* (eq window-system 'mac))
  "Is this the Carbon port of Emacs?")

(defconst *is-cocoa-emacs*
  (and *is-a-mac* (eq window-system 'ns))
  "Is this the Cocoa version of Emacs?")

(defconst *is-linux*
  (eq system-type 'gnu/linux)
  "Is this running on Linux?")

(defconst *from-app*
  (display-graphic-p)
  "Is this running from an application?")

(defconst *from-term*
  (not *from-app*)
  "Is this running from a terminal?")


(defun minimize-startup-p ()
  (member "--minimize-startup" command-line-args))

(add-to-list 'command-switch-alist
             '("--minimize-startup"
               . (lambda (switch) t)))

(defun no-args-p ()
  (or (eq (length command-line-args) 1)
      (and (eq (length command-line-args) 2) (minimize-startup-p))))


;; https://emacs.stackexchange.com/a/30032
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))


;; `diminish' provides diminishing of minor modes by removing their modeline
;; display
;; `use-package' integration: `:diminish'
(use-package diminish
  :straight (:host github :repo "myrjola/diminish.el"))


;; `el-patch' provides on-the-fly patching of functions from other packages
;; via sexp-based diffs
;; `use-package' integration: `:init/el-patch', `:config/el-patch'
(use-package el-patch
  :straight (:host github :repo "raxod502/el-patch"))


;;-----------------------------------------------------------------------------
;; General Settings
;;-----------------------------------------------------------------------------


(use-package simple
  :init
  (setq
   inhibit-startup-screen t                 ; disable startup message
   load-prefer-newer t                      ; prefer newer .el over .elc
   kill-ring-max 5000                       ; truncate kill ring at 5000 entries
   mark-ring-max 5000                       ; truncate mark ring at 5000 entries
   track-eol nil                            ; cursor doesn't track end-of-line
   mouse-yank-at-point t                    ; paste at cursor position
   sentence-end-double-space nil            ; sentences end with one space
   truncate-partial-width-windows nil       ; don't truncate long lines
   column-number-mode t)                    ; show column number in the mode-line

  (setq initial-buffer-choice ((lambda () (if (no-args-p)
                                              default-directory
                                            nil))))
  
  (setq-default
   indicate-empty-lines t                   ; show empty lines
   indent-tabs-mode nil                     ; use spaces instead of tabs
   tab-width 4)                             ; tab length
  
  :config
  ;; app
  (if (display-graphic-p)
      (progn (tool-bar-mode -1)                  ; no toolbar
             (menu-bar-mode -1))))                  ; no menubar

(diminish 'eldoc-mode)

(use-package prog-mode
  :config
  (unbind-key "C-M-Q" prog-mode-map))

;; `paren' highlights matching parenthesis pairs
(use-package paren
  :init
  (setq blink-matching-paren-distance nil)        ; no blinking parenthesis
  :config
  (show-paren-mode t))

;; `hl-line' highlights the current line
(use-package hl-line
  :config
  (global-hl-line-mode t))

(use-package mwheel
  :if *from-app*
  :config
  (mouse-wheel-mode nil))           ; mouse-wheel disabled

(use-package frame
  :init
  (add-to-list 'default-frame-alist '(alpha . (98 . 85)))
  :config
  (blink-cursor-mode -1)                     ; no blinking cursor
  (set-frame-parameter (selected-frame) 'alpha '(98 . 85)))

(use-package files
  :init
  (defvar delete-trailing-on-save t)        ; delete trailing whitespaces on save

  :hook
  (before-save-hook . (lambda () (when delete-trailing-on-save
                                   (delete-trailing-whitespace))))

  :config
  (setq require-final-newline t))                  ; add newline at the end of every file

(use-package subr
  :init
  (provide 'subr)
  :config
  (defalias 'yes-or-no-p 'y-or-n-p))         ; y/n instead of yes/no

(use-package autorevert
  :config
  (global-auto-revert-mode t))

;; modifying text replaces the region
(use-package delsel
  :config
  (delete-selection-mode t))

(use-package scroll-bar
  :if *from-app*
  :init
  (setq scroll-preserve-screen-position t) ; scroll without moving cursor
  :config
  (set-scroll-bar-mode 'right)        ; set scrollbar right
  (scroll-bar-mode -1))                ; disable scrollbar

(use-package smooth-scrolling
  :straight (:host github :repo "aspiers/smooth-scrolling")
  :init (setq smooth-scroll-margin 20)
  :config (smooth-scrolling-mode 1))

(use-package elisp-mode
  :config
  (unbind-key "C-M-q" emacs-lisp-mode-map)
  (use-package aggressive-indent
    :straight (:host github :repo "Malabarba/aggressive-indent-mode")
    :hook
    (emacs-lisp-mode . global-aggressive-indent-mode)))

(use-package exec-path-from-shell
  :straight (:host github :repo "purcell/exec-path-from-shell")
  :if (memq window-system '(mac ns x))
  :config
  (delete "-i" exec-path-from-shell-arguments)
  (exec-path-from-shell-initialize))

(use-package ace-window
  :straight (:host github :repo "abo-abo/ace-window")
  :bind ("C-o" .  ace-window))

(use-package expand-region
  :straight (:host github :repo "magnars/expand-region.el")
  :bind ("C-M-c" . er/expand-region))


(use-package realgud
  :straight (:host github :repo "realgud/realgud"
                   :files ("realgud.el"
                           ("realgud/common" "realgud/common/*.el")
                           ("realgud/common/buffer" "realgud/common/buffer/*.el")
                           ("realgud/debugger/bashdb" "realgud/debugger/bashdb/*.el")
                           ("realgud/debugger/gdb" "realgud/debugger/gdb/*.el")
                           ("realgud/debugger/gub" "realgud/debugger/gub/*.el")
                           ("realgud/debugger/ipdb" "realgud/debugger/ipdb/*.el")
                           ("realgud/debugger/jdb" "realgud/debugger/jdb/*.el")
                           ("realgud/debugger/kshdb" "realgud/debugger/kshdb/*.el")
                           ("realgud/debugger/nodejs" "realgud/debugger/nodejs/*.el")
                           ("realgud/debugger/pdb" "realgud/debugger/pdb/*.el")
                           ("realgud/debugger/perldb" "realgud/debugger/perldb/*.el")
                           ("realgud/debugger/rdebug" "realgud/debugger/rdebug/*.el")
                           ("realgud/debugger/remake" "realgud/debugger/remake/*.el")
                           ("realgud/debugger/trepan" "realgud/debugger/trepan/*.el")
                           ("realgud/debugger/trepan.pl" "realgud/debugger/trepan.pl/*.el")
                           ("realgud/debugger/trepan2" "realgud/debugger/trepan2/*.el")
                           ("realgud/debugger/trepan3k" "realgud/debugger/trepan3k/*.el")
                           ("realgud/debugger/trepanjs" "realgud/debugger/trepanjs/*.el")
                           ("realgud/debugger/zshdb" "realgud/debugger/zshdb/*.el")
                           ("realgud/lang" "realgud/lang/*.el")))
  :defer)

;;-----------------------------------------------------------------------------
;; Keybindings
;;-----------------------------------------------------------------------------

(use-package bind-key
  :straight (:host github :repo "jwiegley/use-package" :files ("bind-key.el"))

  :init
  (defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
    "When called, do not close windows."
    (let (orig-one-window-p)
      (fset 'orig-one-window-p (symbol-function 'one-window-p))
      (fset 'one-window-p (lambda (&optional nomini all-frames) t))
      (unwind-protect
          ad-do-it
        (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

  (defadvice kill-region (before slick-cut activate compile)
    "When called interactively with no active region, kill a single line instead."
    (interactive
     (if mark-active
         (list (region-beginning) (region-end))
       (list (line-beginning-position) (line-beginning-position 2)))))

  (defadvice delete-region (before slick-cut activate compile)
    "When called interactively with no active region, delete a single line instead."
    (interactive
     (if mark-active
         (list (region-beginning) (region-end))
       (list (line-beginning-position) (line-beginning-position 2)))))

  (defadvice kill-ring-save (before slick-copy activate compile)
    "When called interactively with no active region, copy a single line instead."
    (interactive
     (if mark-active
         (list (region-beginning) (region-end))
       (message "Copied line")
       (list (line-beginning-position) (line-beginning-position 2)))))

  (defadvice comment-or-uncomment-region (before mark-whole-line activate compile)
    "When called interactively with no active region, toggle a single line instead."
    (interactive
     (if mark-active
         (list (region-beginning) (region-end))
       (list (line-beginning-position) (line-beginning-position 2)))))

  (when *from-app*
    (keyboard-translate ?\C-i ?\H-i))

  
  :config
  (bind-keys
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


;;-----------------------------------------------------------------------------
;; Display
;;-----------------------------------------------------------------------------

(use-package all-the-icons
  :straight (:host github :repo "domtronn/all-the-icons.el"
                   :files (:defaults "data")))

(use-package neotree
  :straight (:host github :repo "jaypei/emacs-neotree")
  :after (doom-themes all-the-icons)
  :bind ("C-' C-l" . neotree-toggle)
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  :config
  ;; Enable custom neotree theme
  (doom-themes-neotree-config))  ; all-the-icons fonts must be installed!)

(use-package powerline
  :straight (:host github :repo "milkypostman/powerline"))

(use-package spaceline-config
  :straight (spaceline :host github :repo "TheBB/spaceline")
  :after powerline
  :config
  ;; `spaceline-all-the-icons' adds a high startup time
  ;; and is laggy in the presence of `diff-hl'
  (if (minimize-startup-p)
      (spaceline-spacemacs-theme)
    (use-package spaceline-all-the-icons
      :straight (:host github :repo "domtronn/spaceline-all-the-icons.el")
      :config
      (spaceline-all-the-icons-theme))))


(use-package doom-themes
  :straight (:host github :repo "hlissner/emacs-doom-themes"
                   :files (:defaults "themes/*.el"))
  :init
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	    doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))


;;-----------------------------------------------------------------------------
;; Completions
;;-----------------------------------------------------------------------------

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

;; `company' provides general text completion
(use-package company
  :straight (:host github :repo "company-mode/company-mode")
  :diminish
  :init (global-company-mode)
  :bind
  ("C-j" . company-complete)
  (:map company-active-map
        ("<escape>" . company-abort)
        ("C-w" . company-select-previous-or-abort)
        ("C-s" . company-select-next-or-abort)
        ("M-f" . company-search-candidates)
        ("C-M-f" . company-show-location)
        ("C-j" . company-complete-common)))

;; `which-key' provides keybinding completion help
(use-package which-key
  :straight (:host github :repo "justbur/emacs-which-key")
  :diminish
  :init (which-key-mode))

(use-package flycheck
  :straight (:host github :repo "flycheck/flycheck")
  :diminish
  :init (global-flycheck-mode))


;;-----------------------------------------------------------------------------
;; Version Control
;;-----------------------------------------------------------------------------

(use-package vc-hooks
  :config
  (setq vc-follow-symlinks t))  ; follow symlinks to their targets

(use-package magit
  :straight (:host github :repo "magit/magit"
                   :files ("lisp/magit"
                           "lisp/magit*.el"
                           "lisp/git-rebase.el"
                           "Documentation/magit.texi"
                           "Documentation/AUTHORS.md"
                           "LICENSE"))
  :commands magit-status
  
  :bind
  (("C-g" . magit-status)
   :map magit-mode-map
   ("C-w" . nil)
   ("C-f" . magit-section-forward)
   ("C-r" . magit-section-backward)
   ([remap kill-region] . magit-copy-section-value)
   ([remap keyboard-escape-quit] . magit-mode-bury-buffer))
  :custom
  (magit-log-arguments '("-n256"
                         "--graph"
                         "--color"
                         "--decorate")))

(use-package magit-gh-pulls
  :disabled
  :straight (:host github :repo "sigma/magit-gh-pulls")
  :hook (magit-mode . turn-on-magit-gh-pulls))

(use-package magithub
  :disabled
  :straight (:host github :repo "vermiculus/magithub")
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/Projects"))

(use-package vc-git
  :disabled
  :after magit
  :config
  (when (vc-git-responsible-p default-directory)
    (magit-status default-directory)))

(use-package diff-hl
  :straight (:host github :repo "dgutov/diff-hl")
  :if (minimize-startup-p)
  :init
  (global-diff-hl-mode)
  :hook
  (flycheck-mode      . diff-hl-flydiff-mode)
  (dired-mode         . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package git-gutter
  :straight (:host github :repo "syohex/emacs-git-gutter")
  :unless (minimize-startup-p)
  :diminish
  :init
  (global-git-gutter-mode))

(use-package git-gutter-fringe
  :straight (:host github :repo "syohex/emacs-git-gutter-fringe")
  :after git-gutter)


;;-----------------------------------------------------------------------------
;; Python
;;-----------------------------------------------------------------------------
(eval-after-load 'exec-path-from-shell  ; for `:if' to take effect
  (use-package python
    :if (executable-find "python")
    :mode (("\\.py\\'" . python-mode)
           ("\\.pyi\\'" . python-mode))
    :interpreter ("python[0-9.]*" . python-mode)

    :init
    (use-package anaconda-mode
      :straight (:host github :repo "proofit404/anaconda-mode")
      :diminish
      :hook python-mode
      :bind (:map python-mode-map
                  ("C-c M-r" . anaconda-mode-find-references))
      :config
      (unbind-key "M-r" anaconda-mode-map))

    (use-package company-jedi
      :straight (:host github :repo "syohex/emacs-company-jedi")
      :after company
      :bind (:map python-mode-map
                  ("C-c C-?" . jedi:show-doc))
      :hook
      (python-mode . (lambda ()
                       (add-to-list 'company-backends 'company-jedi))))

    (use-package pyvenv
      :straight (:host github :repo "jorgenschaefer/pyvenv")
      :hook (python-mode . pyvenv-mode)
      :bind (:map python-mode-map
                  ("C-c a" . conda-activate)
                  ("C-c d" . conda-deactivate))
      :config
      ;; TODO: make our own wrappers instead of using `WORKON_HOME'
      (setenv "WORKON_HOME" "~/.conda/envs")
      (defalias 'conda-activate 'pyvenv-workon)
      (defalias 'conda-deactivate 'pyvenv-deactivate)
      ;; properly make `jedi' use the right venv
      (add-hook 'pyvenv-post-activate-hooks '(lambda ()
                                               (with-suppressed-message
                                                 (jedi:stop-server)))))
    
    (use-package python-pytest
      :straight (:host github :repo "wbolster/emacs-python-pytest")
      :demand
      :custom
      (python-pytest-arguments
       '("--color"
         "--doctest-modules"
         "--showlocals"
         "--verbose"
         "--failed-first"))
      :bind (:map python-mode-map
                  ("C-C p" . python-pytest-popup)))

    (use-package flycheck-pycheckers
      :straight (:host github :repo "msherry/flycheck-pycheckers"
                       :files (:defaults ("bin" "bin/pycheckers.py")))
      :after (flycheck pyvenv)
      :defines flycheck-pycheckers-checkers
      :custom
      (flycheck-pycheckers-checkers '(flake8 mypy3))
      (flycheck-pycheckers-venv-root (getenv "WORKON_HOME"))
      :hook (flycheck-mode . flycheck-pycheckers-setup)
      :config
      (add-to-list 'flycheck-pycheckers-ignore-codes "W503"))

    :bind (:map python-mode-map
                ("C-c C-M-f" . python-replace-doc))

    :config
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt -i")
    
    ;; TODO: perform replace multiple times in same docstring
    (defun python-replace-doc (regexp to-string &optional delimited start end backward)
      (declare (interactive-only
                "use `re-search-forward' and `replace-match' instead."))
      (interactive
       (let ((common
              (query-replace-read-args
               (concat "Replace"
                       (if current-prefix-arg
                           (if (eq current-prefix-arg '-) " backward" " word")
                         "")
                       " regexp for docstrings"
                       (if (use-region-p) " in region" ""))
               t)))
         (list (nth 0 common) (nth 1 common) (nth 2 common)
               (if (use-region-p) (region-beginning))
               (if (use-region-p) (region-end))
               (nth 3 common))))
      (perform-replace (concat "\\(?8:\"\"\"[[:ascii:]]*\\)"
                               regexp
                               "\\(?9:[[:ascii:]]*\"\"\"\\)")
                       (concat "\\8"
                               to-string
                               "\\9")
                       nil t delimited nil nil start end backward))))

(use-package cython-mode
  :straight (:host github :repo "cython/cython"
                   :files ("Tools/*.el"))
  :mode (("\\.pyx\\'" . cython-mode)
         ("\\.pxd\\'" . cython-mode)))


;;-----------------------------------------------------------------------------
;; Webdev
;;-----------------------------------------------------------------------------

(use-package sgml-mode
  :init
  (use-package scss-mode
    :straight (:host github :repo "antonj/scss-mode")
    :custom
    (sass-scss-command "/usr/local/bin/scss")
    :config
    (setq exec-path (cons (expand-file-name "/usr/local/bin") exec-path)))
  
  :magic-fallback ("<!DOCTYPE html>" . html-mode))

(use-package yaml-mode
  :straight (:host github :repo "yoshiki/yaml-mode")
  :mode "\\.yml\\'")

(use-package markdown-mode
  :straight (:host github :repo "jrblevin/markdown-mode")
  :mode "\\.md\\'"
  :config
  (setq markdown-command (executable-find "pandoc")))

(use-package impatient-mode
  :straight (:host github :repo "skeeto/impatient-mode"
                   :files ("*.html" "*.js" "impatient-mode.el"))
  :defer t
  :init
  ;; https://stackoverflow.com/a/36189456
  ;; https://stackoverflow.com/a/51860126
  (defun markdown-html (buffer)
    (princ (with-current-buffer buffer
             (format "<!DOCTYPE html><html><script src=\"https://cdnjs.cloudflare.com/ajax/libs/he/1.1.1/he.js\"></script><link rel=\"stylesheet\" href=\"https://assets-cdn.github.com/assets/github-e6bb18b320358b77abe040d2eb46b547.css\"><link rel=\"stylesheet\" href=\"https://assets-cdn.github.com/assets/frameworks-95aff0b550d3fe338b645a4deebdcb1b.css\"><title>Impatient Markdown</title><div id=\"markdown-content\" style=\"display:none\">%s</div><div class=\"markdown-body\" style=\"max-width:968px;margin:0 auto;\"></div><script>fetch('https://api.github.com/markdown', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ \"text\": document.getElementById('markdown-content').innerHTML, \"mode\": \"gfm\", \"context\": \"knit-pk/homepage-nuxtjs\"}) }).then(response => response.text()).then(response => {document.querySelector('.markdown-body').innerHTML = he.decode(response)}).then(() => { fetch(\"https://gist.githubusercontent.com/FieryCod/b6938b29531b6ec72de25c76fa978b2c/raw/\").then(response => response.text()).then(eval)});</script></html>"
                     (buffer-substring-no-properties (point-min) (point-max))))
           (current-buffer)))
  (defun markdown-preview-github ()
    (interactive)
    (impatient-mode 1)
    (setq imp-user-filter #'markdown-html)
    (cl-incf imp-last-state)
    (imp--notify-clients)))
