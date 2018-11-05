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
(straight-use-package
 '(use-package :host github :repo "jwiegley/use-package"))


;; configuration topic files
(defconst topics-dir
  (concat user-emacs-directory (file-name-as-directory "topics"))
  "Topic file loadpath.")

;; add it to our loading search path
(add-to-list 'load-path topics-dir)


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


;; `diminish' provides diminishing of minor modes by removing their modeline
;; display
;; `use-package' integration: `:diminish'
(use-package diminish
  :straight (:host github :repo "myrjola/diminish.el"))


;; `el-patch' provides on-the-fly patching of functions from other packages
;; via sexp-based diffs
;; `use-package' integration: `:init/el-patch', `:config/el-patch'
(use-package el-patch
  :straight (:host github :repo "raxod502/el-patch" :branch "develop"))


;;-----------------------------------------------------------------------------
;; Topic Loading
;;-----------------------------------------------------------------------------

;; `settings' provides global settings and advice for built-in libraries
(use-package init-settings)

;; `keybindings' provides global keybindings
(use-package init-keybindings)

;; `display' provides themes, mode-lines, and other visual elements
(use-package init-display)
