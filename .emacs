; ---------------- Packages Archives ----------------
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;(if (require 'quelpa nil t)
;    (quelpa-self-upgrade)
;  (with-temp-buffer
;    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
;    (eval-buffer)))

;(quelpa
; '(quelpa-use-package
;   :fetcher github
;   :repo "quelpa/quelpa-use-package"))
;(require 'quelpa-use-package)

(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (make-directory "~/.emacs.d" :parents)
  (with-temp-buffer (write-file custom-file)))

(load custom-file)

; -------------- Package Initialization --------------

(use-package impatient-mode
  :config (defun preview-markdown ()
            (interactive)
            (httpd-start)
            (impatient-mode)
            (imp-set-user-filter (lambda (buffer)
                                   (princ (with-current-buffer buffer
                                            (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
                                          (current-buffer))))))
(use-package expand-region
  :bind ("C-M-r" . er/expand-region))

(use-package all-the-icons)
(use-package neotree
  :bind ("C-' C-l" . neotree-toggle)
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t))
(use-package spaceline)
(use-package spaceline-all-the-icons
  :after spaceline
  :config (spaceline-all-the-icons-theme))

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
		doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme
  (doom-themes-neotree-config))  ; all-the-icons fonts must be installed!

(use-package magit
  :bind ("C-u" . magit-status))

(use-package company
  :init (global-company-mode))

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  :hook
  (yaml-mode-hook . (lambda ()
        (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; ---------------------------

;; Misc
(defalias 'yes-or-no-p 'y-or-n-p)              ; y/n instead of yes/no
(setq-default indent-tabs-mode nil)
(pending-delete-mode t)

(add-to-list 'magic-mode-alist '("<!DOCTYPE html>" . (lambda() (if (not (boundp 'html-mode)) (html-mode)))))

(use-package which-key)

(use-package scss-mode
  :custom
  (sass-scss-command "/usr/local/bin/scss")
  :config
  (setq exec-path (cons (expand-file-name "/usr/local/bin") exec-path))
  (global-auto-revert-mode t))

(use-package smooth-scrolling
  :init (setq smooth-scroll-margin 20)
  :config (smooth-scrolling-mode 1))

(use-package expand-region
  :bind
  ("C-M-r" . er/expand-region))

;; Helper for compilation. Close the compilation window if
;; there was no error at all. (emacs wiki)
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (kill-buffer (get-buffer "*compilation*")))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))
;; Specify my function (maybe I should have done a lambda function)
(setq compilation-exit-message-function 'compilation-exit-autoclose)

(keyboard-translate ?\C-i ?\H-i)

(require 'bind-key)
(bind-keys*
 ;; Movement
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

 ;; Deletion
 ("H-i" . delete-indentation)
 ("C-M-s" . kill-region)
 ("M-s" . kill-line)
 ("C-M-x" . append-next-kill)

 ("C-M-a" . delete-backward-char)
 ("C-M-d" . delete-forward-char)

 ("C-M-q" . backward-kill-word)
 ("C-M-e" . kill-word)

 ("C-M-w" . yank)

 ("M-w" . undo)

 ("M-r" . set-mark-command)
 ("C-M-f" . exchange-point-and-mark)

 ("M-f" . search-forward)

 ("C-' C-'" .  buffer-menu)       ; Select a buffer from the list.
 ("C-' C-;" .  find-file)         ;

 ("C-; ;" . (lambda () (interactive)
              (select-window (split-window-right))
              (buffer-menu)))
 ("C-; '" . (lambda () (interactive)
              (select-window (split-window-below))
              (buffer-menu)))
 ("C-l ;" . delete-other-windows)
 ("C-l l" . delete-window)
 ("C-l o" . transpose-windows)
 ("C-o" . other-window)
 ("<escape>" . keyboard-escape-quit))

(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(global-unset-key (kbd "C-l"))
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

;; Appearance
(setq visible-bell t)          ; Turnoff audiobell
(global-hl-line-mode t)        ; Highlight cursor line

(set-frame-parameter (selected-frame) 'alpha '(98 . 85))
(add-to-list 'default-frame-alist '(alpha . (98 . 85)))

;; Cursor/mouse
(column-number-mode t)         ; Show column number in mode-line
(blink-cursor-mode 0)          ; No blinking cursor
(setq track-eol nil)           ; Cursor doesn't track end-of-line (?)
(setq mouse-yank-at-point t)   ; Paste at cursor position


;; Tabs, spaces, lines and parenthesis
(setq tab-width 4)                             ; Length of tab is 4 SPC
(setq sentence-end-double-space nil)           ; Sentences end with one space
(setq truncate-partial-width-windows nil)      ; Don't truncate long lines
(setq-default indicate-empty-lines t)          ; Show empty lines
(setq next-line-add-newlines t)                ; Add newline when at buffer end
(setq require-final-newline 't)                ; Always newline at end of file
(global-linum-mode 1)                          ; Show line numbers on buffers
(show-paren-mode 1)                            ; Highlight parenthesis pairs
(setq blink-matching-paren-distance nil)       ; Blinking parenthesis

;; Files and sessions
(setq read-file-name-completion-ignore-case 't) ; Ignore case when completing file names
(defvar delete-trailing-on-save t)              ; Delete trailing whitespaces on save.
(add-hook 'before-save-hook
          (lambda ()
            (when delete-trailing-on-save
              (delete-trailing-whitespace))))


;; Buffers and windows
(icomplete-mode t)
(setq read-buffer-completion-ignore-case 't)

;; App-only settings
(if (window-system)
    (progn (tool-bar-mode -1)             ; No toolbar
           (menu-bar-mode -1)             ; No menubar
           (set-scroll-bar-mode 'right)   ; Set scrollbar right
           (scroll-bar-mode -1)           ; Disable scrollbar
           (setq scroll-preserve-screen-position t) ; Scroll without moving cursor
           (mouse-wheel-mode nil)))         ; Mouse-wheel disabled


;; (global-set-key (kbd "C-v") 'move-to-window-line-top-bottom)

;; (use-package ace-window
;;   :bind ("C-o" .  ace-window))

(defun transpose-windows ()
   "Transpose two windows.  If more or less than two windows are visible, error."
   (interactive)
   (unless (= 2 (count-windows))
     (error "There are not 2 windows."))
   (let* ((windows (window-list))
          (w1 (car windows))
          (w2 (nth 1 windows))
          (w1b (window-buffer w1))
          (w2b (window-buffer w2)))
     (set-window-buffer w1 w2b)
     (set-window-buffer w2 w1b)))

;; Text manipulation
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-v") 'append-next-kill)
