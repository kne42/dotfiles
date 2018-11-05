(use-package kne42-settings
  :init
  (defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
    "`<escape>' only needs to be hit once instead of three times."
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
         (setq beg (line-beginning-position) end (line-end-position)))))

  (setq
   load-prefer-newer t                      ; prefer newer .el over .elc
   vc-follow-symlinks t                     ; follow symlinks to their targets
   delete-selection-mode t                  ; modifying text replaces the region
   kill-ring-max 5000                       ; truncate kill ring at 5000 entries
   mark-ring-max 5000                       ; truncate mark ring at 5000 entries
   visible-bell t                           ; turn off audio bell
   global-hl-line-mode t                    ; highlight cursor line
   blink-cursor-mode -1                     ; no blinking cursor
   track-eol nil                            ; cursor doesn't track end-of-line
   mouse-yank-at-point t                    ; paste at cursor position
   sentence-end-double-space nil            ; sentences end with one space
   truncate-partial-width-windows nil       ; don't truncate long lines
   require-final-newline t                  ; add newline at the end of every file
   column-number-mode t                     ; show column number in the mode-line
   show-paren-mode t                        ; highlight parenthesis pairs
   blink-matching-paren-distance nil        ; no blinking parenthesis
   read-file-name-completion-ignore-case t  ; ignore case when completing file names
   read-buffer-completion-ignore-case t)    ; ignore case when completing minibuffer

  (setq-default
   indicate-empty-lines t                   ; show empty lines
   indent-tabs-mode nil                     ; use spaces instead of tabs
   tab-width 4)                             ; tab length

  (defalias 'yes-or-no-p 'y-or-n-p)         ; y/n instead of yes/no

  (defvar delete-trailing-on-save t)        ; delete trailing whitespaces on save

  (provide 'kne42-settings)

  :custom
  (icomplete-mode t)                         ; minibuffer completion

  :hook
  (before-save-hook . (lambda () (when delete-trailing-on-save
                                   (delete-trailing-whitespace))))

  :config
  ;; app
  (if (display-graphic-p)
      (setq tool-bar-mode -1                  ; no toolbar
            menu-bar-mode -1                  ; no menubar
            set-scroll-bar-mode 'right        ; set scrollbar right
            scroll-bar-mode -1                ; disable scrollbar
            scroll-preserve-screen-position t ; scroll without moving cursor
            mouse-wheel

            
(provide 'init-settings)
