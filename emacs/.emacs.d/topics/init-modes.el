(use-package python
  :if (executable-find "python")
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python[0-9.]*" . python-mode)

  :commands python-replace-doc
  :bind (:map python-mode-map
              ("C-c C-M-F" . python-replace-doc))
  
  :config
  ;; TODO perform replace multiple times in same docstring
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
                     nil t delimited nil nil start end backward)))


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

(use-package markdown-mode
  :straight (:host github :repo "jrblevin/markdown-mode")
  :mode "\\.md\\'")


(provide 'init-modes)
