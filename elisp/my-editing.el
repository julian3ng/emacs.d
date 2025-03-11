(use-package multiple-cursors
  :config (setq julian/mc-keymap
                (define-keymap
                  "e" #'mc/edit-lines
                  "E" #'mc/edit-ends-of-lines
                  "n" #'mc/mark-next-like-this
                  "p" #'mc/mark-previous-like-this
                  "a" #'mc/mark-all-like-this-dwim
                  ))
  :bind-keymap (("s-m" . julian/mc-keymap)))

(use-package paredit
  :diminish paredit-mode
  :hook ((lisp-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode))
  :bind (:map paredit-mode-map ("M-d" . paredit-forward-kill-word))
  :config (progn (add-hook 'lisp-mode-hook #'paredit-mode)
                 (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
                 (add-hook 'common-lisp-mode-hook #'paredit-mode)
                 (add-hook 'scheme-mode-hook #'paredit-mode)
                 (add-hook 'racket-mode-hook #'paredit-mode)))

(defun paredit-space-for-delimiter-p (endp delimiter)
  "Paredit spacing tweaks"
  (and (not (if endp (eobp) (bobp)))
       (memq (char-syntax (if endp (char-after) (char-before)))
             (list ?\" ;; REMOVED ?w ?_
                   (let ((matching (matching-paren delimiter)))
                      (and matching (char-syntax matching)))))))

(use-package vundo
  :bind (("s-u" . vundo))
  :config (setq vundo-glyph-alist vundo-unicode-symbols))


(use-package iedit
  :bind ("C-c i" . iedit-mode))

(use-package embrace
  :bind
  (("C-c C-," . embrace-commander)
   (:map org-mode-map
         ("C-c C-," . embrace-commander))))

(use-package wgrep)

(provide 'my-editing)
