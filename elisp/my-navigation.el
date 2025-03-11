(use-package ace-window
  :after hyperbole
  :demand t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-dispatch-always nil)
  (set-face-foreground 'aw-mode-line-face "#f0f")
  (ace-window-display-mode t)
  :bind
  (("M-o" . ace-window)
   ("C-x o" . other-window)
   :map hyperbole-mode-map
   ("M-o" . ace-window)))

(use-package avy
  :config (defvar julian/avy-keymap
            (define-keymap
              "c"  #'avy-goto-char
              "'"  #'avy-goto-char
              "\""  #'avy-goto-char-2
              "t" #'avy-goto-char-timer
              "w" #'avy-goto-word-0
              "l"  #'avy-goto-line))
  (setq avy-timeout-seconds 0.3
        avy-style 'at-full
        avy-background t
        avy-single-candidate-jump nil
        )
  :bind-keymap ("C-;" . julian/avy-keymap)
  :bind ((:map isearch-mode-map ("C-'" . avy-isearch)))
  :bind (("C-'" . avy-goto-char-timer)))

(use-package dumber-jump
  :config
  (add-hook 'xref-backend-functions #'dumber-jump-xref-activate))

(use-package smerge-mode
  :ensure nil
  :bind (:map smerge-mode-map
              ("C-c C-s n" . smerge-next)
              ("C-c C-s p" . smerge-prev)
              ("C-c C-s r" . smerge-refine)
              ("C-c C-s a" . smerge-keep-all)
              ("C-c C-s l" . smerge-keep-lower)
              ("C-c C-s u" . smerge-keep-upper)))

(provide 'my-navigation)
