(setq backup-directory-alist '(("." . "~/.emacs-backups")))

(set-register ?A '(file . "~/org/agenda.org"))
(set-register ?E '(file . "~/.emacs.d/init.el"))
(set-register ?I '(file . "~/org/inbox.org"))
(set-register ?K '(file . "~/.emacs.d/elisp/my-keybinds.el"))
(set-register ?P '(file . "~/.emacs.d/elisp/my-packages.el"))
(set-register ?S '(file . "~/.emacs.d/elisp/my-system.el"))
(set-register ?T '(file . "~/org/tracker.org"))
(set-register ?U '(file . "~/.emacs.d/elisp/my-ui.el"))




;; Coding stuff ===============================================================
(setq-default indent-tabs-mode nil
              tab-width 4)

;; c ==================================
(setq-default c-basic-offset 4)
(setq c-default-style "k&r")
(setq gdb-many-windows t
      gdb-show-main t)


;; python =============================
(setq python-indent-guess-indent-offset-verbose nil)
(setq python-shell-completion-native nil)


;; Make C-x C-b group things in the specified way
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("magit" (or (mode . magit-status-mode)
                            (mode . magit-diff-mode)
                            (mode . magit-process-mode)))
               ("starred" (starred-name))
               ("dired" (mode . dired-mode))
               ("python" (mode . python-mode))
               ("org" (mode . org-mode))
               ("go" (mode . go-mode))))))

(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default")))


;; tramp
(require 'tramp)
(tramp-set-completion-function "ssh"
                               '((tramp-parse-sconfig "~/.ssh/config")))
(setq password-cache-expiry nil)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; info
(add-to-list 'Info-directory-list "~/info")

(provide 'my-system)
