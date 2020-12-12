(setq backup-directory-alist '(("." . "~/.emacs-backups")))

(desktop-save-mode 1)

(set-register ?e '(file . "~/.emacs.d/init.el"))
(set-register ?p '(file . "~/.emacs.d/elisp/my-packages.el"))
(set-register ?i '(file . "~/Dropbox/org/inbox.org"))
(set-register ?t '(file . "~/Dropbox/org/tracker.org"))

(global-subword-mode t)


;; Coding stuff ===============================================================
(setq-default indent-tabs-mode nil
              tab-width 4)



(setq require-final-newline t)

;; c ==================================
(setq-default c-basic-offset 4)
(setq c-default-style "k&r")
(setq gdb-many-windows t
      gdb-show-main t)


;; python =============================
(setq python-indent-guess-indent-offset-verbose nil)



(setq python-shell-completion-native nil)

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
