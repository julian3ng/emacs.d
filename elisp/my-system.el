(setq backup-directory-alist '(("." . "~/.emacs-backups")))

(set-register ?E '(file . "~/.emacs.d/init.el"))
(set-register ?G '(file . "~/org/gtd.org"))
(set-register ?I '(file . "~/org/inbox.org"))

(set-register ?K '(file . "~/.emacs.d/elisp/my-keybinds.el"))
(set-register ?P '(file . "~/.emacs.d/elisp/my-packages.el"))
(set-register ?S '(file . "~/.emacs.d/elisp/my-system.el"))
(set-register ?T '(file . "~/org/tracker.org"))
(set-register ?U '(file . "~/.emacs.d/elisp/my-ui.el"))
(set-register ?Y '(file . "~/.emacs.d/snippets"))

;; Coding stuff ===============================================================
(setq-default indent-tabs-mode nil
              tab-width 4)

;; c ==================================
(setq c-default-style "linux")
(setq gdb-many-windows t
      gdb-show-main t)
(setq c-basic-offset 4)


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

;; js ================================
(setq js-indent-level 2)


;; tramp
(require 'tramp)
(tramp-set-completion-function "ssh"
                               '((tramp-parse-sconfig "~/.ssh/config")))
(setq password-cache-expiry nil)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; info
                                        ;(add-to-list 'info-directory-list "~/info")

;; Get rid of overwrite mode
(put 'overwrite-mode 'disabled t)

;; M-x inside M-x, do shell stuff inside C-x C-f, etc.
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode t)
(setq-default dabbrev-case-fold-search nil )
(setq tramp-default-method "ssh")


;; Spaces match themselves, not one or more of themselves
(setq search-whitespace-regexp nil)

;; Highlight changes between saves
(defun julian/flicker-highlight-changes-mode ()
  (interactive)
  (if highlight-changes-mode
      (progn  (highlight-changes-mode 0) (highlight-changes-mode 1))
    (progn  (highlight-changes-mode 1) (highlight-changes-mode 0))))

;; (add-hook 'after-save-hook 'julian/flicker-highlight-changes-mode)

(setq isearch-repeat-on-direction-change t
      isearch-wrap-pause 'no)
;;default search commands
;; in C-s
;; C-M-y: add char
;; C-M-d  delete char
;; C-M-z  add to zap

(setq reb-re-syntax 'string)

(setq sql-postgres-login-params (append sql-postgres-login-params '(port)))
(setq sql-port 35432)
(provide 'my-system)
