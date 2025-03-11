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
(setq tramp-default-method "ssh")


;; Spaces match themselves, not one or more of themselves
(setq search-whitespace-regexp nil)

(setq dired-isearch-filenames 'dwim     ; search filenames if in filename column
      dired-kill-when-opening-new-dired-buffer t ; keep only one dired buffer
      dired-create-destination-dirs t   ; create missing dirs on RHS of rename
      )

(setq isearch-repeat-on-direction-change t)
;;default search commands
;; in C-s
;; C-M-y: add char
;; C-M-d  delete char
;; C-M-z  add to zap

(setq reb-re-syntax 'string)

;; Passwords go in ~/.pgpass
(setq sql-connection-alist
      '((local (sql-product 'postgres)
               (sql-user "dev")
               (sql-server "localhost")
               (sql-port 35432)
               (sql-database "development"))
        (alpha (sql-product 'postgres)
               (sql-user "dev")
               (sql-server "db.alpha.i.outcomes4me-staging.com")
               (sql-port 5432)
               (sql-database "development"))
        (staging (sql-product 'postgres)
                 (sql-user "dev")
                 (sql-server "db.staging.i.outcomes4me-staging.com")
                 (sql-port 5432)
                 (sql-database "development"))
        (prod-ro (sql-product 'postgres)
                 (sql-user "postgres_ro")
                 (sql-server "db.production.i.outcomes4me.com")
                 (sql-port 5432)
                 (sql-database "production"))))

;; From https://karthinks.com/software/emacs-window-management-almanac/
;; (advice-add 'other-window :before
;;             (defun other-window-split-if-single (&rest _)
;;               "Split the frame if there is a single window."
;;               (when (one-window-p) (split-window-sensibly))))

;; Use most recent window for other-window-scroll
(setq other-window-scroll-default (lambda ()
                                    (or (get-mru-window nil nil 'not-this-one-dummy)
                                        (next-window)
                                        (next-window nil nil 'visible))))

(add-hook 'prog-mode-hook 'abbrev-mode)
;; Avoid flymake yelling at me (https://github.com/joaotavora/eglot/issues/843)
(add-hook 'clone-indirect-buffer-hook 'read-only-mode)
(setq save-interprogram-paste-before-kill t)

(setq browse-url-browser-function 'browse-url-default-browser)
(setq browse-url-secondary-browser-function 'eww)
;; (setq browse-url-browser-function 'w3m)
;; (setq browse-url-secondary-browser-function 'browse-url-default-browser)

(setq echo-keystrokes 0.01)

;; from https://emacsredux.com/blog/2025/02/03/clean-unloading-of-emacs-themes/
(defun julian/disable-all-active-themes ()
  "Disable all currently active themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(if julian/at-work
    (setq shell-file-name "/opt/homebrew/bin/fish"))

(provide 'my-system)
