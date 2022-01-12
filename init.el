(setq byte-compile-warnings '(cl-functions)) ;; TEMPORARY: turn off cl package deprecation warning

;; Increase garbage collection threshold to speed up initialization
(setq gc-cons-threshold (* 100 1000 1000))
(add-hook 'emacs-startup-hook
      (lambda ()
        (message "Emacs ready in %s with %d garbage collections."
             (format "%.2f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time)))
             gcs-done)))

;; whoami
(setq user-full-name "Julian Eng"
      user-mail-address "julian3ng@gmail.com")

;; Setup packages
(require 'package)
(setq package-enable-at-startup nil)

;; (setq package-archives
;;       '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
;;         ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
;;         ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
             ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t
      use-package-always-ensure t)
(setq load-prefer-newer t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8f567db503a0d27202804f2ee51b4cd409eab5c4374f57640317b8fcbbd3e466" default))
 '(package-selected-packages
   '(j-mode dyalog-mode dictionary org-roam rustic molokai-theme protobuf-mode edit-indirect vagrant vagrant-tramp writeroom-mode wc-mode writegood-mode org-bullets rubocopfmt rvm exec-path-from-shell seeing-is-believing geiser selectrum julia-mode julia-repl nyan-mode ruby-electric-mode inf-ruby-mode inf-ruby rspec-mode company-lua love-minor-mode lua-mode web-mode which-key restclient color-theme-modern :gnu-apl-mode cider clojure-mode sonic-pi elfeed org-journal rainbow-mode common-lisp-snippets expand-region symbol-overlay delight erc-image erc-hl-nicks flycheck-ledger ledger-mode erlang yaml-mode glsl-mode company-lsp lsp-ui wpuzzle fzf rg ack map spinner lsp-mode evil hideshowvis all-the-icons-ivy counsel-projectile latex-preview-pane elpy ace-window escreen auctex emmet-mode racket-mode slime-company use-package undo-tree smex slime rainbow-delimiters paredit magit flycheck diminish counsel color-theme-molokai beacon))
 '(safe-local-variable-values
   '((eval setq-local org-babel-default-header-args:Python
           '((:session . "*ctci*"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-datatype-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-function-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-postulate-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-primitive-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-record-face ((t (:foreground "deep sky blue")))))
(put 'upcase-region 'disabled nil)


(add-to-list 'load-path "~/.emacs.d/elisp/")
(require 'my-system)
(require 'my-packages)
(require 'my-ui)
(require 'my-keybinds)

;; Reset garbage collection threshold
(setq gc-cons-threshold (* 2 1000 1000))
(put 'overwrite-mode 'disabled nil)
(put 'narrow-to-region 'disabled nil)
