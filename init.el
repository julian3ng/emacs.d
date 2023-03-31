(setq byte-compile-warnings '(cl-functions)) ;; TEMPORARY: turn off cl package deprecation warning

(if init-file-debug
      (setq use-package-verbose t
            use-package-expand-minimally nil
            use-package-compute-statistics t
            debug-on-error t)
    (setq use-package-verbose nil
          use-package-expand-minimally t))

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


(put 'upcase-region 'disabled nil)

(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path "~/.emacs.d/elisp/bqn-mode/")
(load "gforth.el")
(require 'forth-mode)
(require 'bqn-mode)
(require 'my-system)
(require 'my-ui)
(require 'my-keybinds)
(require 'my-packages)
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/bqn-mode/"))
(require 'my-meow)
                                        ;(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/bqn-mode/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("139a30fabc387d2c0f16c93abb710f6b412e7f4b870b3caa9568059e8f036f3b" "1ca4166b51f0ce0e709bf166c2cfdd7629093b6a4eb8eb72258b341bd25d4e42" "f66e490ecb62031f055e7e32146693f48b20014906866d2ecdcd05b5225e4666" "7476a68e1c74ad293883bb202ab0e6a6a787591d347cdd86e3bc5be2a4bf9def" "64edbf199b53bd43911962330f125064e9f440586a0a3ac04b8defa7372cf5ec" "212e47f659f337463876e4586607d0378411cda421bb763ac020e50926eda3c7" "a157481364cd3e6c5c6bc4d58cb085b45035463eb3fd665aba1d340e9ddc202c" "7eeedf1448b2f4a50fe58fc3bb9abf68e248456fd4ae9d3a9a71568480cbb214" "10551f0a24d0ac97a109f02178e9e34b448ee12a52357911cf3362a6b249cae6" "ff8be9ed2696bf7bc999423d909a603cb23a9525bb43135c0d256b0b9377c958" "294c4b6a955149c93945f901a284140df29963a57939e9ed2fc4ad79b3605080" "f4af94508e325e24ddf85639b2ef04062bed15436cdc5f7937fe6a962033b597" "be72a4b22806340cf28f046d361a6a7ac76d6fa53d032f65cb98788ea9d0e08d" "b494aae329f000b68aa16737ca1de482e239d44da9486e8d45800fd6fd636780" "8f567db503a0d27202804f2ee51b4cd409eab5c4374f57640317b8fcbbd3e466" default))
 '(org-agenda-files
   '("/Users/julian/org/agenda.org" "/Users/julian/org/inbox.org"))
 '(package-selected-packages
   '(eyebrowse shortdoc company-restclient company-web company-emoji cape all-the-icons-completion corfu eglot embark-consult embark consult marginalia orderless vertico ligature visible-mark meow treemacs treemacs-all-the-icons treemacs-magit treemacs-projectile yasnippet-snippets elixir-mode php-mode typescript-mode helpful gnu-apl-mode py-autopep8 autopep8 jedi j-mode dyalog-mode dictionary org-roam rustic molokai-theme protobuf-mode edit-indirect vagrant vagrant-tramp writeroom-mode wc-mode writegood-mode org-bullets rubocopfmt exec-path-from-shell seeing-is-believing geiser selectrum julia-mode julia-repl nyan-mode ruby-electric-mode inf-ruby-mode inf-ruby rspec-mode company-lua love-minor-mode lua-mode web-mode which-key restclient color-theme-modern :gnu-apl-mode cider clojure-mode sonic-pi elfeed org-journal rainbow-mode common-lisp-snippets expand-region symbol-overlay delight erc-image erc-hl-nicks flycheck-ledger ledger-mode erlang yaml-mode glsl-mode company-lsp lsp-ui wpuzzle fzf rg ack map spinner lsp-mode evil hideshowvis all-the-icons-ivy counsel-projectile latex-preview-pane elpy ace-window escreen auctex emmet-mode racket-mode slime-company use-package undo-tree smex slime rainbow-delimiters paredit magit flycheck diminish counsel color-theme-molokai beacon))
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

;; Reset garbage collection threshold
(setq gc-cons-threshold (* 2 1000 1000))
(put 'overwrite-mode 'disabled nil)
(put 'narrow-to-region 'disabled nil)
