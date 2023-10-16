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
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

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


(require 'my-system)
(require 'my-ui)
(require 'my-keybinds)
(require 'my-packages)
(require 'my-meow)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("01ca8e215adc3a3221b42db10218dd181244e2971071207a384daeb9cbf31e58" "adf00c7e12b00a4ec402d6c695252a425776ca2a93de78805da52a1de67b0e1b" "277a5bce12d6957dbabb43a2f55ee2b6371388b749cbb29fd251df19334a1f0b" "7887cf8b470098657395502e16809523b629249060d61607c2225d2ef2ad59f5" "cca1d386d4a3f645c2f8c49266e3eb9ee14cf69939141e3deb9dfd50ccaada79" default))
 '(package-selected-packages
   '(modus-themes sicp highlight prism terraform-mode dirvish devil olivetti magit pug-mode poke-line eat autothemer chess projectile-ripgrep ripgrep apheleia emacs-rotate pcre2el minions bm hs-minor-mode smerge iedit eyebrowse shortdoc company-restclient company-web company-emoji cape all-the-icons-completion corfu embark-consult embark consult marginalia orderless vertico ligature visible-mark meow treemacs treemacs-all-the-icons treemacs-magit treemacs-projectile yasnippet-snippets elixir-mode php-mode helpful gnu-apl-mode py-autopep8 autopep8 j-mode dyalog-mode dictionary org-roam rustic molokai-theme protobuf-mode edit-indirect vagrant vagrant-tramp writeroom-mode wc-mode writegood-mode org-bullets rubocopfmt exec-path-from-shell seeing-is-believing geiser selectrum julia-mode julia-repl nyan-mode ruby-electric-mode inf-ruby-mode inf-ruby rspec-mode company-lua lua-mode web-mode which-key restclient color-theme-modern :gnu-apl-mode cider clojure-mode sonic-pi elfeed org-journal rainbow-mode common-lisp-snippets expand-region symbol-overlay delight erc-image erc-hl-nicks flycheck-ledger ledger-mode erlang yaml-mode glsl-mode company-lsp lsp-ui wpuzzle fzf rg ack map spinner lsp-mode evil hideshowvis all-the-icons-ivy counsel-projectile latex-preview-pane elpy ace-window escreen auctex emmet-mode racket-mode slime-company use-package undo-tree smex slime rainbow-delimiters paredit flycheck diminish counsel color-theme-molokai beacon))
 '(safe-local-variable-values
   '((projectile-project-test-dir . "src/")
     (projectile-project-test-suffix . ".test")
     (projectile-project-name . "mobile")
     (projectile-project-test-dir . "test-integration/")
     (projectile-project-test-suffix . ".ispec")
     (projectile-project-name . "API"))))

;; Reset garbage collection threshold
(setq gc-cons-threshold (* 2 1000 1000))
(put 'overwrite-mode 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Fira Code" :height 120))))
 '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin)))))


