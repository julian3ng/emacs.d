(setq byte-compile-warnings '(cl-functions)) ;; TEMPORARY: turn off cl package deprecation warning

(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t
        use-package-always-ensure t))

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
                                        ;(setq package-enable-at-startup nil)

;; (setq package-archives
;;       '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
;;         ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
;;         ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(setq package-archive-priorities '(("melpa"    . 5)
                                   ("jcs-elpa" . 0)))


                                        ;(package-initialize)
(setq load-prefer-newer t)

(put 'upcase-region 'disabled nil)

(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path "~/.emacs.d/pico8-mode/")

(require 'pico8-mode)

(require 'my-system)
(require 'my-ui)
(require 'my-keybinds)
(require 'my-packages)
                                        ;(require 'linear)
                                        ;(require 'my-meow)
(require 'my-skeletons)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8" "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d" "7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98" "7c7026a406042e060bce2b56c77d715c3a4e608c31579d336cb825b09e60e827" "aa742450bc84284415b398be20bfe1c7e63b58fbbc4beb4f2709ce08f2ca3c92" "4c7228157ba3a48c288ad8ef83c490b94cb29ef01236205e360c2c4db200bb18" "62cf669c6e5c8a4295f9f8f71551abdb0f1a89aaf0dc82a9ecc41038c0320663" "ab42f5f3ec307f75fe7a959cdd1c00a093f7d4252453af085391ec789c83da85" "28d61ac6f26030e3c649e9f75b6ebd93dbf7f5f7b2f13e14cb1fe101e8cf4737" "4320a92406c5015e8cba1e581a88f058765f7400cf5d885a3aa9b7b9fc448fa7" "eb50f36ed5141c3f702f59baa1968494dc8e9bd22ed99d2aaa536c613c8782db" "f00a605fb19cb258ad7e0d99c007f226f24d767d01bf31f3828ce6688cbdeb22" "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3" "68b35e92f9daa37685218bd11aa5307140a0ec4c8fd17142a83457619e7b1240" "e5a748cbefd483b74b183d7da4fca6228207a6bf9be9792dc85403a186724e1f" "d516f1e3e5504c26b1123caa311476dc66d26d379539d12f9f4ed51f10629df3" "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336" "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428" "249e100de137f516d56bcf2e98c1e3f9e1e8a6dce50726c974fa6838fbfcec6b" "e8567ee21a39c68dbf20e40d29a0f6c1c05681935a41e206f142ab83126153ca" "11cc65061e0a5410d6489af42f1d0f0478dbd181a9660f81a692ddc5f948bf34" "9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37" "01ca8e215adc3a3221b42db10218dd181244e2971071207a384daeb9cbf31e58" "adf00c7e12b00a4ec402d6c695252a425776ca2a93de78805da52a1de67b0e1b" "277a5bce12d6957dbabb43a2f55ee2b6371388b749cbb29fd251df19334a1f0b" "7887cf8b470098657395502e16809523b629249060d61607c2225d2ef2ad59f5" "cca1d386d4a3f645c2f8c49266e3eb9ee14cf69939141e3deb9dfd50ccaada79" default))
 '(package-selected-packages
   '(howm pico8-mode rfc-mode casual-calc gnuplot code-review jinx emms emms-player-spotify multiple-cursors transpose-frame gptel casual-dired beframe sqlformat elpy vundo read-aloud jq-mode jq-ts-mode casual goggles elfeed-summary elfeed-sumary eglot-booster eglot-booster org-preview-html dracula-theme eink-theme gruvbox-theme haki-theme anti-zenburn-theme zenburn-theme nord-theme nordic-night-theme nov sideline-blame sideline-flymake sideline chatgpt-shell fringe-current-line all-the-icons geiser-chicken bqn-mode noaa universal-sidecar w3m nhexl-mode embrace plantuml-mode forth-mode ef-themes combobulate visual-regexp-steroids visual-regexp kaolin-themes modus-themes sicp highlight prism terraform-mode dirvish devil olivetti magit pug-mode poke-line eat autothemer chess projectile-ripgrep ripgrep apheleia emacs-rotate pcre2el minions bm hs-minor-mode smerge iedit eyebrowse shortdoc company-restclient company-web company-emoji cape corfu embark-consult embark consult marginalia orderless vertico ligature visible-mark meow treemacs treemacs-all-the-icons treemacs-magit treemacs-projectile yasnippet-snippets elixir-mode php-mode helpful gnu-apl-mode py-autopep8 autopep8 j-mode dyalog-mode dictionary org-roam rustic molokai-theme protobuf-mode edit-indirect vagrant vagrant-tramp writeroom-mode wc-mode writegood-mode org-bullets rubocopfmt exec-path-from-shell seeing-is-believing geiser selectrum julia-mode julia-repl nyan-mode ruby-electric-mode inf-ruby-mode inf-ruby rspec-mode company-lua lua-mode web-mode which-key restclient color-theme-modern :gnu-apl-mode cider clojure-mode sonic-pi elfeed org-journal rainbow-mode common-lisp-snippets expand-region symbol-overlay delight erc-image erc-hl-nicks flycheck-ledger ledger-mode erlang yaml-mode glsl-mode company-lsp lsp-ui wpuzzle fzf rg ack map spinner lsp-mode evil hideshowvis all-the-icons-ivy counsel-projectile latex-preview-pane ace-window escreen auctex emmet-mode racket-mode slime-company use-package undo-tree smex rainbow-delimiters paredit flycheck diminish counsel color-theme-molokai beacon))
 '(package-vc-selected-packages
   '((pico8-mode :vc-backend Git :url "https://github.com/Kaali/pico8-mode")))
 '(safe-local-variable-values
   '((eval when
           (fboundp 'rainbow-mode)
           (rainbow-mode 1))
     (projectile-project-name . "emacs.d")
     (projectile-project-test-dir . "src/")
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
 '(hl-line ((t :underline (:color "lime"))))
 '(sideline-blame ((t (:italic t :background unspecified :foreground "#7a88cf"))))
 '(tab-bar-tab ((t :foreground "#ff8844")))
 '(tab-bar-tab-group-current ((t :foreground "#ff8844" :background "#282828")))
 '(tab-bar-tab-group-inactive ((t :foreground "#446688" :background "#282828")))
 '(tab-bar-tab-inactive ((t :foreground "#7c6f64")))
 '(variable-pitch ((t (:family "ETBembo" :weight thin)))))


