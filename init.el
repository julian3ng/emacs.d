(setq julian/at-work (eq system-type 'darwin))
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t
        use-package-always-ensure t))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

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


(setq load-prefer-newer t)

(put 'upcase-region 'disabled nil)

(add-to-list 'load-path "~/.emacs.d/elisp/")

(add-to-list 'load-path "~/.emacs.d/pico8-mode/")
(when (file-exists-p "~/.emacs.d/pico8-mode/")
  (require 'pico8-mode))

(require 'my-system)
(require 'my-keybinds)
(require 'my-packages)
(require 'my-ui)
(require 'my-autocomplete)
(require 'my-modes)
(require 'my-skeletons)
(require 'my-remember)

(tab-bar-mode t)

(if (not (require 'o4m-utils nil t))
    (message "'o4m-utils not found"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c341518f5a80752f3113699a7f845dfc7299667311858e7cdfe64677d359d87e"
     "918b39b12c40c780831aac53d3047af862c72f24fe65b18cbdf2cb6fe0d14923"
     "21eb44ad0e958f71261c436a06577dc114b2850e5a82bc12314cf63c2d2d1db5"
     "2e7dc2838b7941ab9cabaa3b6793286e5134f583c04bde2fba2f4e20f2617cf7"
     "33bd2dce9979722e10637c30e47b55cf5a833544c254c4299dc97fb9b004f212"
     "046a2b81d13afddae309930ef85d458c4f5d278a69448e5a5261a5c78598e012"
     "06ed754b259cb54c30c658502f843937ff19f8b53597ac28577ec33bb084fa52"
     "b95f61aa5f8a54d494a219fcde9049e23e3396459a224631e1719effcb981dbd"
     "0170347031e5dfa93813765bc4ef9269a5e357c0be01febfa3ae5e5fcb351f09"
     "788121c96b7a9b99a6f35e53b7c154991f4880bb0046a80330bb904c1a85e275"
     "b5fab52f16546a15f171e6bd450ff11f2a9e20e5ac7ec10fa38a14bb0c67b9ab"
     "c7a926ad0e1ca4272c90fce2e1ffa7760494083356f6bb6d72481b879afce1f2"
     "0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850"
     "871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8"
     "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d"
     "7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98"
     "7c7026a406042e060bce2b56c77d715c3a4e608c31579d336cb825b09e60e827"
     "aa742450bc84284415b398be20bfe1c7e63b58fbbc4beb4f2709ce08f2ca3c92"
     "4c7228157ba3a48c288ad8ef83c490b94cb29ef01236205e360c2c4db200bb18"
     "62cf669c6e5c8a4295f9f8f71551abdb0f1a89aaf0dc82a9ecc41038c0320663"
     "ab42f5f3ec307f75fe7a959cdd1c00a093f7d4252453af085391ec789c83da85"
     "28d61ac6f26030e3c649e9f75b6ebd93dbf7f5f7b2f13e14cb1fe101e8cf4737"
     "4320a92406c5015e8cba1e581a88f058765f7400cf5d885a3aa9b7b9fc448fa7"
     "eb50f36ed5141c3f702f59baa1968494dc8e9bd22ed99d2aaa536c613c8782db"
     "f00a605fb19cb258ad7e0d99c007f226f24d767d01bf31f3828ce6688cbdeb22"
     "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3"
     "68b35e92f9daa37685218bd11aa5307140a0ec4c8fd17142a83457619e7b1240"
     "e5a748cbefd483b74b183d7da4fca6228207a6bf9be9792dc85403a186724e1f"
     "d516f1e3e5504c26b1123caa311476dc66d26d379539d12f9f4ed51f10629df3"
     "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336"
     "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428"
     "249e100de137f516d56bcf2e98c1e3f9e1e8a6dce50726c974fa6838fbfcec6b"
     "e8567ee21a39c68dbf20e40d29a0f6c1c05681935a41e206f142ab83126153ca"
     "11cc65061e0a5410d6489af42f1d0f0478dbd181a9660f81a692ddc5f948bf34"
     "9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3"
     "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041"
     "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37"
     "01ca8e215adc3a3221b42db10218dd181244e2971071207a384daeb9cbf31e58"
     "adf00c7e12b00a4ec402d6c695252a425776ca2a93de78805da52a1de67b0e1b"
     "277a5bce12d6957dbabb43a2f55ee2b6371388b749cbb29fd251df19334a1f0b"
     "7887cf8b470098657395502e16809523b629249060d61607c2225d2ef2ad59f5"
     "cca1d386d4a3f645c2f8c49266e3eb9ee14cf69939141e3deb9dfd50ccaada79" default))
 '(package-selected-packages
   '(ace-window apheleia auctex beframe bqn-mode cape code-review
                common-lisp-snippets corfu csv-mode ct dape delight devdocs
                devil diff-hl diminish docker dumber-jump eat eglot eldoc-box
                elfeed-summary elisp-autofmt eloud elpy embark-consult embrace
                emmet-mode emms-player-spotify erlang exec-path-from-shell
                fish-mode focus form-feed-st forth-mode fringe-current-line
                geiser-chicken glsl-mode gnu-apl-mode gnuplot goggles gptel
                gruvbox-theme helpful highlight howm iedit inf-ruby
                insert-kaomoji jinx jq-mode kaolin-themes ligature lin lua-mode
                magit-todos marginalia modus-themes multiple-cursors nov oauth2
                ollama-buddy orderless org-journal org-present org-preview-html
                org-roam org-static-blog paredit plantuml-mode poke-line
                protobuf-mode pulsar py-autopep8 rainbow-delimiters rainbow-mode
                ready-player restclient ripgrep rspec-mode seeing-is-believing
                sicp sideline-blame sideline-flymake sly-asdf sly-macrostep
                sly-quicklisp sly-repl-ansi-color sqlformat symbol-overlay
                terraform-mode transpose-frame uiua-mode uniline
                vertico-posframe visible-mark visual-regexp-steroids vundo w3m
                wc-mode web-mode wgrep writegood-mode writeroom-mode zoom))
 '(safe-local-variable-directories '("/Users/julian/Documents/Outcomes4me/api/"))
 '(safe-local-variable-values
   '((eval when (fboundp 'rainbow-mode) (rainbow-mode 1))
     (projectile-project-name . "emacs.d")
     (projectile-project-test-dir . "src/")
     (projectile-project-test-suffix . ".test")
     (projectile-project-name . "mobile")
     (projectile-project-test-dir . "test-integration/")
     (projectile-project-test-suffix . ".ispec")
     (projectile-project-name . "API"))))


;; Reset garbage collection threshold
(setq gc-cons-threshold julian--original-gc-cons-threshold)
(put 'overwrite-mode 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(defmacro delay (&rest body)
  `(lambda () (progn ,@body)))

(defmacro force (thunk)
  `(funcall ,thunk))

(defun julian/ensure-tab (position command group)
  (if (<= position (length (tab-bar-tabs)))
      (tab-bar-select-tab position)
    (tab-bar-new-tab-to position))
  (tab-bar-change-tab-group group position)
  (funcall command))

(defun julian/setup-workspaces ()
  (tab-bar-close-other-tabs 1)
  (julian/ensure-tab 1 #'elfeed-summary "ETC")
  (julian/ensure-tab 2 #'ielm "ETC")
  (julian/ensure-tab 3 (delay (find-file "~/Documents/Outcomes4me/api/") (magit-status)) "API")
  (julian/ensure-tab 4 (delay (find-file "~/Documents/Outcomes4me/mobile-3/") (magit-status)) "MOBILE")
  (julian/ensure-tab 5 (delay (find-file "~/Documents/Outcomes4me/web") (magit-status)) "WEB")
  (julian/ensure-tab 6 (delay (find-file "~/Documents/Outcomes4me/infrastructure") (magit-status)) "INFRA"))

(when julian/at-work
  (julian/setup-workspaces))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'list-timers 'disabled nil)

;; Not sure where to put this so that it's always evaluated?
(define-advice sly-completing-read (:around (fn &rest args))
  (let ((icomplete-mode t))
    (apply fn args)))

;; TODO: figure out how to configure ff-find-other-file and similar
