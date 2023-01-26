(use-package erc
     :config
     (setq erc-server "irc.libera.chat"
           erc-port 6697
           erc-nick "themonkeybob11"))

(use-package auctex
  :defer t
  :ensure t)

(use-package elpy
  :init (elpy-enable))

(use-package jedi
  :config (setq jedi:complete-on-dot t)
  :hook ((python-mode . jedi:setup)))

(use-package py-autopep8)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'selenized t)

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-dispatch-always nil)
  :bind
  ("M-o" . ace-window)
  ("C-x o" . other-window))

(use-package all-the-icons)
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package avy
  :bind (("C-'" . avy-goto-char-timer)
         ((:map isearch-mode-map ("C-'" . avy-isearch)))))


;; M-x stuff
(use-package diminish)
(use-package delight)
(use-package dictionary)

(use-package elfeed
  :bind (("C-c e" . elfeed))
  :config (setq elfeed-feeds '(("http://feeds.bbci.co.uk/news/video_and_audio/world/rss.xml" news world)
                               ("https://www.techmeme.com/feed.xml" news tech)
                               ("https://www.engadget.com/rss.xml" news tech)
                               ("http://rss.slashdot.org/Slashdot/slashdotMain" news tech)
                               ("https://www.wired.com/feed/rss" news tech)
                               ("https://css-tricks.com/feed/" tech)
                               ("https://feeds.feedburner.com/codinghorror" blog tech)
                               ("https://jvns.ca/atom.xml" blog tech)
                               ("https://feeds.feedburner.com/codinghorror" blog tech)
                               ("https://slatestarcodex.com/feed/" blog)
                               ("https://mathbabe.org/feed/" blog math)
)))

;; HTML/CSS expansion
(use-package emmet-mode :diminish emmet-mode)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :commands expand-region
  :bind (("C-=" . er/expand-region)))

;; (use-package flycheck :init (global-flycheck-mode)
;;   :config
;;   (setq-default flycheck-disabled-checkers '(emacs-lisp
;;                                              emacs-lisp-checkdoc)
;;                 flycheck-emacs-lisp-load-path 'inherit)
;;   :diminish global-flycheck-mode flycheck-mode)


;; (use-package gnu-apl-mode
;;   :config
;;   (setq gnu-apl-mode-map-prefix "H-")
;;   (setq gnu-apl-mode-map (gnu-apl--make-apl-mode-map)))

(use-package hideshow
  :ensure nil
  :hook ((prog-mode-hook . hs-minor-mode))
  :diminish hs-minor-mode
  :bind (("s-h a" . hs-show-all)
         ("s-h t" . hs-hide-all)
         ("s-h c" . hs-toggle-hiding)
         ("s-h d" . hs-hide-block)
         ("s-h l" . hs-hide-level)))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h C" . helpful-command)))

;; (use-package ivy :diminish ivy-mode
;;   :config
;;   (progn
;;     (ivy-mode 1)
;;     (setq ivy-use-virtual-buffers t)
;;     (setq ivy-initial-inputs-alist nil)
;;     (setq ivy-re-builders-alist
;;           '((t . ivy--regex-ignore-order)))
;;     (setq ivy-count-format "%d/%d ")))

(use-package magit
  :bind (
         ("C-x g" . magit-status)
         ("C-c M-g" . magit-dispatch)
         ("C-c g" . magit-file-dispatch))
  :config (setq magit-process-finish-apply-ansi-colors t))

;; ORG MODE CONFIG ============================================================
(use-package org
  :hook ((org-mode . auto-fill-mode)
         (org-mode . display-fill-column-indicator-mode))
  :bind (:map org-mode-map ("C-'" . avy-goto-char-timer))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config (progn (setq org-catch-invisible-edits 'show-and-error
                       org-startup-folded t
                       org-hide-block-startup t
                       org-hide-emphasis-markers nil
                       org-hide-leading-stars t
                       org-todo-keywords '((sequence "TODO(t)" "|" "DONE(D!)")
                                           (sequence "TD(t)" "WAITING(w)" "IP(i)" "CR(c)" "PR(p)" "RT(r)" "DP(d)" "|" "DN(D!)"))
                       org-use-fast-todo-selection 'expert
                       org-todo-keyword-faces '(("TODO" . "red")
                                                ("WAITING" . "BlueViolet")
                                                ("DONE" . "lightGreen")
                                                ("TD" . "red")
                                                ("IP" . "yellow")
                                                ("CR" . "SeaGreen1")
                                                ("PR" . "SeaGreen2")
                                                ("RT" . "SeaGreen3")
                                                ("DP" . "SeaGreen4")
                                                ("DN" . "DodgerBlue")
                                                )
                       org-directory "~/org/"
                       org-capture-templates `(("i" "Inbox" entry (file "inbox.org")
                                                ,(concat "* TODO %?\n"
                                                         "  /Entered on/ %U")))
                       org-agenda-files (list "~/org/gtd.org"
                                              "~/org/inbox.org")
                       org-refile-use-outline-path 'file
                       org-outline-path-complete-in-steps nil
                       org-refile-targets '((nil :maxlevel . 3)
                                            (org-agenda-files :maxlevel . 3))
                       org-agenda-hide-tags-regexp "."
                       org-format-latex-options (plist-put org-format-latex-options :scale 3.0)
                       org-adapt-indentation t
                       org-use-speed-commands t
                       org-agenda-custom-commands '(("g" "Fortnight Agenda" ((agenda "" ((org-agenda-span 14)))))))
                 (set-face-foreground 'org-block "#888")
                 (set-face-foreground 'org-code "aquamarine")
                 (set-face-foreground 'org-verbatim "#888"))
  (add-to-list 'org-modules 'org-habit))

;(unbind-key "C-c n d") ; what was this for??
(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org-roam")
  :bind (("s-o l" . org-roam-buffer-toggle)
         ("s-o f" . org-roam-node-find)
         ("s-o i" . org-roam-node-insert)
         ("s-o I" . org-id-get-create)
         ("s-o c" . org-roam-capture)
         ("s-o t" . org-roam-tag-add)
         ("s-o d" . org-roam-dailies-capture-today)
         ("s-o g" . org-roam-db-sync))
  :config
  (org-roam-db-autosync-mode t)
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates  '(("d" "default" entry
                                               "* %?"
                                               :target (file+head "%<%Y-%m-%d>.org"
                                                                  "#+title: %<%Y-%m-%d>\n")))))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package ob-C
  :ensure nil
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (python . t))))

(use-package org-journal)

;; END ORG MODE CONFIG ========================================================
(use-package paredit
  :diminish paredit-mode
  :hook ((lisp-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode))
  :bind (("M-d" . paredit-forward-kill-word))
  :config (progn (add-hook 'lisp-mode-hook #'paredit-mode)
                 (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
                 (add-hook 'common-lisp-mode-hook #'paredit-mode)
                 (add-hook 'scheme-mode-hook #'paredit-mode)
                 (add-hook 'racket-mode-hook #'paredit-mode)))

(defun paredit-space-for-delimiter-p (endp delimiter)
  "Paredit spacing tweaks"
  (and (not (if endp (eobp) (bobp)))
       (memq (char-syntax (if endp (char-after) (char-before)))
             (list ?\" ;; REMOVED ?w ?_
                   (let ((matching (matching-paren delimiter)))
                     (and matching (char-syntax matching)))))))


(use-package projectile
  :diminish projectile-mode
  :config (progn
            (add-to-list 'projectile-globally-ignored-directories "node_modules")
            (setq projectile-switch-project-action #'magit)
            (projectile-mode t))
  :bind (:map projectile-mode-map
              ("s-p" . 'projectile-command-map)))

(use-package racket-mode)

(use-package rainbow-mode :diminish rainbow-mode
  :config (rainbow-mode 1))

(use-package rainbow-delimiters :diminish ""
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode-enable))


(use-package restclient)


;;(use-package enh-ruby-mode)
(use-package rspec-mode)

(use-package inf-ruby
  :hook ((ruby-mode . inf-ruby-minor-mode)))

(use-package seeing-is-believing
  :config (setq seeing-is-believing-prefix "C-c ?")
  :hook ((ruby-mode . seeing-is-believing)))

;; Rust setup from https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/
;; (use-package rustic
;;   :bind (("C-c C-c j" . lsp-ui-imenu)
;;          ("C-c C-c f" . lsp-find-references)
;;          ("C-c C-c r" . lsp-rename)
;;          ("C-c C-c q" . lsp-workspace-restart)
;;          ("C-c C-c Q" . lsp-workspace-shutdown)
;;          ("C-c C-c s" . lsp-rust-analyzer-status))
;;   :config
;;   (setq rustic-format-on-save t))

(use-package lua-mode)
(use-package love-minor-mode)

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"
        slime-lisp-implementations '((sbcl ("/usr/bin/sbcl")))
        slime-contribs '(slime-fancy))
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . common-lisp-mode))
  (load "/home/julian/quicklisp/clhs-use-local.el" t))

;; Better highlights
(use-package symbol-overlay
  :bind (("C-c h h" . symbol-overlay-put)
         ("C-c h n" . symbol-overlay-jump-next)
         ("C-c h p" . symbol-overlay-jump-prev)
         ("C-c h f" . symbol-overlay-switch-forward)
         ("C-c h b" . symbol-overlay-switch-backward)
         ("C-c h r" . symbol-overlay-rename)
         ("C-c h m" . symbol-overlay-mode)
         ("C-c h C" . symbol-overlay-remove-all)))

(use-package undo-tree
  :diminish ""
  :init (progn (global-undo-tree-mode) (setq undo-tree-visualizer-timestamps t))
  :bind (("s-u" . undo-tree-visualize))
  :config (setq undo-tree-history-directory-alist '(("." . "/.emacs.d/undo/"))
                undo-tree-auto-save-history nil))

(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode)
  :config (which-key-setup-side-window-right-bottom))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (setq web-mode-engines-alist '(("php" . "\\.php\\'")))
  (setq web-mode-enable-auto-indentation nil))

(use-package writegood-mode)
(use-package writeroom-mode
  :config
  (setq writeroom-width 80
        writeroom-fullscreen-effect 'maximized))


(use-package wc-mode)

(use-package yasnippet
  :commands yas-global-mode
  :config
  (yas-reload-all)
  (yas-global-mode 1))

(use-package common-lisp-snippets)
(use-package geiser)

(use-package gnu-apl-mode) ;; this is for C-\ APL input method

(use-package bqn-mode
  :ensure nil
  :bind (:map bqn--mode-map
              ("C-c C-l" . bqn-process-execute-line)
              ("C-c C-b" . bqn-process-execute-buffer)))

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                     ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                     "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode t))



;; Vertico/Marginalia/

(use-package vertico
  :init (vertico-mode))
(use-package savehist
  :init (savehist-mode))
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package consult)

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Completion frontend
(use-package corfu
  :init (global-corfu-mode))


(use-package company-emoji)
(use-package company-web)
(use-package company-lua)
;; Completion backend
(use-package cape
  :bind (("C-c C-p p" . completion-at-point) ;; capf
         ("C-c C-p t" . complete-tag)        ;; etags
         ("C-c C-p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c C-p h" . cape-history)
         ("C-c C-p f" . cape-file)
         ("C-c C-p k" . cape-keyword)
         ("C-c C-p s" . cape-symbol)
         ("C-c C-p a" . cape-abbrev)
         ("C-c C-p i" . cape-ispell)
         ("C-c C-p l" . cape-line)
         ("C-c C-p w" . cape-dict)
         ("C-c C-p \\" . cape-tex)
         ("C-c C-p _" . cape-tex)
         ("C-c C-p ^" . cape-tex)
         ("C-c C-p &" . cape-sgml)
         ("C-c C-p r" . cape-rfc1345))
  :init
  (defun julian/cape-capf-setup ()
    (let ((result))
      (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-emoji))
      (dolist (capf '(cape-dabbrev cape-file) result)
        (add-to-list 'completion-at-point-functions capf))))
  (defun julian/cape-capf-setup-lua ()
    (julian/cape-capf-setup)
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-lua)))
  :hook (prog-mode . julian/cape-capf-setup)
  :hook (lua-mode . julian/cape-capf-setup-lua)
  :config
  ;;(add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-web-html))
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package eglot
  :bind (("C-c ! n" . flymake-goto-next-error)
         ("C-c ! p" . flymake-goto-prev-error)
         ("C-c ! l" . flymake-show-buffer-diagnostics)
         ("C-c ! L" . flymake-show-project-diagnostics)
         ("s-l c a" . eglot-code-actions)
         ("s-l r r" . eglot-rename)
         ("s-l g t" . eglot-find-typeDefinition))
  :config
  (setq eglot-ignored-server-capabilities nil))

(use-package markdown-mode) ;; we need markdown mode for eglot's eldoc to render

(use-package eldoc
  :config
  (setq
   eldoc-echo-area-use-multiline-p t
   eldoc-echo-area-prefer-doc-buffer nil))

(use-package eldoc-box
  :after eldoc
  :hook (eglot--managed-mode-hook . eldoc-box-hover-mode))

(use-package flymake)

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . hippie-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; asdf package manager to find ruby and such
(add-to-list 'load-path "~/.emacs.d/elisp/asdf.el")
(require 'asdf)
(asdf-enable)

(use-package eyebrowse
  :init
  (setq eyebrowse-new-workspace t)
  :bind (
         :map eyebrowse-mode-map
         ("C-c C-w n" . eyebrowse-next-window-config)
         ("C-c C-w C-n" . eyebrowse-next-window-config)
         ("C-c C-w p" . eyebrowse-prev-window-config)
         ("C-c C-w C-p" . eyebrowse-prev-window-config)
         ("C-c C-w f" . eyebrowse-next-window-config)
         ("C-c C-w C-f" . eyebrowse-next-window-config)
         ("C-c C-w b" . eyebrowse-prev-window-config)
         ("C-c C-w C-b" . eyebrowse-prev-window-config))
  :config
  (eyebrowse-mode t))

(require 'eyebrowse)

(use-package smerge-mode
  :ensure nil
  :config
  (setq smerge-command-prefix "C-c C-s"))

(use-package iedit
  :bind ("C-c i" . iedit-mode))

(use-package typescript-mode
  :config
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  (setq typescript-indent-level 2))

(use-package nyan-mode
  :config (nyan-mode 1))

(defun julian/push-mark-no-activate ()
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

;; not sure about this stuff
(use-package visible-mark
  :bind (("C-c C-SPC" . transient-mark-mode)
         ("C-M-SPC" . julian/push-mark-no-activate))
  :config
  (global-visible-mark-mode t)
  (setq visible-mark-max 3))

(provide 'my-packages)
