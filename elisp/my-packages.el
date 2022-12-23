(use-package elpy
  :config (elpy-enable))

(use-package molokai-theme :ensure t
  :config
;  (load-theme 'molokai t)
  ;; Comments are hard to read
  (let ((comment-color "#799")) 
 ;   (set-face-foreground 'font-lock-comment-face comment-color)
  ;  (set-face-foreground 'font-lock-comment-delimiter-face comment-color)
    ))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;(load-theme 'selenized-theme)



(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-dispatch-always nil)
  :bind
  ("M-o" . ace-window)
  ("C-x o" . other-window))

(use-package all-the-icons)
(use-package all-the-icons-ivy
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-file)
  (all-the-icons-ivy-setup))

(use-package avy
  :bind (("C-'" . avy-goto-char-timer)
         (:map isearch-mode-map ("C-'" . avy-isearch))))

(use-package beacon :init (beacon-mode) :diminish "")

(use-package company :diminish company-mode global-company-mode
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package slime-company)
(global-set-key (kbd "C-M-i") 'company-complete)

(use-package counsel :bind (("M-x" . counsel-M-x)
                      ("C-x C-f" . counsel-find-file)
                      ("C-x 8 C-<return>" . counsel-unicode-char)))

(use-package counsel-projectile
  :diminish counsel-projectile-mode
  :config (counsel-projectile-mode t)
  (setq counsel-projectile-switch-project-action 'counsel-projectile-switch-project-action-dired))

;; M-x stuff
(use-package smex)
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

(use-package exec-path-from-shell)

(use-package expand-region
  :commands expand-region
  :bind (("C-=" . er/expand-region)))

(use-package flycheck :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp
                                             emacs-lisp-checkdoc)
                flycheck-emacs-lisp-load-path 'inherit)
  :diminish global-flycheck-mode flycheck-mode)


;; (use-package gnu-apl-mode
;;   :config
;;   (setq gnu-apl-mode-map-prefix "H-")
;;   (setq gnu-apl-mode-map (gnu-apl--make-apl-mode-map)))

(add-hook 'prog-mode-hook #'hs-minor-mode)
(diminish 'hs-minor-mode "")

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h C" . helpful-command)))

(use-package ivy :diminish ivy-mode
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-initial-inputs-alist nil)
    (setq ivy-re-builders-alist
          '((t . ivy--regex-ignore-order)))
    (setq ivy-count-format "%d/%d ")))

(use-package magit :bind (("C-x g" . magit-status)))

;; ORG MODE CONFIG ============================================================
(use-package org
  :hook ((org-mode . auto-fill-mode)
         (org-mode . display-fill-column-indicator-mode))  
  :bind (:map org-mode-map ("C-'" . avy-goto-char-timer))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))  
  :config (progn (setq org-catch-invisible-edits 'show-and-error
                       org-hide-emphasis-markers nil
                       org-hide-leading-stars t
                       org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)"))
                       org-directory "~/org/"
                       org-capture-templates `(("i" "Inbox" entry (file "inbox.org")
                                                ,(concat "* TODO %?\n"
                                                         "/Entered on/ %U")))
                       org-agenda-files (list "~/org/agenda.org"
                                              "~/org/inbox.org"
                                              "~/org/gtd.org"
                                              "~/org-roam/")
                       org-agenda-hide-tags-regexp "."
                       org-format-latex-options (plist-put org-format-latex-options :scale 3.0)
                       org-adapt-indentation t)
                 (set-face-foreground 'org-block "#888")
                 (set-face-foreground 'org-code "aquamarine")
                 (set-face-foreground 'org-verbatim "#888")
                 )
  (add-to-list 'org-modules 'org-habit))




(unbind-key "C-c n d")
(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org-roam")
  :bind (("s-o l" . org-roam-buffer-toggle)
         ("s-o f" . org-roam-node-find)
         ("s-o i" . org-roam-node-insert)
         ("s-o I" . org-id-get-create)
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

(require 'ob-C)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (python . t)))

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
            (projectile-mode t))
  :bind (:map projectile-mode-map
              ("s-p" . 'projectile-command-map)))

(use-package racket-mode)

(use-package rainbow-mode :diminish rainbow-mode
  :config (rainbow-mode 1))

(use-package rainbow-delimiters :diminish ""
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode-enable))

(use-package restclient)

(use-package rspec-mode)

(use-package rvm
  :config (rvm-use-default))

(use-package inf-ruby
  :hook ((ruby-mode . inf-ruby-minor-mode)))

(use-package seeing-is-believing
  :config (setq seeing-is-believing-prefix "C-.")
  :hook ((ruby-mode . seeing-is-believing)))

(use-package lsp-mode
  :commands lsp
  :hook ((c-mode js-mode js-jsx-mode typescript-mode web-mode) . lsp-deferred)
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.5)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :init
  (setq lsp-keymap-prefix "C-s-l")
  :config
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-rust-clippy-preference "on")
  (setq lsp-enable-indentation nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config (setq lsp-ui-doc-enable nil
                lsp-ui-peek-enable nil
                lsp-ui-peek-always-show nil
                lsp-ui-sideline-show-hover t
                lsp-prefer-flymake nil))

;; Rust setup from https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/
(use-package rustic
  :bind (("C-c C-c j" . lsp-ui-imenu)
         ("C-c C-c f" . lsp-find-references)
         ("C-c C-c r" . lsp-rename)
         ("C-c C-c q" . lsp-workspace-restart)
         ("C-c C-c Q" . lsp-workspace-shutdown)
         ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-format-on-save t))



(use-package lua-mode)
(use-package love-minor-mode)

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"
        slime-lisp-implementations '((sbcl ("/usr/bin/sbcl")))
        slime-contribs '(slime-fancy))
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . common-lisp-mode))
  (load "/home/julian/quicklisp/clhs-use-local.el" t))

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
  :config (setq undo-tree-history-directory-alist '(("." . "/home/julian/.emacs.d/undo/"))))

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
              ("C-c C-b" . bqn-process-execute-buffer))
  )

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

(provide 'my-packages)

