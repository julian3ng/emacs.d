(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package ollama-buddy
  :bind (("C-c o" . ollama-buddy-menu)))

(use-package autothemer)
(use-package erc
  :config
  (setq erc-server "irc.libera.chat"
        erc-port 6697
        erc-nick "themonkeybob11")
  (add-to-list 'erc-modules 'services)
  (setq erc-prompt-for-nickserv-password nil))

(use-package auctex
  :defer t
  :ensure t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(use-package ace-window
  :demand t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-dispatch-always nil)
  (set-face-foreground 'aw-mode-line-face "#f0f")
  (ace-window-display-mode t)

  :bind
  ("M-o" . ace-window)
  ("C-x o" . other-window))

(use-package avy
  :config (defvar julian/avy-keymap
            (define-keymap
              "c"  #'avy-goto-char
              "'"  #'avy-goto-char
              "\""  #'avy-goto-char-2
              "t" #'avy-goto-char-timer
              "w" #'avy-goto-word-0
              "l"  #'avy-goto-line))
  (setq avy-timeout-seconds 0.3
        avy-style 'at-full
        avy-background t
        avy-single-candidate-jump nil
        )
  :bind-keymap ("C-;" . julian/avy-keymap)
  :bind ((:map isearch-mode-map ("C-'" . avy-isearch)))
  :bind (("C-'" . avy-goto-char-timer)))

(use-package multiple-cursors
  :config (setq julian/mc-keymap
                (define-keymap
                  "e" #'mc/edit-lines
                  "E" #'mc/edit-ends-of-lines
                  "n" #'mc/mark-next-like-this
                  "p" #'mc/mark-previous-like-this
                  "a" #'mc/mark-all-like-this-dwim
                  ))
  :bind-keymap (("s-m" . julian/mc-keymap)))

;; ;; M-x stuff
(use-package diminish)
(use-package delight)
(use-package dictionary)

 (defun julian/elfeed-go-to-comments ()
   (interactive)
   (elfeed-show-next-link)
   (shr-browse-url))

(use-package elfeed
  ;; filters
  ;; press "s" to start edit the filter
  ;; +/- requires/diables a tag
  ;; ex. +games -blog
  ;; @ starts a date / date range
  ;; ex. @10-days-ago--5-days-ago
  ;; ! inverts regex
  ;; = matches regex on entry's title or url (entry matches if hits at least one =)
  ;; ~ inverts regex on entry's title or url
  ;; # restricts number of entries
  ;;
  :bind (("C-c e" . elfeed)
         (:map elfeed-show-mode-map
               ("C" . julian/elfeed-go-to-comments)))
  :config (setq elfeed-feeds '(("https://news.ycombinator.com/rss" news tech)
                               ("https://lobste.rs/rss" news tech)
                               ("https://arraycast.com/episodes?format=rss" podcast pl)
                               ("https://reddit.com/r/forth.rss" reddit forth)
                               ("https://reddit.com/r/emacs.rss" reddit emacs)
                               ("https://css-tricks.com/feed/" tech)
                               ("https://feeds.feedburner.com/codinghorror" blog tech)
                               ("https://jvns.ca/atom.xml" blog tech)
                               ("https://slatestarcodex.com/feed/" blog)
                               ("https://feeds.ign.com/ign/games-all" games)
                               ("https://polygon.com/rss/index.xml" games)
                               ("https://mathbabe.org/feed/" blog math)
                               ("https://ciechanow.ski/atom.xml" blog css)
                               ("https://planet.emacslife.com/atom.xml" blog emacs)
                               ("https://karthinks.com/index.xml" blog emacs)
                               ;; ("https://www.cnet.com/rss/gaming/" games cnet)
                               ("http://crawl.develz.org/wordpress/feed" games)
                               ("https://nicole.express/feed.xml" blog tech)
                               ("https://danluu.com/atom.xml" blog tech)
                               ("https://lambdaland.org/index.xml" blog tech)
                               ("https://xeiaso.net/blog.rss" blog tech)
                               ("http://feeds.feedburner.com/CbloomRants" blog tech)
                               ("https://ericlippert.com/feed/" blog tech)
                               ("https://alvaromontoro.com/feed.rss" blog css)
                               ("https://www.siteinspire.com/websites/feed" blog design)
                               ("https://caseymuratori.com/blog_atom.rss" blog dev)
                               ("https://hackaday.com/blog/feed/" tech)
                               ("https://www.wheresyoured.at/rss" blog)
                               ("https://twostopbits.com/rss" blog tech)
                               ("https://yoric.github.io/index.xml" blog tech)
                               ("https://www.redblobgames.com/blog/posts.xml" blog tech games)
                               ("https://www.internalpointers.com/rss" blog tech)
                               ("https://jakelazaroff.com/rss.xml" blog tech)
                               ("https://simonwillison.net/atom/everything/" blog tech)
                               ("https://feedpress.me/512pixels" blog tech history)
                               ("https://gamedev.city/rss" news gamedev)
                               ("https://blog.jpalardy.com/atom.xml" blog tech)
                               ("https://catonmat.net/feed" blog tech)
                               ("https://localthunk.com/?format=rss" blog gamedev)))
  (setq shr-inhibit-images t)
  (setq-default elfeed-search-filter "@1-month-ago +unread"
                elfeed-search-title-max-width 100))

(use-package elfeed-summary
  :bind (("C-c E" . elfeed-summary))
  :bind (:map elfeed-summary-mode-map
              ("p" . magit-section-backward))
  :config
  (setq elfeed-summary-settings
        '((auto-tags (:title "All feeds"
                             :max-level 2)))))

;; ;; HTML/CSS expansion
(use-package emmet-mode :diminish emmet-mode)

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(use-package hideshow
  :ensure nil
  :hook ((prog-mode-hook . hs-minor-mode))
  :diminish hs-minor-mode
  :bind (:map hs-minor-mode-map
              ("s-h a" . hs-show-all)
              ("s-h t" . hs-hide-all)
              ("s-h c" . hs-toggle-hiding)
              ("s-h d" . hs-hide-block)
              ("s-h l" . hs-hide-level)))

(use-package hideshowvis
  :hook ((prog-mode-hook . hideshowvis-enable)))

(use-package magit
  :bind (
         ("C-x g" . magit-status)
         ("C-c M-g" . magit-dispatch)
         ("C-c g" . magit-file-dispatch))
  :init
  ;; (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M:%S" magit-log-margin-width t 18))

  :config
  (setq
   git-commit-major-mode 'markdown-mode
   magit-process-finish-apply-ansi-colors t
   magit-status-margin '(nil "%Y-%m-%d %H:%M:%S" magit-log-margin-width nil 18)
   magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
   magit-bury-buffer-function #'magit-restore-window-configuration)

  (defalias 'julian/magit-refresh-origin-develop
    (kmacro "f r <return> d e v e l o p : d e v e l o p <return>")))

(defun julian/magit-refresh-origin-develop ()
  (interactive)
  (magit-fetch-refspec "origin" "develop:develop" (magit-fetch-arguments)))

;; ORG MODE CONFIG ============================================================
(use-package org
  :init (progn (setq org-fold-core-style 'overlays))
  :hook ((org-mode . (lambda () (display-line-numbers-mode 0)))
         (org-mode . (lambda () (display-fill-column-indicator-mode 0)))
         (org-mode . visual-line-mode)
         (org-mode . abbrev-mode))

  :bind (:map org-mode-map ("C-'" . avy-goto-char-timer)
              ("C-c n" . org-next-item)
              ("C-c p" . org-previous-item))
  :bind (("C-c a" . org-agenda)
         ("C-c C" . org-capture)
         ("C-c l" . org-store-link))
  :config (progn (setq
                  org-src-window-setup 'split-window-below
                  org-catch-invisible-edits 'show-and-error
                  org-startup-folded t
                  org-hide-block-startup t
                  org-hide-emphasis-markers nil
                  org-hide-leading-stars nil
                  org-todo-keywords '((sequence "TODO(t)" "∈PRG(i)" "CR(c)" "PR(p)" "RT(r)" "|" "DONE(D)" "CA(C)" "BL(B)"))
                  org-use-fast-todo-selection 'expert
                  org-todo-keyword-faces '(("TODO" . "lightGrey")
                                           ("∈PRG" . "yellow")
                                           ("CR" . "green")
                                           ("PR" . "green")
                                           ("RT" . "green")
                                           ("DONE" . "royalBlue")
                                           ("CA" . "grey")
                                           ("BL" . "red"))
                  org-directory "~/org/"
                  org-capture-templates `(
                                          ("t" "ticket" entry (file+olp "gtd.org" "Tickets")
                                           ,(concat "** %?\n" "*** Ticket Body\n" "*** Paperwork\n" "*** Notifications"))
                                          ("i" "Inbox" entry (file "inbox.org")
                                           ,(concat "* TODO %?\n"
                                                    "  /Entered on/ %U"))
                                          ("c" "Code" entry (file "inbox.org")
                                           ,(concat "* TODO %?\n"
                                                    "  %A\n"))
                                          ("f" "Friction" entry (file+olp "gtd.org" "Logs" "Friction Log")
                                           ,(concat
                                             "*** %U %^{Title} (%^{Size|small|small|medium|large})\n"
                                             "**** Context\n"
                                             "     - How familiar are you with the feature?\n"
                                             "     - What are you trying to build?\n"
                                             "     - What parts of the feature is this log about?\n"
                                             "**** Pros/Cons: useful parts and improvements/wrong expectations\n"
                                             "**** Stream of Consciousness\n"
                                             )
                                           ))
                  org-agenda-files (list "~/Documents/journal/")
                  org-refile-use-outline-path 'file
                  org-outline-path-complete-in-steps nil
                  org-refile-targets '((nil :maxlevel . 3)
                                       (org-agenda-files :maxlevel . 3))
                  org-agenda-hide-tags-regexp "."
                  org-format-latex-options (plist-put org-format-latex-options :scale 3.0)
                  org-adapt-indentation t
                  org-use-speed-commands t
                  org-agenda-custom-commands '(("g" "Fortnight Agenda" ((agenda "" ((org-agenda-span 14))))))
                  org-priority-highest 0
                  org-priority-lowest 9
                  org-priority-default 5)
                 (set-face-foreground 'org-block "#888")
                 (set-face-foreground 'org-code "aquamarine")
                 (set-face-foreground 'org-verbatim "#888")
                 (set-face-foreground 'org-hide (face-attribute 'default :background))
                 (set-face-background 'org-hide (face-attribute 'default :background))
                 (add-to-list 'org-modules 'org-habit)))

(require 'org-tempo) ;; make <s work again

;; (use-package org-roam
;;   :init
;;   (setq org-roam-v2-ack t)
;;   :custom
;;   (org-roam-directory "~/org/roam")
;;   :bind (("s-o l" . org-roam-buffer-toggle)
;;          ("s-o f" . org-roam-node-find)
;;          ("s-o i" . org-roam-node-insert)
;;          ("s-o I" . org-id-get-create)
;;          ("s-o c" . org-roam-capture)
;;          ("s-o t" . org-roam-tag-add)
;;          ("s-o d" . org-roam-dailies-capture-today)
;;          ("s-o g" . org-roam-db-sync))
;;   :config
;;   (org-roam-db-autosync-mode t)
;;   (setq org-roam-dailies-directory "daily/")
;;   (setq org-roam-dailies-capture-templates  '(("d" "default" entry
;;                                                "* %?"
;;                                                :target (file+head "%<%Y-%m-%d>.org"
;;                                                                   "#+title: %<%Y-%m-%d>\n")))))
(use-package ob-C
  :ensure nil
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (python . t)
     (ruby . t))))

(use-package org-static-blog
  :config
  (setq
   org-static-blog-publish-title "Blog"
   org-static-blog-publish-url "https://julian3ng.github.io/"
   org-static-blog-publish-directory "~/Public/julian3ng.github.io"
   org-static-blog-posts-directory "~/Public/julian3ng.github.io/posts"
   org-static-blog-drafts-directory "~/Public/julian3ng.github.io/drafts"
   org-static-blog-enable-tags t
   org-export-with-toc nil
   org-export-with-section-numbers nil
   org-static-blog-index-front-matter "<h1> Welcome to my blog </h1>\n"
   org-static-blog-use-preview t))

;; ;; END ORG MODE CONFIG ========================================================
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

(use-package ripgrep)

;; ;; ; EXAMPLE DIR LOCALS FOR PROJECTILE PROJECT
;; ;; ; these get "s-p t" to work
;; ;; ((nil . ((projectile-project-name . "API")
;; ;;          (projectile-project-test-suffix . ".ispec")
;; ;;          (projectile-project-test-dir . "test-integration/"))))

(use-package rainbow-mode :diminish rainbow-mode
  :hook ((prog-mode . rainbow-mode)))

(use-package rainbow-delimiters :diminish ""
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode-enable))

(use-package robe)

;; (use-package inf-ruby
;;   :hook ((ruby-mode . inf-ruby-minor-mode)))

(use-package lua-mode)

(use-package sly
  :after vertico
  :config
  (setq inferior-lisp-program "sbcl")
  (load "/home/julian/quicklisp/clhs-use-local.el" t)
  ;; Everything should go through vertico, not sly
  (sly-symbol-completion-mode -1))



(use-package sly-asdf :after sly)
(use-package sly-macrostep :after sly)
(use-package sly-quicklisp :after sly)
(use-package sly-repl-ansi-color :after sly)


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

(use-package vundo
  :bind (("s-u" . vundo))
  :config (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (setq web-mode-engines-alist '(("php" . "\\.php\\'")))
  (setq web-mode-enable-auto-indentation nil))

(use-package geiser)
(use-package geiser-chicken)

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
  (ligature-set-ligatures 'org-mode '("www" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
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



;; ;; Vertico/Marginalia/
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :after orderless
  :init (vertico-mode)
  :config (setq vertico-count 48))

(use-package savehist
  :init (savehist-mode))


;; ;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

;; ;; default to default directory instead of project dir
(defun julian/consult-grep-here (&optional dir)
  (interactive "P")
  (consult-grep (if dir dir default-directory)))

(unbind-key "s-k")
(use-package consult
  :bind (("C-c c g" . julian/consult-grep-here)
         ("C-c c l" . consult-line)
         ("C-c c f" . consult-find)
         ("C-c c m" . consult-mark)
         ("C-c c b" . consult-buffer)
         ("C-c c F" . consult-focus-lines)

         ("s-k e" . consult-flymake)
         ("s-k g" . julian/consult-grep-here)
         ("s-k G" . consult-ripgrep)
         ("s-k l" . consult-line)
         ("s-k f" . consult-find)
         ("s-k SPC" . consult-mark)
         ("s-k m" . consult-mark)
         ("s-k b" . consult-buffer)
         ("s-k F" . consult-focus-lines)
         ("s-k y" . consult-yank-from-kill-ring)
         ("s-k r" . consult-register)
         ("s-k T" . consult-theme)
         ("s-k x" . consult-complex-command))
  :init
  (setq xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function 'consult-xref))

;; ;; from https://karthinks.com/software/avy-can-do-anything/
(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)


(use-package embark
  :after avy
  :bind (("C-." . embark-act)
         ("C-h B" . embark-bindings))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))



(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ;; Completion frontend
(use-package corfu
  :config ;; (global-corfu-mode)
  (global-completion-preview-mode))

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
  ;; https://github.com/minad/corfu/issues/84
  ;; :config (setq slime-completion-at-point-functions (remove  'slime-c-p-c-completion-at-point slime-completion-at-point-functions))
  )

;; ;; pulses modified regions
(use-package goggles
  :hook (prog-mode . goggles-mode)
  :config (setq-default goggles-pulse t))

;; (defun flymake-eldoc-function (report-doc &rest _)
;;   "Document diagnostics at point.
;;    Intended for `eldoc-documentation-functions' (which see)."
;;   (let ((diags (flymake-diagnostics (point))))
;;     (when diags
;;       (funcall report-doc
;;                (mapconcat (lambda (d)
;;                             (let ((level (flymake-diagnostic-type d)))
;;                               (pcase level
;;                                 ('warning (propertize (flymake-diagnostic-text d) 'face 'flymake-warning))
;;                                 ('error (propertize (flymake-diagnostic-text d) 'face 'flymake-error))
;;                                 ('note (propertize (flymake-diagnostic-text d) 'face 'flymake-note))
;;                                 ('eglot-warning (propertize (flymake-diagnostic-text d) 'face 'flymake-warning))
;;                                 ('eglot-error (propertize (flymake-diagnostic-text d) 'face 'flymake-error))
;;                                 ('eglot-note (propertize (flymake-diagnostic-text d) 'face 'flymake-note))
;;                                 (_ (flymake-diagnostic-text d)))
;;                               )) diags "\n")))))


(use-package eglot
  :bind (("s-l c a" . eglot-code-actions)
         ("s-l r r" . eglot-rename)
         ("s-l g t" . eglot-find-typeDefinition))
  :config (add-to-list 'project-vc-extra-root-markers "tsconfig.json")
  (setq eglot-confirm-server-initiated-edits nil))

(use-package markdown-mode) ;; we need markdown mode for eglot's eldoc to render

(use-package eldoc
  :config
  (setq
   eldoc-echo-area-use-multiline-p t
   eldoc-echo-area-prefer-doc-buffer nil))

(use-package flymake
  :bind (("C-c C-f n" . flymake-goto-next-error)
         ("C-c C-f p" . flymake-goto-prev-error)
         ("C-c C-f l" . flymake-show-buffer-diagnostics)
         ("C-c C-f L" . flymake-show-project-diagnostics)
         ("s-e n" . flymake-goto-next-error)
         ("s-e p" . flymake-goto-prev-error)
         ("s-e l" . flymake-show-buffer-diagnostics)
         ("s-e L" . flymake-show-project-diagnostics)
         :repeat-map flymake-repeat-map
         ("n" . flymake-goto-next-error)
         ("p" . flymake-goto-prev-error)
         ("l" . flymake-show-buffer-diagnostics)
         ("L" . flymake-show-project-diagnostics)))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . expand-abbrev)
         ("C-M-/" . hippie-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package smerge-mode
  :ensure nil
  :bind (:map smerge-mode-map
              ("C-c C-s n" . smerge-next)
              ("C-c C-s p" . smerge-prev)
              ("C-c C-s r" . smerge-refine)
              ("C-c C-s a" . smerge-keep-all)
              ("C-c C-s l" . smerge-keep-lower)
              ("C-c C-s u" . smerge-keep-upper)))

(use-package iedit
  :bind ("C-c i" . iedit-mode))

(use-package sicp)

(use-package modus-themes)

;; dark themes: ("kaolin-aurora" "kaolin-blossom"  "kaolin-bubblegum" "kaolin-dark" "kaolin-eclipse" "kaolin-galaxy" "kaolin-mono-dark" "kaolin-ocean" "kaolin-shiva" "kaolin-temple" "kaolin-valley-dark")

(use-package kaolin-themes)
(use-package gruvbox-theme)

(defun julian/load-theme-after-frame (&optional frame)
  (load-theme 'gruvbox-dark-medium t)
  (set-face-attribute 'mode-line-buffer-id nil :inherit nil))

(add-to-list 'after-make-frame-functions #'julian/load-theme-after-frame)
(julian/load-theme-after-frame)

;; (use-package visual-regexp)
;; (use-package visual-regexp-steroids
;;   :after visual-regexp)

(use-package embrace
  :bind
  (("C-c C-," . embrace-commander)
   (:map org-mode-map
         ("C-c C-," . embrace-commander))))

(use-package bqn-mode)
(use-package protobuf-mode)

(use-package yaml-ts-mode :mode "\\.yml")

(use-package sideline-flymake :after sideline :config (setq sideline-flymake-display-mode 'line))
(use-package sideline-blame :after sideline)
(use-package sideline-eglot :after sideline)
(use-package sideline
  :config
  (setq sideline-backends-left '(sideline-flymake))
  (setq sideline-backends-right '(sideline-eglot))
  (global-sideline-mode -1))

(use-package forth-mode)

(use-package sqlformat)

(use-package transpose-frame
  :config (defvar julian/transpose-frame-keymap
            (define-keymap "t" #'transpose-frame
              "r" #'rotate-frame
              "h" #'flip-frame
              "v" #'flop-frame))
  :bind-keymap ("s-f" . julian/transpose-frame-keymap))


(use-package eat
  :hook ((eshell-first-time-mode . eat-eshell-mode)
         (eshell-first-time-mode . eat-eshell-visual-command-mode))

  ;; you can add `[ -x $(which fish) ] && SHELL=$(which fish) exec fish`
  ;; to your .posixshellrc file to make eat use fish
  :config (setq eat-term-name "xterm-256color"))

(use-package gnuplot)

(use-package terraform-mode)

(use-package wgrep)

(use-package fish-mode)

(use-package dape
  ;; Usage:
  ;; Run M-x dape (or C-x C-a d)
  ;; Make the prompt look like this:
  ;; js-debug-node-attach :remoteRoot "/usr/src/app" :localRoot "/Users/julian/Documents/Outcomes4me/api"

  ;; Make sure to be on a valid line when using breakpoints... trying to
  ;; breakpoint between lines doesn't seem to do anything.
  :config
  (setq dape-buffer-window-arrangement 'right))

(use-package eloud
  :bind ("s-s" . eloud-mode)
  :config
  (setq eloud-espeak-path "/opt/homebrew/bin/espeak"
        eloud-speech-rate 270))

(use-package jq-mode)
(use-package erlang)

(use-package devdocs)
(use-package focus)


(use-package server
  :ensure nil
  :defer t
  :config
  (unless (server-running-p)
    (server-start)))

(use-package form-feed-st
  :config (global-form-feed-st-mode))

(use-package uiua-mode)
(use-package glsl-mode)

(use-package dumber-jump
  :config
  (add-hook 'xref-backend-functions #'dumber-jump-xref-activate))

(use-package csv-mode)

(use-package linum-relative
  :config
  (setq linum-relative-backend 'display-line-numbers-mode)
  (linum-relative-global-mode))

(use-package hyperbole)
(use-package writeroom-mode
  :hook (info-mode . writeroom-mode)
  :config (setq
           writeroom-fullscreen-effect 'maximized))
(use-package emacs
  :config
  (setq dired-listing-switches "-lGgha")
  (tab-bar-history-mode 1)
  :bind  (("s-{" . tab-previous)
          ("s-}" . tab-next)
          ("s-]" . tab-line-switch-to-next-tab)
          ("s-[" . tab-line-switch-to-prev-tab)
          ("s-<" . previous-window-any-frame)
          ("s->" . next-window-any-frame)
          ("M-_" . undo-redo)
          :repeat-map tab-repeat-map
          ("o" . tab-next)
          ("O" . tab-previous)
          ("2" . tab-new)
          ("u" . tab-undo)
          ("0" . tab-close)))

(diminish 'auto-revert-mode)
(diminish 'sideline-mode)

;; Various Notes
;; Use C-x 4 4 to do next command in other window
;; Remember avy-dispatch and ace-window-dispatch exist
;; Embark exists!
;; Consult exists!
;; Tab bar history!!

(provide 'my-packages)
