(use-package ollama-buddy
  :bind (("C-c o" . ollama-buddy-menu)))

(use-package erc
  :config
  (setq erc-server "irc.libera.chat"
        erc-port 6697
        erc-nick "themonkeybob11")
  (add-to-list 'erc-modules 'services)
  (setq erc-prompt-for-nickserv-password nil))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(use-package ace-window
  :after hyperbole
  :demand t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-dispatch-always nil)
  (set-face-foreground 'aw-mode-line-face "#f0f")
  (ace-window-display-mode t)
  :bind
  (("M-o" . ace-window)
   ("C-x o" . other-window)
   :map hyperbole-mode-map
   ("M-o" . ace-window)))

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

(use-package dictionary)

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

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

(use-package paredit
  :diminish paredit-mode
  :hook ((lisp-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode))
  :bind (:map paredit-mode-map ("M-d" . paredit-forward-kill-word))
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

(use-package eglot
  :bind (("s-l c a" . eglot-code-actions)
         ("s-l r r" . eglot-rename)
         ("s-l g t" . eglot-find-typeDefinition))
  :config (add-to-list 'project-vc-extra-root-markers "tsconfig.json")
  (setq eglot-confirm-server-initiated-edits nil))

(use-package eldoc
  :config
  (setq
   eldoc-echo-area-use-multiline-p t
   eldoc-echo-area-prefer-doc-buffer nil))

(use-package flymake
  :bind (("C-c ! n" . flymake-goto-next-error)
         ("C-c ! p" . flymake-goto-prev-error)
         ("C-c ! l" . flymake-show-buffer-diagnostics)
         ("C-c ! L" . flymake-show-project-diagnostics)
         ("s-e n" . flymake-goto-next-error)
         ("s-e p" . flymake-goto-prev-error)
         ("s-e l" . flymake-show-buffer-diagnostics)
         ("s-e L" . flymake-show-project-diagnostics)
         :repeat-map flymake-repeat-map
         ("n" . flymake-goto-next-error)
         ("p" . flymake-goto-prev-error)
         ("l" . flymake-show-buffer-diagnostics)
         ("L" . flymake-show-project-diagnostics)))

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

(use-package embrace
  :bind
  (("C-c C-," . embrace-commander)
   (:map org-mode-map
         ("C-c C-," . embrace-commander))))

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

(use-package wgrep)

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

(use-package server
  :ensure nil
  :defer t
  :config
  (unless (server-running-p)
    (server-start)))

(use-package form-feed-st
  :config (global-form-feed-st-mode))

(use-package dumber-jump
  :config
  (add-hook 'xref-backend-functions #'dumber-jump-xref-activate))

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
