(use-package ollama-buddy
  :bind (("C-c o" . ollama-buddy-menu)))

(use-package erc
  :config
  (setq erc-server "irc.libera.chat"
        erc-port 6697
        erc-nick "themonkeybob11")
  (add-to-list 'erc-modules 'services)
  (setq erc-prompt-for-nickserv-password nil))

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

(use-package sqlformat)

(use-package eat
  :hook ((eshell-first-time-mode . eat-eshell-mode)
         (eshell-first-time-mode . eat-eshell-visual-command-mode))

  ;; you can add `[ -x $(which fish) ] && SHELL=$(which fish) exec fish`
  ;; to your .posixshellrc file to make eat use fish
  :config (setq eat-term-name "xterm-256color"))

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

(provide 'my-packages)
