(use-package elpy
  :config (elpy-enable))

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
  :bind (("C-'" . avy-goto-char)
         ("C-M-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

(use-package beacon :init (beacon-mode) :diminish "")

(use-package company :diminish company-mode global-company-mode
  :init (add-hook 'after-init-hook 'global-company-mode))
(use-package slime-company)
(global-set-key (kbd "C-M-i") 'company-complete)

(use-package color-theme-modern )

(use-package counsel :bind (("M-x" . counsel-M-x)
                      ("C-x C-f" . counsel-find-file)
                      ("C-x 8 C-<return>" . counsel-unicode-char)))

(use-package smex)

(use-package diminish)
(use-package delight)
(use-package emmet-mode :diminish emmet-mode)

;; ERC
(use-package erc
  :delight "Ɛ "
  :custom
  (erc-autojoin-channels-alist nil)
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-password nil)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :config
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-services-mode 1)
  (erc-update-modules))

(defun my/erc-start-or-switch ()
  (interactive)
  (if (get-buffer "irc.freenode.net:6667")
      (erc-track-switch-buffer 1)
    (when (y-or-n-p "Start ERC?")
      (erc :server "irc.freenode.net" :port 6667 :nick "themonkeybob11"))))

(setq auth-sources '("~/.authinfo"  "~/.authinfo.gpg" "~/.netrc"))

(use-package erc-hl-nicks :after erc)
(use-package erc-image :after erc)

(use-package expand-region
  :commands expand-region
  :bind (("C-=" . er/expand-region)))

(use-package flycheck :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp
                                             emacs-lisp-checkdoc)
                flycheck-emacs-lisp-load-path 'inherit)
  :diminish global-flycheck-mode flycheck-mode)




(add-hook 'prog-mode-hook #'hs-minor-mode)
(diminish 'hs-minor-mode "")

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

(use-package org
  :bind (:map org-mode-map
              ("C-'" . nil))
  :config (setq org-catch-invisible-edits 'show-and-error
                org-hide-emphasis-markers t))

(require 'ob-C)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (python . t)))

(use-package paredit
  :diminish paredit-mode
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
  :config (projectile-mode t)
  :bind (:map projectile-mode-map
              ("s-p" . 'projectile-command-map)))

(use-package counsel-projectile
  :diminish counsel-projectile-mode
  :config (counsel-projectile-mode t)
  (setq counsel-projectile-switch-project-action 'counsel-projectile-switch-project-action-dired))

(use-package rainbow-delimiters :diminish ""
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode-enable))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package rust-mode)

(use-package racer
  :hook ((rust-mode . racer-mode)
         (rust-mode . eldoc-mode)))

(use-package flycheck-rust
  :hook (flycheck-mode-hook . flycheck-rust-setup))

(use-package restclient)

(use-package lsp-mode
  :hook (rust-mode . lsp)
  :commands lsp)

(use-package lsp-ui
  :hook (lsp-ui-mode . lsp-ui-mode)
  :config (setq lsp-ui-doc-enable nil
                lsp-prefer-flymake nil))

(require 'company-lsp)
(add-to-list 'company-backends 'company-lsp)

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"
        slime-lisp-implementations '((sbcl ("/usr/bin/sbcl")))
        slime-contribs '(slime-fancy))
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . common-lisp-mode)))

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
  :bind (("s-u" . undo-tree-visualize)))

(use-package which-key

  :diminish which-key-mode
  :init (which-key-mode)
  :config (which-key-setup-side-window-right-bottom))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (setq web-mode-engines-alist
        '("django" . "\\.html\\'")))

(use-package yasnippet
  :commands yas-global-mode
  :config
  (yas-global-mode 1))

(use-package common-lisp-snippets)

(provide 'my-packages)
