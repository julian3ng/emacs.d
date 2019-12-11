(use-package elpy :ensure t
  :config (elpy-enable))

(use-package ace-window :ensure t :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-dispatch-always nil)
  :bind
  ("C-x o" . ace-window))

(use-package all-the-icons :ensure t)
(use-package all-the-icons-ivy :ensure t
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-file)
  (all-the-icons-ivy-setup))

(use-package avy :ensure t
  :bind (("C-'" . avy-goto-char)
         ("C-M-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

(use-package beacon :ensure t :init (beacon-mode) :diminish "")

(use-package company :ensure t :diminish company-mode global-company-mode
  :init (add-hook 'after-init-hook 'global-company-mode))
(use-package slime-company :ensure t)
(global-set-key (kbd "C-M-i") 'company-complete)

(use-package color-theme-modern :ensure t )

(use-package counsel :ensure t :bind (("M-x" . counsel-M-x)
                      ("C-x C-f" . counsel-find-file)
                      ("C-x 8 C-<return>" . counsel-unicode-char)))

(use-package smex :ensure t)

(use-package diminish :ensure t)
(use-package delight :ensure t)
(use-package emmet-mode :ensure t :diminish emmet-mode)

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

(use-package flycheck :ensure t :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp
                                             emacs-lisp-checkdoc)
                flycheck-emacs-lisp-load-path 'inherit)
         :diminish global-flycheck-mode flycheck-mode)

(add-hook 'prog-mode-hook #'hs-minor-mode)
(diminish 'hs-minor-mode "")
(use-package hideshowvis :ensure t
  :config
  (add-hook 'prog-mode-hook #'hideshowvis-enable)
)

(use-package ivy :ensure t :diminish ivy-mode
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-initial-inputs-alist nil)
    (setq ivy-re-builders-alist
          '((t . ivy--regex-ignore-order)))
    (setq ivy-count-format "%d/%d ")))

(use-package magit :ensure t :bind (("C-x g" . magit-status)))

(use-package org :ensure t
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
  :ensure t
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

(use-package projectile :ensure t
  :diminish projectile-mode
  :config (projectile-mode t)
  :bind (:map projectile-mode-map
              ("s-p" . 'projectile-command-map)))

(use-package counsel-projectile :ensure t
  :diminish counsel-projectile-mode
  :config (counsel-projectile-mode t))

(use-package rainbow-delimiters :ensure t :diminish ""
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode-enable))

(use-package cargo :ensure t)
(use-package rust-mode :ensure t
  :config

  (add-hook 'rust-mode-hook #'cargo-minor-mode))
(use-package racer :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))
(use-package flycheck-rust
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package restclient :ensure t)

(use-package lsp-mode
  :ensure t
  :hook (rust-mode . lsp)
  :commands lsp)

(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package company-lsp :ensure t :commands company-lsp)

(use-package slime
  :ensure t
  :config
    (setq inferior-lisp-program "sbcl"
          slime-lisp-implementations '((sbcl ("/usr/bin/sbcl"))))
    (slime-setup '(slime-fancy))
    (add-to-list 'auto-mode-alist '("\\.lisp\\'" . common-lisp-mode)))

(use-package undo-tree
  :ensure t
  :diminish ""
  :init (progn (global-undo-tree-mode) (setq undo-tree-visualizer-timestamps t))
  :bind (("s-u" . undo-tree-visualize)))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode)
  :config (which-key-setup-side-window-right-bottom))

(use-package web-mode :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (setq web-mode-engines-alist
        '("django" . "\\.html\\'")))

(provide 'my-packages)
