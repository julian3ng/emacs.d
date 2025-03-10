;; autocomplete, abbrev, dictionaries, etc.

(setq-default dabbrev-case-fold-search nil )

(setq completion-auto-help 'always)
(setq completions-format 'one-column)
(setq completion-auto-select 'second-tab)
(setq tab-always-indent 'complete)
(setq dictionary-server "dict.org")

;; Completion frontend
(use-package corfu :config (global-corfu-mode))
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

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . expand-abbrev)
         ("C-M-/" . hippie-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))


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

;; from https://karthinks.com/software/avy-can-do-anything/
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
  (setf (alist-get ?. avy-dispatch-alist) #'avy-action-embark))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'my-autocomplete)
