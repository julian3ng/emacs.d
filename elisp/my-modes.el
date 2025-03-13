;; extra modes for other languages

(use-package auctex :defer t)

;; HTML/CSS expansion
(use-package emmet-mode :diminish emmet-mode)

;; Ruby
(use-package robe)

(use-package lua-mode)
(add-to-list 'load-path "~/.emacs.d/pico8-mode/")
(when (file-exists-p "~/.emacs.d/pico8-mode/")
  (require 'pico8-mode))

;; CL
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

(use-package geiser)
(use-package geiser-chicken)

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (setq web-mode-engines-alist '(("php" . "\\.php\\'")))
  (setq web-mode-enable-auto-indentation nil))

(use-package markdown-mode) ;; we need markdown mode for eglot's eldoc to render

(use-package bqn-mode)
(use-package protobuf-mode)

(use-package yaml-ts-mode :mode "\\.yml")
(use-package forth-mode)
(use-package terraform-mode)
(use-package fish-mode)
(use-package jq-mode)
(use-package erlang)
(use-package uiua-mode)
(use-package glsl-mode)
(use-package csv-mode)
(use-package gdscript-mode)
(use-package dictionary)
(use-package gnuplot)

(provide 'my-modes)
