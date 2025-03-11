;; things like org, howm, hyperbole

;; Elfeed
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

;; org
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

(use-package ob-C
  :ensure nil
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (python . t)
     (ruby . t))))

(use-package howm
  :init (require 'howm-org)
  (setq howm-directory "~/howm/")
  (setq howm-file-name-format "%Y/%m/%d/%Y-%m-%d-%H%M%S.org")
  (setq howm-keyword-file (expand-file-name ".howm-keys" howm-directory))
  (setq howm-history-file (expand-file-name ".howm-history" howm-directory))
  (setq howm-view-use-grep t)
  (setq howm-view-grep-command nil))

(use-package hyperbole)
(use-package sicp)
(use-package devdocs)

(provide 'my-knowledge)
