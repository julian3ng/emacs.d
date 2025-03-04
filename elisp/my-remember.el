(setq remember-notes-initial-major-mode 'org-mode
      remember-in-new-frame t)

(global-set-key (kbd "C-c C-j") 'remember)
(global-set-key (kbd "C-c M-j") 'remember-notes)

(provide 'my-remember)
