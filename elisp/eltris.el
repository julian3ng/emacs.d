(require 'gamegrid)

(defconst eltris-buffer-name "*Eltris*")
(defconst eltris-tick 0.5)

(defconst eltris-buffer-width 10)
(defconst eltris-buffer-height 20)

(defun eltris-init-buffer ()
  (gamegrid-init-buffer eltris-bufer-width eltris-buffer-height 0)
  (let ((buffer-read-only nil))))

(defun eltris-reset ()
  (gamegrid-kill-timer)
  (eltris-init-buffer))


(defun eltris-start-game ()
  (interactive)
  (unless (string= (buffer-name (current-buffer)) eltris-buffer-name)
    (error "Ensure you are in the *Eltris* buffer"))

  (eltris-reset)
  (use-local-map eltris-game-map)
  (gamegrid-start-timer eltris-tick #'eltris-update-game))

(defvar eltris-null-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'bury-buffer)
    (define-key map (kbd "n") #'eltris-start-game)
    map)
  "Eltris menu keymap")


(defun eltris-display-options ()
  (let ((vec (make-vector 256 nil)))
    (dotimes (c 256)
      (aset vec c
            (cond
             ((= c 0) '(((t 32)) nil nil))
             (t '(nil nil nil)))))
    vec))


(define-derived-mode eltris-mode special-mode "Eltris"
  (add-hook 'kill-buffer-hook #'gamegrid-kill-timer nil t)
  (use-local-map eltris-null-map)
  (gamegrid-init (eltris-display-options)))

(defun eltris-start-game ())

(defun eltris ()
  "ELTRIS"
  (interactive)
  (switch-to-buffer eltris-buffer-name)
  (gamegrid-kill-timer)
  (eltris-mode)
  (eltris-start-game)
  )
