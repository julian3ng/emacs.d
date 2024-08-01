(requie 'gamegrid)

(defconst eltris-buffer-name "*Eltris*")
(defconst eltris-tick 0.01)

(defconst eltris-buffer-width 10)
(defconst eltris-buffer-height 20)

(defun eltris-init-buffer ()
  (gamegrid-init-buffer eltris-buffer-width eltris-buffer-height 0)
  (let ((buffer-read-only nil))
    (dotimes (x eltris-buffer-width)
      (dotimes (y eltris-buffer-height)
        (when (= (mod (+ x y) 2) 0)
          (gamegrid-set-cell x y 1))))))

(defun eltris-reset ()
  (gamegrid-kill-timer)
  (eltris-init-buffer))

(defun eltris-update-game (buffer)
  (dotimes (x eltris-buffer-width)
    (dotimes (y eltris-buffer-height)
      (let ((cell (gamegrid-get-cell x y)))
        (gamegrid-set-cell x y (if (= cell 0) 1 0))))))

(defun eltris-start-game ()
  (interactive)
  (unless (string= (buffer-name (current-buffer)) eltris-buffer-name)
    (error "Ensure you are in the *Eltris* buffer"))
  (eltris-reset)
  (use-local-map eltris-game-map)
  (gamegrid-start-timer eltris-tick #'eltris-update-game))


(defun eltris-end-game ()
  (interactive)
  (message "End")
  (gamegrid-kill-timer)
  (kill-buffer eltris-buffer-name))

(defun eltris-move-left ()
  (interactive)
  (message "left"))

(defun eltris-move-right ()
  (interactive)
  (message "right"))

(defun eltris-soft-drop ()
  (interactive)
  (message "soft drop"))

(defun eltris-hold ()
  (interactive)
  (message "hold"))

(defun eltris-rotate-ccw ()
  (interactive)
  (message "ccw"))

(defun eltris-rotate-cw ()
  (interactive)
  (message "cw"))

(defun eltris-rotate-180 ()
  (interactive)
  (message "180"))

(defun eltris-hard-drop ()
  (interactive)
  (message "hard drop"))

(defvar eltris-game-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'eltris-end-game)
    (define-key map (kbd "j") #'eltris-move-left)
    (define-key map (kbd "l") #'eltris-move-right)
    (define-key map (kbd "k") #'eltris-soft-drop)
    (define-key map (kbd "i") #'eltris-hold)
    (define-key map (kbd "s") #'eltris-rotate-ccw)
    (define-key map (kbd "f") #'eltris-rotate-cw)
    (define-key map (kbd "d") #'eltris-rotate-180)
    (define-key map (kbd "e") #'eltris-hard-drop)
    (define-key map (kbd "r") #'eltris-start-game)
    map)
  "Eltris menu keymap")


(defvar eltris-null-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'bury-buffer)
    (define-key map (kbd "n") #'eltris-start-game)
    map)
  "Eltris menu keymap")


(defun eltris-display-options ()
  ""
  (let ((vec (make-vector 256 nil)))
    (dotimes (c 256)
      (aset
       vec c
       (cond
        ((= c 0)
         '(((t 32)) nil nil))
        ((= c 1)
         '(((t 65)) nil nil))
        (t
         '(nil nil nil)))))
    vec))


(define-derived-mode
  eltris-mode
  special-mode
  "Eltris"
  (add-hook 'kill-buffer-hook #'gamegrid-kill-timer nil t)
  (use-local-map eltris-null-map)
  (gamegrid-init (eltris-display-options)))

(defun eltris ()
  "ELTRIS"
  (interactive)
  (switch-to-buffer eltris-buffer-name)
  (gamegrid-kill-timer)
  (eltris-mode)
  (eltris-start-game))
