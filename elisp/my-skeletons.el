(setq skeleton-end-newline nil)
(define-skeleton julian/transaction-thunk
  "Insert transaction thunk"
  nil
  "async (transaction) => {"
  _
  "}")

(define-skeleton julian/console-log
  "Insert console.log"
  nil
  "console.log(" _ ");")

; console.log('\x1b[m', JSON.stringify(test, null, 4), '\x1b[m');

(define-skeleton julian/notify
  "Insert iterm notification log"
  nil
  "'\\x1b]9;', " _ ", '\\x07'")

(define-skeleton julian/json
  "Insert console.log(JSON.stringify(..., null, 4))"
  nil
  "JSON.stringify(" _ ", null, 4)")


(define-skeleton julian/color
  "Insert console color code"
  nil
  "'\\x1b[" str "m', " _ ", '\\x1b[m'")


(defun julian/make-console-log (r1 r2)
  (interactive "r")
  (let* ((args (transient-args transient-current-command))
         (red (transient-arg-value "--red" args))
         (green (transient-arg-value "--green" args))
         (blue (transient-arg-value "--blue" args))
         (bright (transient-arg-value "--bright" args))
         (json (transient-arg-value "--json" args))
         (notification (transient-arg-value "--notification" args))
         (color (when (or red green blue) (+ (if bright 90 30) (* 4 (if blue 1 0)) (* 2 (if green 1 0)) (if red 1 0)))))
    ;; TODO: Better way of managing region?
    (kill-region r1 r2)
    (let ((skeletons nil)
          (mark-active nil))
      (when json (push #'julian/json skeletons))
      (when notification (push #'julian/notify skeletons))
      (when (not (null color)) (push #'(lambda () (julian/color (number-to-string color))) skeletons))
      (push #'julian/console-log skeletons)
      (dolist (s skeletons)
        (funcall s)))
    (yank)))

(transient-define-prefix julian/console ()
  "Infix test"
  :incompatible '(("--notification" "--red")
                  ("--notification" "--green")
                  ("--notification" "--blue")
                  ("--notification" "--bright"))
  ["Infixes"
   ("!" "bright" ("-!" "--bright"))
   ("r" "red" ("-r" "--red"))
   ("g" "green" ("-g" "--green"))
   ("b" "blue" ("-b" "--blue"))
   ("j" "JSON" "--json")
   ("n" "Notification" "--notification")]
  [("RET" "confirm" julian/make-console-log :transient nil)])

(transient-define-prefix julian/main-menu ()
  "Julian's main menu"
  [("c" "console.log" julian/console :transient nil)
])

(global-set-key (kbd "s-j") #'julian/main-menu)

(provide 'my-skeletons)


