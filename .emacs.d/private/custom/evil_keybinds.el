;------------------------------------------------------------
;; Exit insert mode by pressing k and then j
(defun evil-escape-if-next-char (c)
  "Watches the next letter.  If c, then switch to evil insert mode, otherwise insert a j and forward unpressed key to unread-command-events"
  (self-insert-command 1)
  (let ((next-key (read-event)))
    (if (= c next-key)
        (progn
          (delete-backward-char 1)
          (evil-normal-state))
      (setq unread-command-events (list next-key)))))

(defun evil-escape-if-next-char-is-j (arg)
  (interactive "p")
  (if (= arg 1)
      (evil-escape-if-next-char ?j)
    (self-insert-command arg)))

(define-key evil-insert-state-map (kbd "k") 'evil-escape-if-next-char-is-j)
;------------------------------------------------------------

(define-key evil-insert-state-map (kbd "J") (progn
                                              (evil-line-move up)
                                              (evil-repeat 4)))
(define-key evil-insert-state-map (kbd "K") "5k")
