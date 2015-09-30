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
;; map J 5j
(define-key evil-normal-state-map (kbd "J") (lambda ()
                                              (interactive) (evil-next-line 5)))
;; map K 5k
(define-key evil-normal-state-map (kbd "K") (lambda ()
                                              (interactive) (evil-previous-line 5)))
;; map <C-k> J
(define-key evil-normal-state-map (kbd "C-k") 'evil-join)
;; map ; :
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
