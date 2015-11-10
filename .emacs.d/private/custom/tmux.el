;;; tmux.el --- Seamlessly navigate between Emacs and tmux

;; Author:   Keith Smiley <keithbsmiley@gmail.com>
;; Created:  April 25 2014
;; Version:  0.1.5
;; Keywords: tmux, evil, vi, vim

;;; Commentary:

;; This package is inspired by vim-tmux-navigator.
;; It allows you to navigate splits in evil mode
;; Along with tmux splits with the same commands
;; Include with:
;;
;;    (require 'navigate)
;;

;;; Code:

(require 'evil)

(defgroup navigate nil
  "seamlessly navigate between Emacs and tmux"
  :prefix "navigate-"
  :group 'evil)

(defcustom navigate-bind-on-evil-window-map nil
  "If nill, bind navigate keys on evil-normal-map and evil-motion-state-map.
Else, bind on evil-window-map."
  :type 'boolean
  :group 'navigate)
(defcustom navigate-pane-left-key (kbd "C-h")
  "Key binding used by navigate to switch to split on the left"
  :type 'string
  :group 'navigate)
(defcustom navigate-pane-down-key (kbd "C-j")
  "Key binding used by navigate to switch to split on the down"
  :type 'string
  :group 'navigate)
(defcustom navigate-pane-up-key (kbd "C-k")
  "Key binding used by navigate to switch to split on the up"
  :type 'string
  :group 'navigate)
(defcustom navigate-pane-right-key (kbd "C-l")
  "Key binding used by navigate to switch to split on the right"
  :type 'string
  :group 'navigate)

(setq-local bindings (list
                      navigate-pane-left-key
                      navigate-pane-down-key
                      navigate-pane-up-key
                      navigate-pane-right-key))

;; This requires windmove commands
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(defun tmux-navigate (direction)
  (let
      ((cmd (concat "windmove-" direction)))
    (condition-case nil
        (funcall (read cmd))
      (error
       (tmux-command direction)))))

(defun tmux-command (direction)
  (shell-command-to-string
   (concat "tmux select-pane -"
           (tmux-direction direction))))

(defun tmux-direction (direction)
  (upcase
   (substring direction 0 1)))

(if navigate-bind-on-evil-window-map
    (progn
      (define-key evil-window-map
        navigate-pane-left-key
        (lambda ()
          (interactive)
          (tmux-navigate "left")))
      (define-key evil-window-map
        navigate-pane-down-key
        (lambda ()
          (interactive)
          (tmux-navigate "down")))
      (define-key evil-window-map
        navigate-pane-up-key
        (lambda ()
          (interactive)
          (tmux-navigate "up")))
      (define-key evil-window-map
        navigate-pane-right-key
        (lambda ()
          (interactive)
          (tmux-navigate "right"))))
  (progn
    ;; Unset key-bindings to avoid conflicts
    (dolist (key bindings)
      (progn (print key)(global-unset-key key)))

    (define-key evil-normal-state-map
      navigate-pane-left-key
      (lambda ()
        (interactive)
        (tmux-navigate "left")))
    (define-key evil-normal-state-map
      navigate-pane-down-key
      (lambda ()
        (interactive)
        (tmux-navigate "down")))
    (define-key evil-normal-state-map
      navigate-pane-up-key
      (lambda ()
        (interactive)
        (tmux-navigate "up")))
    (define-key evil-normal-state-map
      navigate-pane-right-key
      (lambda ()
        (interactive)
        (tmux-navigate "right")))
    
    (define-key evil-motion-state-map
      navigate-pane-left-key
      (lambda ()
        (interactive)
        (tmux-navigate "left")))
    (define-key evil-motion-state-map
      navigate-pane-down-key
      (lambda ()
        (interactive)
        (tmux-navigate "down")))
    (define-key evil-motion-state-map
      navigate-pane-up-key
      (lambda ()
        (interactive)
        (tmux-navigate "up")))
    (define-key evil-motion-state-map
      navigate-pane-right-key
      (lambda ()
        (interactive)
        (tmux-navigate "right")))
    ))

(provide 'tmux)

;;; tmux.el ends here
