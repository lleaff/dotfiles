;;;####################################################################
;;; better-defaults.el --- Fixing weird quirks and poor defaults
;; Copyright © 2013 Phil Hagelberg and contributors
;; Author: Phil Hagelberg
;; URL: https://github.com/technomancy/better-defaults
;; Version: 0.1.2
;; Created: 2013-04-16
;;; License: GPLv3

;;;###autoload
(progn
  (ido-mode t)
  (setq ido-enable-flex-matching t)

  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  (autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR." t)

  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

  (require 'saveplace)
  (setq-default save-place t)

  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "M-z") 'zap-up-to-char)

  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-M-s") 'isearch-forward)
  (global-set-key (kbd "C-M-r") 'isearch-backward)

  (show-paren-mode 1)
  (setq-default indent-tabs-mode nil) ; Deactivate Tab
  (setq x-select-enable-clipboard t
	x-select-enable-primary t
	save-interprogram-paste-before-kill t
	apropos-do-all t
	mouse-yank-at-point t
	require-final-newline t
	visible-bell t
	load-prefer-newer t
	ediff-window-setup-function 'ediff-setup-windows-plain
	save-place-file (concat user-emacs-directory "places")
	backup-directory-alist `(("." . ,(concat user-emacs-directory
						 "backups")))))

(provide 'better-defaults)
;;; better-defaults.el ends here

;;;####################################################################
;;;  =Packages
;;;####################################################################

;; Enable package repositories
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

; activate all the packages (in particular autoloads)
(package-initialize)

(require 'company) ; TODO install (use-package?)
(add-hook 'after-init-hook 'global-company-mode)


;;;####################################################################
;;;  =Settings
;;;####################################################################

(setq ring-bell-function 'ignore) ; No alarm bells

(linum-mode) ; Display line numbers
(setq linum-format "%d ") ; Takes one integer argument, defaults to "%d"

;; Mouse support in terminal
(unless window-system ; Only if in terminal
  (require 'mouse)
  (xterm-mouse-mode t) ; Basic mouse interaction
  (defun track-mouse (e))
  (setq mouse-sel-mode t) ; Allow selecting text with mouse
)

(setq-default tab-width 4) ; tab -> 4 spaces
(delete-selection-mode) ; Replace selected text when typing

(global-set-key (kbd "RET") 'newline-and-indent) ; Automatically indent when pressing return
