(setq mode-line-bg-active "#585858")
(setq mode-line-bg-inactive "#B9B9B9")
(setq mode-line-fg-active "#afaf87") ;"#afaf87"
(setq mode-line-fg-inactive "#87875f")


;;; Alect customization
;;;------------------------------------------------------------
(defun puddle-light-alect-customization ()
  "alect-light customization options"

  ;; No background in terminal
  (if (not window-system)
      (alect-set-color 'light 'bg-1 "none"); Main background color
    )

  (alect-set-color 'light 'blue+1 "#4659a2")
  (alect-set-color 'light 'gray+1 "#555555")
  ;;(alect-set-color 'light 'yellow+1 "#d78765")
  (alect-set-color 'light 'yellow+2 "#ce724b")
  (alect-set-color 'light 'green-1 "#079530")
  (alect-set-color 'light 'green-2 "#678c6b")
  (alect-set-color 'light 'magenta "#af5082") ;"#bf6092"
  (alect-set-color 'light 'cyan-1 "#4e8c87")
  (alect-set-color 'light 'cyan-2 "#67878d")

  ;;"#67878d" ; blue-green low cont

  (setq
   alect-overriding-faces
   `(
     (font-lock-string-face  ((t :foreground cyan-1)))
     (font-lock-doc-face     ((t :inherit font-lock-comment-face)))
     (font-lock-comment-face ((t :foreground "#9a8e75"))) ; #7e8b71
     (font-lock-builtin-face ((t :foreground yellow+2)))
     (font-lock-variable-name-face ((t :foreground magenta)))
     (font-lock-function-name-face ((t :inherit font-lock-variable-name-face)))
     (font-lock-keyword-face ((t :foreground green-1)))
     (font-lock-constant-face ((t :foreground blue+1)))

     (linum ((t :foreground ,mode-line-fg-inactive
                :background ,mode-line-bg-inactive)))

     ;;(mode-line-buffer-id    ((t :foreground ,mode-line-fg-active)))
     (mode-line              ((((background light))
                               :foreground ,mode-line-fg-active
                               :background ,mode-line-bg-active
                               :box (:line-width 2 :color bg-2 :style nil))
                              (((background dark))
                               :foreground fg+1 :background "firebrick3"
                               :box (:line-width 2 :color bg-2 :style nil))))
     ))
  )

;;; Other customization
;;;------------------------------------------------------------
(defun face-inherit (srcFace, destFace)
  (set-face-attribute srcFace :inherit destFace))

(defun js2-face-settings ()
  (interactive)
  (set-face-foreground 'js2-function-param "#875faf")
  (set-face-foreground 'js2-external-variable "#d70000")
  (set-face-foreground 'js2-jsdoc-html-tag-delimiter "#9a8e75")
  (set-face-foreground 'js2-jsdoc-html-tag-name "#9a8e75")
  (set-face-foreground 'js2-jsdoc-tag "#9a8e75")
  (set-face-foreground 'js2-jsdoc-type "#9a8e75")
  (set-face-foreground 'js2-jsdoc-value "#9a8e75")
  ;;(set-face-attribute 'js2-jsdoc-html-tag-delimiter  nil
  ;;                    :inherit font-lock-comment-face)
  ;;(set-face-attribute 'js2-jsdoc-html-tag-name  nil
  ;;                    :inherit font-lock-comment-face)
  ;;(set-face-attribute 'js2-jsdoc-tag  nil
  ;;                    :inherit font-lock-comment-face)
  ;;(set-face-attribute 'js2-jsdoc-type  nil
  ;;                    :inherit font-lock-comment-face)
  ;;(set-face-attribute 'js2-jsdoc-value  nil
  ;;                    :inherit font-lock-comment-face)
  )

(defun powerline-face-settings ()

  (progn
    (set-face-attribute 'powerline-active1 nil
                    :background mode-line-bg-active
                    :foreground mode-line-fg-active)
    (set-face-attribute 'powerline-active2 nil
                    :background mode-line-bg-active
                    :foreground mode-line-fg-active)

    (set-face-attribute 'powerline-inactive1 nil
                    :background mode-line-bg-inactive
                    :foreground mode-line-fg-inactive)
    (set-face-attribute 'powerline-inactive2 nil
                    :background mode-line-bg-inactive
                    :foreground mode-line-fg-inactive)
    (set-face-attribute 'mode-line-inactive nil
                    :background mode-line-bg-inactive
                    :foreground mode-line-fg-inactive)
    (set-face-background 'mode-line-inactive mode-line-bg-inactive)
    )

  (setq spacemacs-mode-line-left
        '(
          ;;((workspace-number window-number)
          ;; :fallback state-tag :separator "|" :face state-face)
          anzu
          ;;(buffer-modified buffer-size buffer-id remote-host)
          (buffer-modified buffer-id remote-host)
          major-mode
          ((flycheck-errors flycheck-warnings flycheck-infos)
           :when active)
          ;;((minor-modes process)
          ;; :when active)
          (erc-track :when active)
          (org-pomodoro :when active)
          (org-clock :when active)
          (version-control :when active)
          ))
  (setq spacemacs-mode-line-right
        '(
          selection-info
          (battery :when active)
          ((point-position line-column)
           :separator " | ")
          ((global-mode new-version)
           :when active)
          buffer-position hud))
  )

(defun puddle-light-other-customization ()
  "Customization independent of alect-themes"
  (interactive)

  (defvar spacemacs-evil-cursor-colors
    '((normal . "red") ;"DarkGoldenrod2"
      (insert . "red") ;"chartreuse3"
      (emacs  . "red") ;"SkyBlue2"
      (evilified . "red") ;"red"
      (visual . "red") ;"gray"
      (motion . "red") ;"plum3"
      (lisp   . "red") ;"HotPink1"
      (iedit  . "red") ;"firebrick1"
      (iedit-insert  . "red")) ;"firebrick1"
    "Colors assigned to evil states.")

  (powerline-face-settings)
  (add-hook 'js2-mode-hook 'js2-face-settings))

;;; Custom theme
;;;------------------------------------------------------------
(defun puddle-light-theme ()
  "Loads 'puddle-light' theme, requires 'alect-themes' to be installed"
  (interactive)
  (load-theme 'alect-light)
  (puddle-light-alect-customization)
  (load-theme 'alect-light)
  (puddle-light-other-customization))
