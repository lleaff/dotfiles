;;; Alect customization
;;;------------------------------------------------------------

;; No background in terminal
(if (not window-system)
    (alect-set-color 'light 'bg-1 "none"); Main background color
  )

(alect-set-color 'light 'blue+1 "#4659a2")
(alect-set-color 'light 'gray+1 "#555555")
;(alect-set-color 'light 'yellow+1 "#d78765")
(alect-set-color 'light 'yellow+2 "#ce724b")
(alect-set-color 'light 'green-1 "#079530")
(alect-set-color 'light 'green-2 "#678c6b")
(alect-set-color 'light 'magenta "#ba7082")
(alect-set-color 'light 'cyan-1 "#4e8c87")
(alect-set-color 'light 'cyan-2 "#67878d")

;;"#67878d" ; blue-green low cont

;;(setq fg-alt "#878769")

(setq
 alect-overriding-faces
 '(
   (font-lock-string-face  ((t :foreground cyan-1)))
   (font-lock-doc-face     ((t :inherit font-lock-comment-face)))
   (font-lock-comment-face ((t :foreground "#9a8e75"))) ; #7e8b71
   (font-lock-builtin-face ((t :foreground yellow+2)))
   (font-lock-variable-name-face ((t :foreground magenta)))
   (font-lock-function-name-face ((t :inherit font-lock-variable-name-face)))
   (font-lock-keyword-face ((t :foreground green-1)))
   (font-lock-constant-face ((t :foreground blue+1)))

   (linum ((t :foreground "#878769" :background gray-1)))

   (mode-line-buffer-id    ((t :foreground "#878769")))
   (mode-line              ((((background light))
                             :foreground "#878769" :background gray
                             :box (:line-width 2 :color bg-2 :style nil))
                            (((background dark))
                             :foreground fg+1 :background "firebrick3"
                             :box (:line-width 2 :color bg-2 :style nil))))
   ))

;;; Other customization
;;;------------------------------------------------------------

(defvar spacemacs-evil-cursor-colors
  '((normal . "DarkGoldenrod2")
    (insert . "chartreuse3")
    (emacs  . "SkyBlue2")
    (evilified . "LightGoldenrod3")
    (visual . "gray")
    (motion . "plum3")
    (lisp   . "HotPink1")
    (iedit  . "firebrick1")
    (iedit-insert  . "firebrick1"))
  "Colors assigned to evil states.")
