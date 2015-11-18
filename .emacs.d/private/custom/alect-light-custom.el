(alect-set-color 'light 'bg-1 "none")
(alect-set-color 'light 'blue+1 "#2639E2")

(setq
 alect-overriding-faces
 '(
   (font-lock-string-face  ((t :foreground green-2)))
   (font-lock-doc-face     ((t :inherit font-lock-string-face)))
   (font-lock-comment-face ((t :foreground gray)))
   (font-lock-function-name-face ((t :foreground yellow+2)))
   (font-lock-variable-name-face ((t :foreground yellow+2)))
   (font-lock-builtin-face ((t :foreground magenta)))
   (font-lock-keyword-face ((t :foreground green-1)))
   (font-lock-constant-face ((t :foreground blue+1)))

   (linum ((t :foreground gray+1 :background gray-1)))

   (mode-line-buffer-id    ((t :foreground gray-2)))
   (mode-line              ((((background light))
                             :foreground gray-1 :background gray+1
                             :box (:line-width 2 :color bg-2 :style nil))
                            (((background dark))
                             :foreground fg+1 :background "firebrick3"
                             :box (:line-width 2 :color bg-2 :style nil))))
   ))
