;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   ;; Press <SPC f e h> to display available layers.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;;; Syntax
     auto-completion
     syntax-checking
     ;;; Other
     better-defaults
     dash
     git
     ;; org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     ;; version-control
     ;;; Cosmetic
     colors
     themes-megapack
     ;;; Languages
     html
     emacs-lisp
     markdown
     c-c++
     php
     python
     javascript
     haskell
     prolog
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages
   '(
     editorconfig
     )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         alect-light
                         soft-stone
                         oldlace
                         sunny-day
                         alect-light-alt
                         ample-light
                         anti-zenburn
                         aproprospriate-light
                         flatui
                         organic-green
                         sanityinc-solarized-light
                         spacemacs-dark
                         soft-morning
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."

  (add-to-list 'load-path "~/.emacs.d/private/custom") ; Random .el files

  (setq evil-toggle-key "C-`") ; Default is C-z

  ;; =My defaults
  (setq-default
   c-basic-offset 2 ; Default indent level

   ;; js2-mode
   js2-basic-offset 2
   js2-include-node-externs t ; Node.js syntax
   js2-skip-preprocessor-directives t ; Treat lines beginning with # as comments
   js2-strict-trailing-comma-warning nil ; Don't warn about trailing commas (bugs in old IE versions)

   ;; web-mode
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2

   python-indent-offset 2

   ;; Requires 'eslint' to be installed ($> npm i -g eslint)
   flycheck-eslintrc ".eslintrc"
   )
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  ;;; Functions
  ;;;------------------------------------------------------------

  (defun add-to-mode (mode lst)
    "Add filenames to mode, eg: (add-to-mode 'gam-mode '(\".gamrc\" \".gamport\"))"
    (dolist (file lst)
      (add-to-list 'auto-mode-alist
                   (cons file mode))))


  (defun append-to-list (listToModify toAppend)
    "Append a list to another list, Like 'append but modifies the list in place instead of creating a copy"
    (dolist (element toAppend)
      (add-to-list listToModify element)))

  ;;;------------------------------------------------------------

  (add-to-list 'load-path "~/.emacs.d/private/custom")

  (require 'evil-custom-keybinds)

  ;;(defun load-file-s (filename)
  ;;  "Load file if it exists"
  ;;  (interactive "P\nfFile to load: ")
  ;;  (when (file-exists-p filename)
  ;;                        (load-file filename))
  ;;  )

  ;;; Cosmetic
  ;;;------------------------------------------------------------
  (global-hl-line-mode -1) ; -1 to deactivate current line highlight

  (global-linum-mode t) ; Activate line numbers left of text
  (setq linum-format "%3d ") ; Default line number format: "%d"
  (set-face-background 'linum "#B9B9B9")
  ;; Deactivate sp-pair-overlay (highlights the inside of new parenthesis pairs)
  (setq sp-highlight-pair-overlay nil) 

  ;; Custom theme
  (require 'puddle-light)
  (puddle-light-theme)

  ;;; Code highlighting
  (auto-highlight-symbol-mode t)

  ;;;------------------------------------------------------------

  ;; Personal headers
  (setq-local loginname-file "~/.emacs.d/private/custom/loginname.el")
  (if (file-readable-p loginname-file)
      (require 'loginname)
    (require 'std_comment))

  (add-hook 'php-mode-hook 'php-my-settings)
  (defun php-my-settings ()
    (setq c-basic-offset 2)
    )

  (add-hook 'prog-mode-hook 'prog-my-settings)
  (defun prog-my-settings ()
    (setq c-basic-offset 2)
    (spacemacs/toggle-fill-column-indicator-on)
    )

  (defun js-node-mode ()
    "Initialize Node.js syntax checking and completion"
    (interactive)
    (progn
      (setq js2-include-browser-externs nil)
      (setq js2-include-node-externs t)
      js2-mode))

  (defun js-browser-mode ()
    "Initialize WebAPI syntax checking and completion"
    (interactive)
    (progn
      (setq js2-include-browser-externs t)
      (setq js2-include-node-externs nil)
      js2-mode))

  (add-hook 'web-mode-hook 'web-my-settings)
  (defun web-my-settings ()
    (spacemacs/toggle-fill-column-indicator-off) ; "on" makes web-mode buggy
    )

  ;;;;; Tern.js
  ;;;; Load
  ;; (add-to-list 'load-path "/usr/lib/node_modules/tern/emacs")
  ;; (autoload 'tern-mode "tern.el" nil t)
  ;; (add-hook 'js-mode-hook (lambda () (tern-mode t)))
  ;; (eval-after-load 'tern
  ;;   '(progn
  ;;      (require 'tern-auto-complete)
  ;;      (tern-ac-setup)))
  ;; Reset Tern (to reload .tern-project resolution)
  (defun kill-tern-process ()
    (interactive)
    (delete-process "Tern"))

  ;;; Restore cursor position ;TODO Doesnt work
  ;; Turn on save place so that when opening a file, the cursor will be at the last position.
  (require 'saveplace)
  (setq save-place-file (concat user-emacs-directory "saveplace.el") ) ; use standard emacs dir
  (setq-default save-place t)


  (add-to-mode 'json-mode (list
                           ".eslintrc"
                           ".tern-project"))

  ;; Load company (an auto-completion package) on JSON files
  (add-hook 'json-mode-hook (lambda () (company-mode t)))

  ;; Disable evil mode in specific modes
  (append-to-list 'evil-emacs-state-modes '(haskell-interactive-mode))

  (define-key evil-emacs-state-map (kbd "C-u") (lambda ()
                                                 (interactive)
                                                 (haskell-interactive-mode-beginning)
                                                 (kill-line)))
  (define-key evil-emacs-state-map (kbd "C-p") 'haskell-interactive-mode-history-previous)
  (define-key evil-emacs-state-map (kbd "C-n") 'haskell-interactive-mode-history-next)
  (define-key evil-emacs-state-map (kbd "C-h") 'delete-backward-char)

  (require 'evil-tmux-navigator)
  (setq-default
   evil-tmux-navigator-bind-on-evil-window-map nil
   evil-tmux-navigator-pane-left-key  (kbd "C-M-z h")
   evil-tmux-navigator-pane-down-key  (kbd "C-M-z j")
   evil-tmux-navigator-pane-up-key    (kbd "C-M-z k")
   evil-tmux-navigator-pane-right-key (kbd "C-M-z l"))
  (evil-tmux-navigator-bind-keys)

  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(package-selected-packages
   (quote
    (web-beautify mmm-mode markdown-toc markdown-mode json-mode js2-refactor js2-mode js-doc gh-md company-tern coffee-mode phpunit phpcbf php-auto-yasnippets drupal-mode pyvenv pytest pyenv-mode pip-requirements hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode ample-zen-theme zeal-at-point helm-dash rainbow-mode rainbow-identifiers disaster company-c-headers cmake-mode clang-format helm-c-yasnippet flycheck-pos-tip flycheck company-statistics company-quickhelp company auto-yasnippet ac-ispell alect-themes window-numbering which-key volatile-highlights vi-tilde-fringe use-package spray spacemacs-theme smooth-scrolling rainbow-delimiters quelpa powerline popwin popup pcre2el paradox page-break-lines open-junk-file neotree move-text macrostep linum-relative leuven-theme info+ indent-guide ido-vertical-mode hungry-delete highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-jumper evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu elisp-slime-nav define-word clean-aindent-mode buffer-move auto-highlight-symbol auto-dictionary aggressive-indent adaptive-wrap ace-window ace-link))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
