

;;==== PACKAGES =====================================================================================
;; Shut up package-initialize warning
(setq warning-suppress-log-types '((package reinitialization)))

;; Start Cask and load installed packages
(require 'cask "/usr/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)




;;==== VISUAL TWEAKS =================================================================================
;;# Basic visual settings
(menu-bar-mode 0)
(tool-bar-mode 0)
(set-scroll-bar-mode 'right)
(global-display-line-numbers-mode 1)
(setq truncate-lines 1)

;; Use Spacemacs dashboard instead of usual Emacs splash screen and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(require 'dashboard)
(dashboard-setup-startup-hook)

;; Load theme
(load-theme 'lush t)

;; Set font
(set-frame-font "Cascadia Mono Medium 11" nil t)

;; Enable TODO highlighting
(global-hl-todo-mode 1)

;; Enable rainbow delimiters whenever a file is opened (no global mode exists)
(add-hook 'find-file-hook 'rainbow-delimiters-mode 1)

;; Only show fringes on right edge of buffers
(fringe-mode '(0 . nil))

;; Variable font sizes for markdown headers
(custom-set-faces
 '(markdown-header-delimiter-face ((t (:inherit font-lock-function-name-face :underline t :weight bold))) t)
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.5))) t)
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.3))) t)
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :underline t :height 1.2))) t)
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :underline t :height 1.1))) t)
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :underline t))) t)
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :underline t))) t))




;;==== BEHAVIOUR TWEAKS ===============================================================================
;; Disable customize
(setq custom-file "/dev/null")

;; Disable backups and autosave files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Disable C-x C-c in GUI mode
(if window-system
    (global-unset-key (kbd "C-x C-c")))

;; Always follow symlinks rather than asking
(setq vc-follow-symlinks t)

;; Enable ido mode
(require 'ido)
(ido-mode t)

;; Enable dtrt-indent mode
(dtrt-indent-global-mode 1)

;; Set tab width to 4 spaces
(setq-default tab-width 4)

;; Disable arrow keys
(setq too-hardcore-backspace t)
(setq too-hardcore-return t)
(require 'hardcore-mode)
(global-hardcore-mode)

;; Trim trailing whitespaces
(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)

;; Set simple-httpd port
(setq httpd-port 8000)

;; Enable Discord rich presence when using GUI mode
(if window-system
    (progn
      (require 'elcord)
      (elcord-mode)))




;;==== TREE-SITTER ========================================================================================
;; Set list of tree-sitter grammar sources
(setq treesit-language-source-alist
      '(
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (make "https://github.com/alemuller/tree-sitter-make")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        ))

;; Define function to install all grammars automatically
(defun treesit-install-all-language-grammars ()
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

;; Remap major modes to tree-sitter equivalents
(setq major-mode-remap-alist
      '(
        (bash-mode . bash-ts-mode)
        (cmake-mode . cmake-ts-mode)
        (css-mode . css-ts-mode)
        (elisp-mode . elisp-ts-mode)
        (html-mode . html-ts-mode)
        (javascript-mode . js-ts-mode)
        (make-mode . make-ts-mode)
        (python-mode . python-ts-mode)
        (ruby-mode . ruby-ts-mode)
        ))
