

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

;; Disable splash screen and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Load theme
(load-theme 'lush t)

;; Set font
(set-frame-font "Cascadia Mono Medium 11" nil t)



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

;; Enable TODO highlighting
(global-hl-todo-mode 1)

;; Enable rainbow delimiters whenever a file is opened (no global mode exists)
(add-hook 'find-file-hook 'rainbow-delimiters-mode 1)

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
