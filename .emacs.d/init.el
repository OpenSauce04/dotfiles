

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

;; Disable backups
(setq make-backup-files nil)

;; Enable ido mode
(require 'ido)
(ido-mode t)

;; Enable TODO highlighting
(global-hl-todo-mode 1)

;; Enable rainbow delimiters whenever a file is opened (no global mode exists)
(add-hook 'find-file-hook 'rainbow-delimiters-mode 1)