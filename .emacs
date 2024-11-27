;; Disable customize
(setq custom-file "/dev/null")

;; Basic visual settings
(menu-bar-mode 0)
(tool-bar-mode 0)
(set-scroll-bar-mode 'right)
(global-display-line-numbers-mode 1)
(setq truncate-lines 1)

;; Disable splash screen and startup message
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

;; Load theme
(load-theme 'tango-dark)

;; Enable ido mode
(require 'ido)
(ido-mode t)
