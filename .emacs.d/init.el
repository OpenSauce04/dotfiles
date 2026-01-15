;; -*- lexical-binding: t; -*-

;;==== PACKAGES =====================================================================================

;; Initialize package loading
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;; Load packages
(use-package 2048-game)
(use-package blamer)
(use-package browse-kill-ring)
(use-package cmake-mode)
(use-package crystal-mode)
(use-package dockerfile-mode)
(use-package dtrt-indent)
(use-package elcord)
(use-package git-gutter)
(use-package hl-todo)
(use-package jinx)
(use-package kotlin-mode)
(use-package lua-mode)
(use-package lush-theme)
(use-package markdown-mode)
(use-package markdown-preview-mode)
(use-package package-lint)
(use-package perfect-margin)
(use-package portage-modes)
(use-package rainbow-delimiters)
(use-package ripgrep)
(use-package shut-up)
(use-package sixcolors-mode)
(use-package smooth-scrolling)
(use-package typit)
(use-package vterm)
(use-package w3m)
(use-package wc-mode)
(use-package ws-butler)
(use-package yaml-mode)

;;==== VISUAL TWEAKS =================================================================================
;;# Basic self-explainitory visual settings
(menu-bar-mode 0)
(blink-cursor-mode 0)
(line-number-mode 1)
(column-number-mode 1)

;; Show git info of lines on side of screen
(global-git-gutter-mode 1)
(custom-set-variables
 '(git-gutter:update-interval 1)
 '(git-gutter:modified-sign " ")
 '(git-gutter:added-sign " ")
 '(git-gutter:deleted-sign " "))
(set-face-background 'git-gutter:modified "dark magenta")
(set-face-background 'git-gutter:added "dark green")
(set-face-background 'git-gutter:deleted "dark red")

;; Show in-progress key sequences with no delay
(setq echo-keystrokes 0.001)

;; Use sixcolors scrollbar
(setq sixcolors-colors '("#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF" ))
(sixcolors-mode 1)

;; Enable current line highlighting for prog-mode modes
(add-hook 'prog-mode-hook (lambda () (hl-line-mode 1)))

;; Save the recent files list after any file (non fundamental mode) is opened
;; TODO: Is there a better hook for this?
(recentf-mode 1)
(add-hook 'after-change-major-mode-hook
          (lambda () (shut-up
                       (recentf-save-list))))

;; Load theme
(load-theme 'lush t)

;; Enable TODO highlighting
(global-hl-todo-mode 1)

;; Enable rainbow delimiters whenever a file is opened (no global mode exists)
(add-hook 'find-file-hook 'rainbow-delimiters-mode 1)

;; Only wrap on word boundaries
(setq-default word-wrap t)

;; Customize wc-mode string format
(setq wc-modeline-format "charcount:%tc wordcount:%tw")
;; Enable wc-mode when opening text files
(add-hook 'text-mode-hook 'wc-mode)

;; Enable Discord rich presence
(require 'elcord)
(setq elcord-quiet t) ;; Shut up connection failure messages
(setq elcord-display-buffer-details nil) ;; Don't show file details in status
(elcord-mode)

;; Better Markdown header styling
(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :underline t :weight bold))) t)
 '(markdown-header-delimiter-face ((t (:inherit markdown-header-face))) t))

;; Disable bell (not really visual but shh)
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Configure markdown-preview-mode
(setq markdown-preview-stylesheets
      (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.9.0/github-markdown.min.css"
            "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css" "
  <style>
   .markdown-body {
     box-sizing: border-box;
     min-width: 200px;
     max-width: 980px;
     margin: 0 auto;
     padding: 45px;
   }

   @media (max-width: 767px) {
     .markdown-body {
       padding: 15px;
     }
   }
  </style>
"))
(setq markdown-preview-javascript
      (list "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js" "
  <script>
   $(document).on('mdContentChange', function() {
     $('pre code').each(function(i, block) {
       hljs.highlightBlock(block);
     });
   });
  </script>
"))


;;==== BEHAVIOUR TWEAKS ===============================================================================
;; Disable customize
(setq custom-file "/dev/null")

;; Disable backups, autosave, and lock files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Disable Emacs startup message
(setq inhibit-startup-message t)

;; Make scratch a blank fundamental-mode buffer
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "")

;; Disable C-z
(global-unset-key (kbd "C-z"))

;; Always follow symlinks rather than asking
(setq vc-follow-symlinks t)

;; Enable ido mode
(require 'ido)
(ido-mode t)

;; Disable electric indent
(electric-indent-mode 0)

;; Never insert tabs into indentation
(setq-default indent-tabs-mode nil)

;; Enable dtrt-indent mode
(dtrt-indent-global-mode 1)

;; Set tab width to 4 spaces
(setq-default tab-width 4)

;; Always split buffers vertically
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Add padding to bottom and top of screen for scrolling with the cursor
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
;; Kind-of workaround/fallback for smooth-scrolling sometimes failing
(setq scroll-conservatively most-positive-fixnum)

;; Register `rg` as a shortcut for ripgrep
(defun rg ()
  (interactive)
  (call-interactively 'ripgrep-regexp))

;; Trim trailing whitespaces
(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)

;; ERC config
(require 'erc)
(add-to-list 'erc-modules 'scrolltobottom)
(setq erc-server "irc.libera.chat"
      erc-port 6697
      erc-nick "opensauce04"
      erc-autojoin-channels-alist
      '(("libera.chat"
         "#archlinux"
         "#emacs-social"
         "#netbsd")))
(setq erc-fill-function 'erc-fill-wrap
         erc-fill-static-center nil)
(erc-log-mode)
(setq erc-log-channels-directory "~/.emacs.d/erc-logs/")
(make-directory erc-log-channels-directory t)
(setq erc-save-buffer-on-part nil)
(setq erc-save-queries-on-quit nil)
(setq erc-log-write-after-insert t)
(setq erc-log-write-after-send t)

;; Better C-Backspace and C-Delete behaviour
(defun my-backward-kill-spaces-or-char-or-word ()
  (interactive)
  (cond
    ((looking-back (rx (char word)) 1)
      (backward-kill-word 1))
    ((looking-back (rx (char blank)) 1)
      (delete-horizontal-space t))
    (t
      (backward-delete-char 1))))
(global-set-key (kbd "<C-backspace>") 'my-backward-kill-spaces-or-char-or-word)

(defun my-forward-kill-spaces-or-char-or-word ()
  (interactive)
  (cond
    ((looking-at (rx (char word)) 1)
      (kill-word 1))
    ((looking-at (rx (char blank)) 1)
      (delete-horizontal-space))
    (t
      (delete-forward-char 1))))
(global-set-key (kbd "<C-delete>") 'my-forward-kill-spaces-or-char-or-word)

;; Map C-h to act like C-backspace (for terminal compatibility)
(keymap-global-set "C-h" 'my-backward-kill-spaces-or-char-or-word)

;; Assign automatic modes for file extensions where needed
(add-to-list 'auto-mode-alist '("\\.go\\'" . prog-mode))

;;==== TREE-SITTER ========================================================================================
;; Set list of tree-sitter grammar sources
(setq treesit-language-source-alist
      '(
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (make "https://github.com/alemuller/tree-sitter-make")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")))

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
        (javascript-mode . js-ts-mode)
        (make-mode . make-ts-mode)
        (python-mode . python-ts-mode)
        (ruby-mode . ruby-ts-mode)))
