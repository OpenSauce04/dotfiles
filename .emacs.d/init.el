;;=== INIT ===========================================================================================
;; Workaround for https://trac.macports.org/ticket/71528
(when (eq system-type 'darwin)
  (setq native-comp-enable-subr-trampolines nil))


;;==== PACKAGES =====================================================================================
;; Shut up package-initialize warning
(setq warning-suppress-log-types '((package reinitialization)))

;; Start Cask and load installed packages
(if (eq system-type 'gnu/linux)
  (require 'cask "/usr/share/emacs/site-lisp/cask/cask.el")
;;else
  (require 'cask "~/.emacs.d/lisp/cask/cask.el"))
(cask--initialize)


;;==== VISUAL TWEAKS =================================================================================
;;# Basic self-explainitory visual settings
(menu-bar-mode 0)
(tool-bar-mode 0)
(spacious-padding-mode 1)
(blink-cursor-mode 0)

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

;; Conditional is a workaround MacPorts Emacs BS - Do not run the following if in terminal mode on MacOS
(when (or (not (eq system-type 'darwin)) (window-system))
  ;; Disable built-in scrollbars
  (scroll-bar-mode 0)
  ;; Only show fringes on right edge of buffers
  (add-hook 'server-after-make-frame-hook (lambda () (fringe-mode '(0 . nil)))))

;; Use sixcolors scrollbar
(setq sixcolors-colors '("#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF" ))
(sixcolors-mode 1)

;; Enable current line highlighting for prog-mode modes
(add-hook 'prog-mode-hook (lambda () (hl-line-mode 1)))

;; Enable subtle mode line appearance option provided by spacious-padding
(setq spacious-padding-subtle-mode-line 1)

;; Enable line numbers for non-eww modes. TODO: Do this better
(add-hook 'text-mode-hook 'display-line-numbers-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode 1)

;; Use Spacemacs dashboard instead of usual Emacs splash screen and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(require 'dashboard)
(setq dashboard-startup-banner 'logo)
(setq dashboard-items '((recents . 15)))
(dashboard-setup-startup-hook)
(if (daemonp)
    (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))))

;; Load theme
(load-theme 'lush t)

;; Set font style and size (larger font on MacOS)
(if (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(font . "Cascadia Mono Medium 14"))
;;else
  (add-to-list 'default-frame-alist '(font . "Cascadia Mono Medium 11")))

;; Show current line git blame info
(global-blamer-mode 1)

;; Enable TODO highlighting
(global-hl-todo-mode 1)

;; Enable rainbow delimiters whenever a file is opened (no global mode exists)
(add-hook 'find-file-hook 'rainbow-delimiters-mode 1)

;; Only wrap on word boundaries
(setq-default word-wrap t)

;; Customize wc-mode string format
(setq wc-modeline-format "wordcount:%tw")
;; Enable wc-mode when opening text files
(add-hook 'text-mode-hook 'wc-mode)

;; Make native-comp warnings shut up
(setq native-comp-async-report-warnings-errors nil)

;; Variable font sizes for markdown headers
(custom-set-faces
 '(markdown-header-delimiter-face ((t (:inherit font-lock-function-name-face :underline t :weight bold))) t)
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.5))) t)
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.3))) t)
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :underline t :height 1.2))) t)
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :underline t :height 1.1))) t)
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :underline t))) t)
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :underline t))) t))

;; Disable bell (not really visual but shh)
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Force a redraw of entire Emacs window every second on ARM Linux to circumvent Asahi graphical issues
(if (string-match "aarch64-unknown-linux-gnu" system-configuration)
    (run-with-timer 0 3
                    (lambda ()
                      (if (window-system)
                          (redraw-display)))))

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

;; Disable C-x C-c in GUI mode
(when window-system
  (global-unset-key (kbd "C-x C-c")))
;; Disable C-z
(global-unset-key (kbd "C-z"))

;; Always follow symlinks rather than asking
(setq vc-follow-symlinks t)

;; Enable ido mode
(require 'ido)
(ido-mode t)

;; Enable clickable URLs
(global-goto-address-mode)

;; Disable electric indent
(electric-indent-mode 0)

;; Never insert tabs into indentation
(setq-default indent-tabs-mode nil)

;; Enable dtrt-indent mode
(dtrt-indent-global-mode 1)

;; Per-language indent tweaks
(setq js-indent-level 2)

;; Set tab width to 4 spaces
(setq-default tab-width 4)

;; Per-pixel scrolling
;;(pixel-scroll-precision-mode 1)

;; Always split buffers vertically
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Add padding to bottom and top of screen for scrolling with the cursor
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
;; Kind-of workaround/fallback for smooth-scrolling sometimes failing
(setq scroll-conservatively most-positive-fixnum)

;; Disable arrow keys
(setq too-hardcore-backspace t)
(setq too-hardcore-return nil)
(require 'hardcore-mode)
(global-hardcore-mode)
;; Don't use hardcore-mode in minibuffers or certain modes
(add-hook 'minibuffer-setup-hook (lambda () (hardcore-mode 0)))
(add-hook 'minibuffer-exit-hook (lambda () (hardcore-mode 1)))
(add-hook '2048-mode-hook (lambda () (hardcore-mode 0)))
(add-hook 'dashboard-mode-hook (lambda () (hardcore-mode 0)))
(add-hook 'eww-mode-hook (lambda () (hardcore-mode 0)))
(add-hook 'ripgrep-search-mode-hook (lambda () (hardcore-mode 0)))
(add-hook 'snake-mode-hook (lambda () (hardcore-mode 0)))
(add-hook 'tetris-mode-hook (lambda () (hardcore-mode 0)))

;; Register `rg` as a shortcut for ripgrep
(defun rg ()
  (interactive)
  (call-interactively 'ripgrep-regexp))

;; Use pdf-tools instead of the built-in DocView for viewing PDFs
(when (not (eq system-type 'darwin))
  (pdf-tools-install)  ; Standard activation command
  (pdf-loader-install)) ; On demand loading, leads to faster startup time

;; Automatically activate spell checking in Markdown files
(add-hook 'markdown-mode-hook (lambda () (jinx-mode 1)))

;; Trim trailing whitespaces
(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)

;; Set simple-httpd port
(setq httpd-port 8000)

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


;;==== TREE-SITTER ========================================================================================
;; Set list of tree-sitter grammar sources
(setq treesit-language-source-alist
      '(
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
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
        (kotlin-mode . kotlin-ts-mode)
        (make-mode . make-ts-mode)
        (python-mode . python-ts-mode)
        (ruby-mode . ruby-ts-mode)))
