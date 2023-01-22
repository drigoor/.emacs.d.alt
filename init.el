;;
;; references:
;;
;;    https://github.com/ianyepan/yay-evil-emacs/blob/master/config.org
;;       https://ianyepan.github.io/posts/setting-up-use-package/
;;
;;    https://gitlab.com/hineios/dotfiles

;; Package configs
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t))

(use-package emacs
  :ensure nil
  :preface
  (defvar ian/indent-width 4) ; change this value to your preferred width
  ;; from: https://github.com/magnars/.emacs.d/blob/master/settings/appearance.el
  ;; from: https://www.emacswiki.org/emacs/AlarmBell
  (defun flash-mode-line ()
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil #'invert-face 'mode-line))
  :config
  (setq frame-title-format '("emacs")
        ring-bell-function 'flash-mode-line
        visible-bell nil
        frame-resize-pixelwise t
        default-directory "~/")

  (menu-bar-mode -1)
  (tool-bar-mode -1)

  (fset 'yes-or-no-p 'y-or-n-p)

  ;; better scrolling experience
  (setq scroll-margin 0
        scroll-conservatively 101       ; > 100
        scroll-preserve-screen-position t
        auto-window-vscroll nil)

  ;; Always use spaces for indentation
  (setq-default indent-tabs-mode nil
                tab-width ian/indent-width
                show-trailing-whitespace t
                truncate-lines t)

  (setq-default line-spacing 0.15)

  (setq inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message t
        initial-scratch-message nil)

  (setq echo-keystrokes 0.1
        mode-line-percent-position "")

  (add-to-list 'default-frame-alist '(left . 800))
  (add-to-list 'default-frame-alist '(top . 180))
  (add-to-list 'default-frame-alist '(width . (text-pixels . 1024)))
  (add-to-list 'default-frame-alist '(height . (text-pixels . 1024))))

(use-package frame
  :preface
  (defun ian/set-default-font ()
    (interactive)
    (when (member "Consolas" (font-family-list))
      (set-face-attribute 'default nil :family "Consolas"))
    (set-face-attribute 'default nil :height 100 :weight 'normal))
  :ensure nil
  :config
  (ian/set-default-font)

  (blink-cursor-mode -1)
  (set-background-color "#fefefc")

  (set-face-attribute 'mode-line nil
                      :height 1.0
                      :foreground (face-foreground 'default)
                      :foreground "#605e57"
                      :background "#f2ecdb"
                      :overline nil
                      :underline nil
                      :box `(:line-width 3 :color ,"#f0e0d0" :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :height 1.0
                      :foreground "#aba9a7"
                      :background "#faf8f4"
                      :overline nil
                      :underline nil
                      :inherit nil
                      :box `(:line-width 3 :color ,"#f5f2ef" :style nil)))

(use-package hl-line
  :ensure nil
  :config (global-hl-line-mode +1))

(use-package tooltip
  :ensure nil
  :config (tooltip-mode -1))

(use-package delsel
  :ensure nil
  :config (delete-selection-mode +1))

(use-package scroll-bar
  :ensure nil
  :config (scroll-bar-mode -1))

(use-package simple
  :ensure nil
  :config (column-number-mode +1))

(use-package files
  :ensure nil
  :config
  (setq make-backup-files nil)
  (setq-default require-final-newline t))

(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode +1) ; revert buffers automatically when underlying files are changed externally
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        global-auto-revert-non-file-buffers t))

(use-package windmove
  :ensure nil
  :config
  (windmove-default-keybindings 'M)) ; use Meta + arrow keys to switch between visible buffers

(use-package mwheel
  :ensure nil
  :config (setq mouse-wheel-scroll-amount '(7 ((shift) . 1) ((meta) . hscroll) ((control) . text-scale))
                mouse-wheel-progressive-speed nil))

(use-package paren
  :ensure nil
  :init (setq show-paren-delay 0)
  :config (show-paren-mode +1))

(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function #'split-window-horizontally))

;; Auto-insert matching parenthesis
(use-package elec-pair
  :ensure nil
  :config (electric-pair-mode t))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

;; Delete intermediate buffers when navigating through dired
(use-package dired
  :ensure nil
  :config
  (setq delete-by-moving-to-trash t)
  (eval-after-load "dired"
    #'(lambda ()
        (put 'dired-find-alternate-file 'disabled nil)
        (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file))))

;; Dump custom-set-variables to a garbage file and don't load it
(use-package cus-edit
  :ensure nil
  :config (setq custom-file (concat user-emacs-directory "custom.el")))

;; Super charge Emacs' completion engine
(use-package ido
  :ensure nil
  :init (setq ido-enable-flex-matching t
              ido-everywhere t
              ido-auto-merge-work-directories-length -1
              ido-create-new-buffer 'always)
  :config
  (ido-mode +1))

;; -- global keys --------------------------------------------------------------

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "C-S-<tab>") (lambda () (interactive) (other-window -1)))

;; scroll
(global-set-key "\M-i" "\C-u1\M-v")
(global-set-key "\M-k" "\C-u1\C-v")

;; -----------------------------------------------------------------------------

(use-package ahk-mode
  :config
  (setq ahk-indentation 2))

(use-package crux
  :bind
  (("C-a"        . crux-move-beginning-of-line)
   ("C-S-k"      . crux-kill-whole-line)
   ("C-c k"      . crux-kill-other-buffers)
   ("C-M-z"      . crux-indent-defun)
   ("C-c d"      . crux-duplicate-current-line-or-region)
   ("C-c M-d"    . crux-duplicate-and-comment-current-line-or-region)
   ("C-c r"      . crux-rename-buffer-and-file)
   ([(M return)] . crux-smart-open-line)))

(use-package anzu
  :bind
  (("<remap> <query-replace>" . 'anzu-query-replace)
   ("<remap> <query-replace-regexp>" . 'anzu-query-replace-regexp))
  :config
  (global-anzu-mode t)
  (set-face-foreground 'anzu-mode-line "#FF6F00"))

;; -- magit --------------------------------------------------------------------

(defun magit-status-around (orig-fun &rest args)
  (window-configuration-to-register 'x)
  (delete-other-windows)
  (apply orig-fun args))

;; references:
;; https://github.com/bradwright/emacs-d/blob/master/packages/init-magit.el
;; http://whattheemacsd.com/setup-magit.el-01.html
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-c b" . magit-blame)
         :map magit-status-mode-map
         ("q" . magit-quit-session))
  :config
  (advice-add 'magit-status :around #'magit-status-around) ; check: https://www.gnu.org/software/emacs/manual/html_node/elisp/Porting-old-advice.html
  (defun magit-quit-session ()
    (interactive)
    (kill-buffer)
    (jump-to-register 'x))
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))


;; -- lisp ---------------------------------------------------------------------

(use-package sly
  :commands (sly sly-connect)
  :config (setq inferior-lisp-program (expand-file-name "C:/home/scoop/apps/sbcl/current/sbcl.exe")))

;; -------------------------------------

;; from: https://www.john2x.com/emacs.html

(defconst elispy-modes
  '(emacs-lisp-mode ielm-mode)
  "Emacs major modes.")

(defconst lispy-modes
  (append elispy-modes
          '(lisp-mode inferior-lisp-mode lisp-interaction-mode clojure-mode cider-mode-hook cider-repl-mode-hook))
  "All lispy major modes.")

(defun maybe-check-parens ()
  "Run `check-parens' if this is a lispy mode."
  (when (memq major-mode lispy-modes)
    (check-parens)))

(add-hook 'after-save-hook 'maybe-check-parens)
