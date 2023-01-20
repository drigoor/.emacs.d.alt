;;
;; references:
;;
;;    https://github.com/ianyepan/yay-evil-emacs/blob/master/config.org
;;       https://ianyepan.github.io/posts/setting-up-use-package/
;;
;;    https://gitlab.com/hineios/dotfiles



(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))


(use-package emacs
  :ensure nil
  :preface
  (defvar ian/indent-width 4) ; change this value to your preferred width
  :config
  (setq frame-title-format '("emacs")
        ring-bell-function 'ignore
        frame-resize-pixelwise t
        default-directory "~/")

  (tool-bar-mode -1)
  (menu-bar-mode -1)

  ;; better scrolling experience
  (setq scroll-margin 0
        scroll-conservatively 101 ; > 100
        scroll-preserve-screen-position t
        auto-window-vscroll nil)

  ;; Always use spaces for indentation
  (setq-default indent-tabs-mode nil
                tab-width ian/indent-width)
  (setq inhibit-startup-screen t))


;; The Emacs default split doesn't seem too intuitive for most users.
(use-package emacs
  :ensure nil
  :preface
  (defun ian/split-and-follow-horizontally ()
    "Split window below."
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun ian/split-and-follow-vertically ()
    "Split window right."
    (interactive)
    (split-window-right)
    (other-window 1))
  :config
  (global-set-key (kbd "C-x 2") #'ian/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") #'ian/split-and-follow-vertically))

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
  (setq ;; confirm-kill-processes nil
        ;; create-lockfiles nil ; don't create .# files (crashes 'npm start')
        make-backup-files nil))

(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode +1)
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        global-auto-revert-non-file-buffers t
        ;; auto-revert-verbose nil
        ))

(use-package mwheel
  :ensure nil
  :config (setq ;; mouse-wheel-scroll-amount '(2 ((shift) . 1))
                mouse-wheel-progressive-speed nil))

(use-package paren
  :ensure nil
  :init (setq show-paren-delay 0)
  :config (show-paren-mode +1))

(use-package frame
  :preface
  (defun ian/set-default-font ()
    (interactive)
    (when (member "Consolas" (font-family-list))
      (set-face-attribute 'default nil :family "Consolas"))
    (set-face-attribute 'default nil
                        :height 100
                        :weight 'normal))
  :ensure nil
  :config
  (setq initial-frame-alist '((fullscreen . maximized)))
  (ian/set-default-font))

(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function #'split-window-horizontally))

;; Auto-insert matching parenthesis
(use-package elec-pair
  :ensure nil
  :config (electric-pair-mode t))

;; (use-package elec-pair
;;   :ensure nil
;;   :hook (prog-mode . electric-pair-mode))

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














;; TODO -- use smartparens ???



;; ------------------------------

;; external packages ---------------------------------------------------

;; (use-package company
;;   :diminish company-mode
;;   :hook (prog-mode . company-mode)
;;   :config
;;   (setq company-minimum-prefix-length 1
;;         company-idle-delay 0.1
;;         company-selection-wrap-around t
;;         company-tooltip-align-annotations t
;;         company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
;;                             company-echo-metadata-frontend))
;;   (define-key company-active-map (kbd "C-n") 'company-select-next)
;;   (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; (use-package flycheck :config (global-flycheck-mode +1))

;; (use-package org
;;   :hook ((org-mode . visual-line-mode)
;;          (org-mode . org-indent-mode)))

;; (use-package org-bullets :hook (org-mode . org-bullets-mode))

;; (use-package markdown-mode
;;   :hook (markdown-mode . auto-fill-mode)
;;   :custom-face (markdown-code-face ((t (:inherit org-block)))))

;; (use-package web-mode
;;   :mode (("\\.html?\\'" . web-mode)
;;          ("\\.css\\'"   . web-mode)
;;          ("\\.jsx?\\'"  . web-mode)
;;          ("\\.tsx?\\'"  . web-mode)
;;          ("\\.json\\'"  . web-mode))
;;   :config
;;   (setq web-mode-markup-indent-offset 2) ; HTML
;;   (setq web-mode-css-indent-offset 2)    ; CSS
;;   (setq web-mode-code-indent-offset 2)   ; JS/JSX/TS/TSX
;;   (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

;; (use-package json-mode)

;; (use-package yaml-mode)


;; ;; Editable and saveable grep buffer
;; (use-package wgrep)



;; (use-package highlight-numbers
;;   :hook (prog-mode . highlight-numbers-mode))

;; (use-package highlight-escape-sequences
;;   :hook (prog-mode . hes-mode))



;; (use-package magit
;;   :bind ("C-x g" . magit-status)
;;   ;; :config (add-hook 'with-editor-mode-hook #'evil-insert-state)
;;   )


;; (use-package dashboard
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-startup-banner 'logo
;;         dashboard-banner-logo-title " . ~ e m a c s ~ ."
;;         dashboard-items nil
;;         dashboard-set-footer nil))


;; (use-package diminish
;;   :demand t)

;; (use-package which-key
;;   :diminish which-key-mode
;;   :config
;;   (which-key-mode +1)
;;   (setq which-key-idle-delay 0.4
;;         which-key-idle-secondary-delay 0.4))

;; (use-package exec-path-from-shell
;;   :config (when (memq window-system '(mac ns x))
;;             (exec-path-from-shell-initialize)))
