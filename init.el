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


;; Dump custom-set-variables to a garbage file and don’t load it
(use-package cus-edit
  :ensure nil
  :config (setq custom-file (concat user-emacs-directory "to-be-dumped.el")))


;; Delete intermediate buffers when navigating through dired
(use-package dired
  :ensure nil
  :config
  (setq delete-by-moving-to-trash t)
  (eval-after-load "dired"
    #'(lambda ()
        (put 'dired-find-alternate-file 'disabled nil)
        (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file))))


;; Super charge Emacs' completion engine
(use-package ido
  :ensure nil
  :init (setq ido-enable-flex-matching t
              ido-everywhere t
              ido-auto-merge-work-directories-length -1
	      ido-create-new-buffer 'always)
  :config
  (ido-mode +1))


(use-package paren
  :ensure nil
  :init (setq show-paren-delay 0)
  :config (show-paren-mode +1))


;; Auto-insert matching parenthesis
(use-package elec-pair
  :ensure nil
  :config (electric-pair-mode t))

;; TODO -- use smartparens ???


(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function #'split-window-horizontally))




;; ------------------------------

;; external packages

;; (use-package markdown-mode
;;   :hook (markdown-mode . auto-fill-mode)
;;   :custom-face (markdown-code-face ((t (:inherit org-block)))))

;; (use-package json-mode)

;; (use-package yaml-mode)


;; ;; Editable and saveable grep buffer
;; (use-package wgrep)
