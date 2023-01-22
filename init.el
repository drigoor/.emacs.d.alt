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
  (defun my-window-setup ()
    (setq default-frame-alist '((undecorated . t)))
    (add-to-list 'default-frame-alist '(left . 830))
    (add-to-list 'default-frame-alist '(top . 50))
    (add-to-list 'default-frame-alist '(width . (text-pixels . 1680)))
    (add-to-list 'default-frame-alist '(height . (text-pixels . 1324)))
    (add-to-list 'default-frame-alist '(drag-internal-border . +1))
    (add-to-list 'default-frame-alist '(internal-border-width . 10))
    (set-face-background 'internal-border "#faf8f4")
    (setq window-divider-default-right-width 10
          window-divider-default-places 'right-only)
    (window-divider-mode +1)
    (set-face-foreground 'window-divider "#faf8f4")
    (set-face-foreground 'window-divider-first-pixel "#f5f2ef")
    (set-face-foreground 'window-divider-last-pixel "#f5f2ef"))
  :hook (emacs-startup . my-window-setup)
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
        mode-line-percent-position ""))

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

(use-package fringe
  :ensure nil
  :config (set-fringe-mode 10))

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
  (set-background-color "#fefefc"))

(use-package faces
  :ensure nil
  :config
  (set-face-background 'cursor "orange")
  (set-face-background 'region "#ffffcc")

  (set-face-background 'fringe (face-background 'default))

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
  :config (windmove-default-keybindings 'M)) ; use Meta + arrow keys to switch between visible buffers

(use-package mwheel
  :ensure nil
  :config
  (setq mouse-wheel-scroll-amount '(7 ((shift) . 1) ((meta) . hscroll) ((control) . text-scale))
        mouse-wheel-progressive-speed nil))

(use-package paren
  :ensure nil
  :init (setq show-paren-delay 0)
  :config
  (setq show-paren-style 'expression)
  (show-paren-mode +1))

(use-package ediff
  :ensure nil
  :init (add-to-list 'exec-path "c:/home/scoop/apps/git/current/usr/bin")
  :config
  ;; from: https://emacs.stackexchange.com/questions/7482/restoring-windows-and-layout-after-an-ediff-session/7486

  (defvar ediff-last-windows nil
    "Last ediff window configuration.")

  (defun ediff-restore-windows ()
    "Restore window configuration to `ediff-last-windows'."
    (set-window-configuration ediff-last-windows)
    (remove-hook 'ediff-after-quit-hook-internal 'ediff-restore-windows))

  (defun ediff-save-windows ()
    "Save window configuration to `ediff-last-windows'."
    (setq ediff-last-windows (current-window-configuration))
    (add-hook 'ediff-after-quit-hook-internal 'ediff-restore-windows))

  (defadvice ediff-files (around ediff-restore-windows activate)
    (ediff-save-windows)
    ad-do-it)

  (defadvice ediff-buffers (around ediff-restore-windows activate)
    (ediff-save-windows)
    ad-do-it)

  (defadvice ediff-regions-linewise (around ediff-restore-windows activate)
    (ediff-save-windows)
    ad-do-it)

  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally))

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

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*")) ; don't muck with special buffers

(use-package grep
  :ensure nil
  :bind
  (("C-S-s" . extra-gitgrep)
   ("C-M-S-s" . extra-gitgrep-with-comments))
  :config
    (defvar extra-gitgrep-default-git-repo nil
    "If a git repository is not found, this specifies where to search by default.")

  (defvar extra-gitgrep-file-extensions "*.lisp *.cl"
    "Default file extensions to search.")

  (defvar extra-gitgrep-default-comment-string ";"
    "Default string for comments.")

  (defun extra-gitgrep-command (search-in-comments-p)
    (let ((dir (or (vc-root-dir)
                   extra-gitgrep-default-git-repo
                   default-directory)))
      (let ((regexp (grep-read-regexp)))
        (when (and (stringp regexp) (> (length regexp) 0))
          (let ((command (grep-expand-template "git --no-pager grep -n -i -e <R> -- <F>"
                                               (if search-in-comments-p
                                                   regexp
                                                 (format "^[^%s]*%s" extra-gitgrep-default-comment-string regexp))
                                               extra-gitgrep-file-extensions)))
            (when command
              (add-to-history 'grep-history command)
              (let ((default-directory dir)
                    (compilation-environment (cons "PAGER=" compilation-environment)))
                ;; Setting process-setup-function makes exit-message-function work
                ;; even when async processes aren't supported.
                (compilation-start command 'grep-mode))
              (when (eq next-error-last-buffer (current-buffer))
                (setq default-directory dir))))))))

  (defun extra-gitgrep ()
    (interactive)
    (extra-gitgrep-command nil)
    (select-window (get-buffer-window "*grep*")))

  (defun extra-gitgrep-with-comments ()
    (interactive)
    (extra-gitgrep-command t)
    (select-window (get-buffer-window "*grep*")))

  (setq extra-gitgrep-file-extensions "*.lisp *.cl"))

;; -----------------------------------------------------------------------------

(use-package expand-region ; from: https://susamn.medium.com/ultimate-emacs-setup-with-documentation-in-org-mode-8ed32e2b3487
  :ensure t
  :bind ("M-m" . er/expand-region))

(use-package minions ; from: https://susamn.medium.com/ultimate-emacs-setup-with-documentation-in-org-mode-8ed32e2b3487
  :config
  (setq minions-mode-line-lighter ""
        minions-mode-line-delimiters '("" . ""))
  (global-set-key [S-down-mouse-3] 'minions-minor-modes-menu)
  (minions-mode +1))

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

(use-package corfu
  :bind
  (:map corfu-map
        ("TAB" . corfu-complete)
        ("<tab>" . corfu-complete))
  :custom
  (corfu-auto t) ;; Enable auto completion
  (corfu-preview-current 'insert)
  (corfu-preselect-first t)
  (corfu-count 14)
  (corfu-scroll-margin 0)
  (corfu-min-width 30)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  (setq completion-cycle-threshold 3) ; TAB cycle if there are only few candidates
  (setq tab-always-indent 'complete))

;; -- magit --------------------------------------------------------------------

;; references:
;; https://github.com/bradwright/emacs-d/blob/master/packages/init-magit.el
;; http://whattheemacsd.com/setup-magit.el-01.html
(use-package magit
  :preface
  (defun magit-status-around (orig-fun &rest args)
    (window-configuration-to-register 'x)
    (delete-other-windows)
    (apply orig-fun args))
  (defun magit-quit-session ()
    (interactive)
    (kill-buffer)
    (jump-to-register 'x))
  :bind
  (("C-x g" . magit-status)
   ("C-c b" . magit-blame)
   :map magit-status-mode-map
   ("q" . magit-quit-session))
  :config
  (advice-add 'magit-status :around #'magit-status-around) ; check: https://www.gnu.org/software/emacs/manual/html_node/elisp/Porting-old-advice.html
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))

;; from: https://github.com/munen/emacs.d/
(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

;; -- lisp ---------------------------------------------------------------------

(use-package sly
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

;; -- custom code --------------------------------------------------------------

(defun open-file (path)
  `(lambda ()
     (interactive)
     (find-file ,path)))

(defun my-just-one-space()
  (interactive "*")
  (just-one-space -1))

;; from: https://gitorious.org/gnu-emacs-config/mainline/blobs/a3fe6e69d9a752ef094448bfdf1794ce39916f4d/dotemacs.el
(defun comment-or-uncomment-region-or-line ()
  "Like comment-or-uncomment-region, but if there's no mark \(that means no 194 region\) apply comment-or-uncomment to the current line"
  (interactive)
  (if (not mark-active)
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (if (< (point) (mark))
        (comment-or-uncomment-region (point) (mark))
      (comment-or-uncomment-region (mark) (point)))))

;; from: http://sachachua.com/blog/2008/07/emacs-keyboard-shortcuts-for-navigating-code/
(defun sacha/isearch-yank-current-word ()
  "Pull current word from buffer into search string."
  (interactive)
  (save-excursion
    (skip-syntax-backward "w_")
    (isearch-yank-internal
     (lambda ()
       (skip-syntax-forward "w_")
       (point)))))

;; modified to display message
(defun sacha/search-word-forward ()
  "Find the next occurrance of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-forward "w_")
    (let ((text (with-current-buffer (current-buffer)
                  (buffer-substring-no-properties cur (point)))))
      (goto-char
       (cond ((re-search-forward (concat "\\_<" (regexp-quote (current-word)) "\\_>") nil t)
              (message "found forward: %s" text)
              (match-beginning 0))
             (t
              (message "'%s' not found forward in buffer" text)
              cur))))))

;; modified to display message
(defun sacha/search-word-backward ()
  "Find the previous occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-backward "w_")
    (let* ((found? (re-search-backward (concat "\\_<" (regexp-quote (current-word)) "\\_>") nil t))
           (pt2 (save-excursion
                  (re-search-forward (concat "\\_<" (regexp-quote (current-word)) "\\_>") nil t)
                  (point)))
           (text (with-current-buffer (current-buffer)
                   (buffer-substring-no-properties (point) pt2))))
      (cond (found?
             (message "found backward: %s" text)
             (match-beginning 0))
            (t
             (message "'%s' not found backward in buffer" text)
             cur)))))

;; from: http://stackoverflow.com/questions/2416655/file-path-to-clipboard-in-emacs
;; original: https://github.com/bbatsov/prelude
(defun prelude-copy-file-name-to-clipboard ()
  "Display and copy to the clipboard the current buffer file name."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "%s" filename))))

;; from LBO > scg-open-explorer
(defun open-buffer-path ()
  (interactive)
  (cl-flet ((w32ify-path (path)
                         (convert-standard-filename (replace-regexp-in-string "/" "\\" path t t))))
    (cond (buffer-file-name
           (w32-shell-execute "open" "explorer" (concat "/e,/select," (w32ify-path buffer-file-name))))
          (default-directory
            (w32-shell-execute "explore" (w32ify-path default-directory)))
          (t
           (user-error "Current buffer not associated with any path")))))

;; from: http://www.emacswiki.org/emacs/RevertBuffer
(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  ;;(message "force-reverting value is %s" force-reverting)
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))


;; from: https://stackoverflow.com/questions/43765/pin-emacs-buffers-to-windows-for-cscope
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not."
  (interactive)
  (message (if (let (window (get-buffer-window (current-buffer)))
                 (set-window-dedicated-p window (not (window-dedicated-p window))))
               "Window '%s' is dedicated"
             "Window '%s' is normal")
           (current-buffer)))

;; -- global keys --------------------------------------------------------------

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "C-S-<tab>") (lambda () (interactive) (other-window -1)))

(global-set-key "\M-i" "\C-u1\M-v") ; scroll up
(global-set-key "\M-k" "\C-u1\C-v") ; scroll down

(global-set-key (kbd "C-M-#") (open-file user-init-file))
(global-set-key (kbd "C-M-$") (open-file "c:/home/.autohotkey/autohotkey.ahk"))
(global-set-key (kbd "C-M-&") (open-file "c:/home/.bashrc"))

(global-set-key (kbd "C-c c") 'comment-or-uncomment-region-or-line)
(global-set-key [remap just-one-space] 'my-just-one-space)

(global-set-key [f5] 'revert-buffer-no-confirm)

(global-set-key [f8] 'toggle-window-dedicated)
(global-set-key [f9] 'whitespace-mode)
(global-set-key [f10] 'toggle-truncate-lines)
(global-set-key [f11] 'prelude-copy-file-name-to-clipboard)
(global-set-key [f12] 'open-buffer-path)

(define-key isearch-mode-map (kbd "C-d") 'sacha/isearch-yank-current-word) ; Type C-s (isearch-forward) to start interactively searching forward, and type C-x to get the current word.
(global-set-key '[C-M-up] 'sacha/search-word-backward)
(global-set-key '[C-M-down] 'sacha/search-word-forward)
