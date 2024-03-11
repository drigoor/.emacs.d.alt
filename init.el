(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)


(require 'use-package-ensure)
(setq use-package-always-ensure t)


(use-package emacs
  :demand t
  :custom
  ;; memory configuration
  (gc-cons-threshold 10000000 "Higher garbage collection threshold, prevents frequent gc locks.")
  (byte-compile-warnings '(not obsolete) "Ignore warnings for (obsolete) elisp compilations.")
  (warning-suppress-log-types '((comp) (bytecomp)) "And other log types completely.")
  (large-file-warning-threshold 100000000 "Large files are okay in the new millenium.")
  (read-process-output-max (max 65536 read-process-output-max) "Read upto 64K (or max) based on system pipe capacity")
  ;; frame configuration
  (frame-inhibit-implied-resize t "Improve emacs startup time by not resizing to adjust for custom settings.")
  (frame-resize-pixelwise t "Dont resize based on character height / width but to exact pixels.")
  ;; backups
  (make-backup-files nil)

  (inhibit-startup-screen t)
  (inhibit-startup-message t)
  (inhibit-startup-echo-area-message t)
  (initial-scratch-message (concat
                            ";; This buffer is for text that is not saved, and for Lisp evaluation.\n"
                            ";; To create a file, visit it with C-x C-f and enter text in its buffer.\n"
                            ";;\n"
                            ";; __          __  _\n"
                            ";; \\ \\        / / | |\n"
                            ";;  \\ \\  /\\  / /__| | ___ ___  _ __ ___   ___\n"
                            ";;   \\ \\/  \\/ / _ \\ |/ __/ _ \\| '_ ` _ \\ / _ \\\n"
                            ";;    \\  /\\  /  __/ | (_| (_) | | | | | |  __/_\n"
                            ";;     \\/  \\/ \\___|_|\\___\\___/|_| |_| |_|\\___(_)\n"))

  ;; packages
  (package-install-upgrade-built-in t)


  (default-directory "~/")

  :init
  (delete-selection-mode +1)
  (tooltip-mode -1)

  (menu-bar-mode -1)                    ;; no menu bar
  (toggle-scroll-bar -1)                ;; no scroll bar
  (tool-bar-mode -1)                    ;; no tool bar either
  (global-hl-line-mode +1)              ;; always highlight current line
  (blink-cursor-mode -1)                ;; stop blinking
  (global-display-line-numbers-mode -1) ;; always show line numbers
  (column-number-mode t)                ;; column number in the mode line
  (pixel-scroll-precision-mode)         ;; smooth mouse scroll
  (fset 'yes-or-no-p 'y-or-n-p)         ;; dont ask me to type yes/no everytime, y/n is good enough
;;----------- TODO repeated  (electric-pair-mode)                  ;; auto-insert matching parenthesis

  ;; UTF-8 EVERYWHERE
  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment   'utf-8)

  (set-frame-font "Consolas 11" nil t) ;; font of the century

  ;; Always use spaces for indentation
  (setq-default indent-tabs-mode nil
                tab-width 4
                show-trailing-whitespace t
                truncate-lines t)

  ;; TODO DELETE ME -- just an example: https://www.gnu.org/software/emacs/manual/html_node/use-package/Getting-Started.html
  ;;
  ;; :bind (("M-s O" . moccur)
  ;;        :map isearch-mode-map
  ;;        ("M-o" . isearch-moccur)
  ;;        ("M-O" . isearch-moccur-all))

  :bind
  (("C-<wheel-up>"   . nil)                  ; dont zoom in please
   ("C-<wheel-down>" . nil)                  ; dont zoom in either
   ("C-x k"          . kill-this-buffer))    ; kill the buffer, dont ask
  :mode
  ("\\.rs\\'" . rust-ts-mode)
  ("\\.go\\'" . go-ts-mode)
  ("\\.ts\\'" . typescript-ts-mode)
  ("\\.tsx\\'" . tsx-ts-mode)
  ("\\.cs\\'" . csharp-ts-mode)
  :hook
  (before-save . whitespace-cleanup))


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
  (global-set-key (kbd "C-x 2") 'ian/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") 'ian/split-and-follow-vertically))


(use-package frame
  :ensure nil
  ;; :preface
  ;; (defun ian/set-default-font ()
  ;;   (interactive)
  ;;   (when (member "Consolas" (font-family-list))
  ;;     (set-face-attribute 'default nil :family "Consolas"))
  ;;   (set-face-attribute 'default nil :height 100 :weight 'normal))
  :config
  ;; (ian/set-default-font)
  (set-background-color "#fefefc"))


(use-package faces
  :ensure nil
  :config
  (set-face-background 'cursor "orange")
  (set-face-background 'region "#ffffcc")
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


(use-package fringe
  :ensure nil
  ;; :custom-face (fringe ((t (:background "#fefefc"))))
  :config (set-fringe-mode 16))


(use-package hl-line
  :ensure nil
  :custom-face
  (hl-line ((t (:background "#f0ffe0"))))
  :config
  (global-hl-line-mode +1))


;; Auto-insert matching parenthesis
(use-package elec-pair
  :config (electric-pair-mode t))


;; Highlight matching parenthesis
(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :config
  (setq show-paren-style 'expression)
  (show-paren-mode +1))
;; (set-face-attribute 'show-paren-match-expression nil :background "yellow")


(use-package goto-last-change
  :bind (("C-c \\" . goto-last-change)))


;; Editable and saveable grep buffer
(use-package wgrep)


(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode +1) ; revert buffers automatically when underlying files are changed externally
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        global-auto-revert-non-file-buffers t))


;; Delete intermediate buffers when navigating through dired
(use-package dired
  :ensure nil
  :config
  (setq delete-by-moving-to-trash t)
  (setq ls-lisp-dirs-first t)
  (eval-after-load "dired"
    #'(lambda ()
        (put 'dired-find-alternate-file 'disabled nil)
        (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file))))


;; Dump custom-set-variables to a garbage file and don't load it
(use-package cus-edit
  :ensure nil
  :config (setq custom-file (concat user-emacs-directory "custom.el")))


(use-package nerd-icons
  :defer nil
  :custom
  (nerd-icons-color-icons nil))


;; (use-package doom-modeline
;;   :after (nerd-icons)
;;   :custom
;;   (inhibit-compacting-font-caches t)
;;   ;; (doom-modeline-buffer-file-name-style 'relative-from-project)
;;   (doom-modeline-buffer-file-name-style 'auto)
;;   (setq doom-modeline-buffer-file-name-style 'buffer-name)
;;   (doom-modeline-major-mode-icon nil)
;;   (doom-modeline-buffer-encoding nil)
;;   (doom-modeline-buffer-state-icon nil)
;;   ;; (doom-modeline-lsp nil)
;;   (doom-modeline-height 33)
;;   ;; (doom-modeline-modal t)
;;   ;; (doom-modeline-display-misc-in-all-mode-lines t)
;;   ;; (doom-modeline-env-enable-go t)
;;   (doom-modeline-position-column-line-format '("%l:%c"))
;;   (doom-modeline-percent-position nil)
;;   ;; (doom-modeline-total-line-number t)
;;   ;; (doom-modeline-battery t) ;; `display-battery-mode'
;;   ;; (doom-modeline-time t) ;; `display-time-mode'
;;   (doom-modeline-highlight-modified-buffer-name t)
;;   ;;
;;   (size-indication-mode nil) ;; file size in the mode line
;;   :hook (after-init . doom-modeline-mode))


(use-package company
  :hook (after-init . global-company-mode))


(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  ;; (load-theme 'doom-nord t)
  ;; (load-theme 'doom-one-light t)
  ;; (load-theme 'doom-nord-light t)
  (doom-themes-visual-bell-config))


(use-package pulsar
  :defer t
  :custom
  (pulsar-pulse t)
  (pulsar-delay 0.05)
  (pulsar-iterations 10)
  (pulsar-face 'pulsar-magenta)
  (pulsar-highlight-face 'pulsar-yellow)
  :init
  (pulsar-global-mode 1)
  :hook
  (next-error . pulsar-pulse-line))


;; displays the key bindings following your currently entered incomplete command
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))


;; Expand region increases the selected region by semantic units. Just keep pressing the key until it selects what you want.
(use-package expand-region ; from: https://susamn.medium.com/ultimate-emacs-setup-with-documentation-in-org-mode-8ed32e2b3487
  :defer t
  :bind
  ("M-m" . er/expand-region))


;; Vertico provides a performant and minimalistic vertical completion UI based on the default completion system.
(use-package vertico
  :defer t
  :custom
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (enable-recursive-minibuffers t)
  :init
  (vertico-mode)
  :config
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))


;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


;; Puni contains commands for soft deletion, which means deleting while keeping parentheses (or other delimiters, like html tags) balanced.
(use-package puni
  :defer t
  :init
  (puni-global-mode))


(use-package crux
  :defer t
  :config
  ;;(define-key lisp-mode-map (kbd "RET") #'dired-find-alternate-file)
  :bind

  ;; ((kbd [remap move-beginning-of-line]) #'crux-move-beginning-of-line)
  ;; ((kbd "C-c o") #'crux-open-with)
  ;; ([(shift return)] #'crux-smart-open-line)
  ;; ;; ([(M return)] . crux-smart-open-line)
  ;; ;; (global-set-key (kbd "s-r") #'crux-recentf-ido-find-file)
  ;; ;; (global-set-key (kbd "C-<backspace>" #'crux-kill-line-backwards))
  ;; ;; (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)

  ;; ("C-c C-w" . crux-transpose-windows)
  ;; ;;  ("C-a"     . crux-move-beginning-of-line)
  ;; ("C-S-k"      . crux-kill-whole-line)
  ;; ("C-c k"      . crux-kill-other-buffers)

  ;; ("C-c d"      . crux-duplicate-current-line-or-region)
  ;; ("C-c M-d"    . crux-duplicate-and-comment-current-line-or-region)


  ;; ;; ("C-c r"      . crux-rename-buffer-and-file)

  ;; (global-set-key (kbd "C-M-q") 'crux-indent-defun)

  )




;; (global-set-key (kbd "C-x k") 'kill-this-buffer)
;; (global-set-key (kbd "C-w") 'backward-kill-word)
;; (global-set-key (kbd "M-o") 'other-window)



;; (define-key (current-global-map) [remap kill-line] 'my-homemade-kill-line)


;; displays current match and total matches information in the mode-line in various search modes
(use-package anzu
  :bind
  (("<remap> <query-replace>" . 'anzu-query-replace)
   ("<remap> <query-replace-regexp>" . 'anzu-query-replace-regexp))
  :config
  (global-anzu-mode t)
  (set-face-foreground 'anzu-mode-line "#FF6F00"))


(use-package ahk-mode
  :config
  (setq ahk-indentation 2))


(use-package ediff
  :ensure nil
  :init
  (add-to-list 'exec-path "c:/bin/scoop/apps/git/current/usr/bin")
  (setenv "PATH" (concat (getenv "PATH") ";" "c:/bin/scoop/apps/git/current/usr/bin"))
  :config
  ;; from: https://emacs.stackexchange.com/questions/7482/restoring-windows-and-layout-after-an-ediff-session/7486
  ;;
  ;; check alternative in: https://github.com/Gavinok/emacs.d/blob/main/init.el

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

  (setq extra-gitgrep-file-extensions "*.el *.lisp *.cl *.lua *.fnl *.java *.kt *.ahk *.js *.ts *.html"))


(use-package hideshow
  :ensure nil
  :preface
  ;; from: https://wiki/siscog/MetodosDeTrabalhoNaoOficiais#A.5BLBO.4020100209.5D_Hide.2BAC8-Show_Comments_.28and_Code.29
  (defun hs-hide-all-comments ()
    "Adapted from `hs-hide-all'."
    (interactive)
    (hs-life-goes-on
     (save-excursion
       (unless hs-allow-nesting
         (hs-discard-overlays (point-min) (point-max)))
       (goto-char (point-min))
       (let ((spew (make-progress-reporter "Hiding all comments..."
                                           (point-min) (point-max)))
             (re (concat "\\(" hs-block-start-regexp "\\)"
                         "\\|\\(" hs-c-start-regexp "\\)")))
         (while (progn
                  (unless hs-hide-comments-when-hiding-all
                    (forward-comment (point-max)))
                  (re-search-forward re (point-max) t))
           (if (match-beginning 1)
               ;; we have found a block beginning, skip it
               (progn
                 (goto-char (match-beginning 1))
                 (forward-sexp 1))
             ;; found a comment, probably
             (let ((c-reg (hs-inside-comment-p)))
               (when (and c-reg (car c-reg))
                 (if (> (count-lines (car c-reg) (nth 1 c-reg)) 1)
                     (hs-hide-block-at-point t c-reg)
                   (goto-char (nth 1 c-reg))))))
           (progress-reporter-update spew (point)))
         (progress-reporter-done spew)))
     (beginning-of-line)
     (run-hooks 'hs-hide-hook)))

  (defvar hs-hide-all-comments-p nil)

  (make-variable-buffer-local 'hs-all-comments-hidden-p)

  (defun hs-toggle-all-comments ()
    (interactive)
    (setq hs-all-comments-hidden-p (not hs-all-comments-hidden-p))
    (if hs-all-comments-hidden-p
        (hs-hide-all-comments)
      (hs-show-all)))

  (defun hide-show-fn () ; from: http://emacs-fu.blogspot.pt/2008/12/showing-and-hiding-blocks-of-code.html
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode +1))
  :hook (prog-mode . hide-show-fn)
  :config (setq hs-isearch-open 'code))


;; (use-package diminish
;;   :defer t
;;   :config
;;   (diminish 'eldoc-mode)
;;   (diminish 'anzu-mode)
;;   (diminish 'hs-minor-mode)
;;   )


;; hide all minor modes
(use-package minions ; from: https://susamn.medium.com/ultimate-emacs-setup-with-documentation-in-org-mode-8ed32e2b3487
  :config
  (setq minions-mode-line-lighter ""
        minions-mode-line-delimiters '("" . ""))
  (global-set-key [S-down-mouse-3] 'minions-minor-modes-menu)
  (minions-mode +1))


;; -- magit --------------------------------------------------------------------

;; references:
;; https://github.com/bradwright/emacs-d/blob/master/packages/init-magit.el
;; http://whattheemacsd.com/setup-magit.el-01.html
(use-package magit
  :defer t
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


(use-package magit-delta ; check  or https://scripter.co/using-git-delta-with-magit
  :hook (magit-mode . magit-delta-mode))


;; -- lisp ---------------------------------------------------------------------

(use-package sly
  :config
  (setq inferior-lisp-program (expand-file-name "C:/bin/scoop/apps/sbcl/current/sbcl.exe")))


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

(global-set-key (kbd "C-M-#") (open-file user-init-file))
(global-set-key (kbd "C-M-$") (open-file "d:/.autohotkey/autohotkey.ahk"))
(global-set-key (kbd "C-M-&") (open-file "d:/.bashrc"))

(global-set-key (kbd "C-c c") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-SPC") 'my-just-one-space)

(global-set-key [f5] 'revert-buffer-no-confirm)
(global-set-key [f6] (lambda ()
                       (interactive)
                       (revert-buffer-no-confirm)
                       (hs-hide-all-comments)))
(global-set-key [f8] 'toggle-window-dedicated)
(global-set-key [f9] 'whitespace-mode)
(global-set-key [f10] 'toggle-truncate-lines)
(global-set-key [f11] 'prelude-copy-file-name-to-clipboard)
(global-set-key [f12] 'open-buffer-path)

(global-set-key '[C-M-up] 'sacha/search-word-backward)
(global-set-key '[C-M-down] 'sacha/search-word-forward)

(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "C-S-<tab>") (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "M-<up>") "\C-u1\M-v") ; scroll up
(global-set-key (kbd "M-<down>") "\C-u1\C-v") ; scroll down

(define-key isearch-mode-map (kbd "C-d") 'sacha/isearch-yank-current-word) ; Type C-s (isearch-forward) to start interactively searching forward, and type C-x to get the current word.

;; -----------------------------------------------------------------------------


;; CHECK THIS OUT:
;;
;; https://github.com/wolray/symbol-overlay?tab=readme-ov-file
;;    https://github.com/nschum/highlight-symbol.el
;;
;; https://hieuphay.com/doom-emacs-config/
;; https://www.gonsie.com/blorg/modeline.html


;; REFERENCES:
;;
;; https://vishesh.github.io/emacs/editors/2023/01/25/lean-emacs-config.html
;; https://github.com/munen/emacs.d/

;; https://kristofferbalintona.me/posts/202202211546/
;; https://ianyepan.github.io/posts/setting-up-use-package/
;; https://suvratapte.com/configuring-emacs-from-scratch-use-package/


;; REQUIREMENTS:
;;
;; * delta -- use in package `magit-deltasc'
;;
;; for delta ensure in .gitconfig
;; [core]
;;     pager = delta
;;     editor = \"C:/home/scoop/apps/gitextensions/current/gitextensions.exe\" fileeditor
;; [interactive]
;;     diffFilter = delta --color-only
;; [delta]
;;     navigate = true
;;     light = false


;; PROVIDED:
;;
;; + start a command and after a while shows helper info for commands (by `which-key')




;; from: sachachua.com 2024-02-26 Emacs news >> https://blog.dornea.nu/2024/02/22/from-doom-to-vanilla-emacs/ >>
;;       https://config.phundrak.com/emacs/basic-config.html >>
;;       https://tecosaur.github.io/emacs-config/config.html




;; (setq-default fill-column 79)
;; (global-display-fill-column-indicator-mode 1)
;; (add-hook 'java-mode-hook (lambda () (setq fill-column 120)))
;; (add-hook 'python-mode-hook (lambda () (setq fill-column 79)))



(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-o") 'other-window)
