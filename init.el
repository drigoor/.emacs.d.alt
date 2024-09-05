;;; init.el --- that thing one wastes so much time -*- lexical-binding: t -*-


(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
  (require 'use-package)
  (setq use-package-compute-statistics t  ; to use `use-package-report'
        use-package-expand-minimally nil
        use-package-verbose t
        use-package-always-ensure t))


;; -------------------------------------


(server-mode)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq inhibit-startup-echo-area-message "user")

(blink-cursor-mode -1)
(tooltip-mode -1)
(column-number-mode +1)
(global-so-long-mode +1)
(delete-selection-mode +1)

(setq-default indent-tabs-mode nil
              show-trailing-whitespace t
              truncate-lines t)

(setq frame-title-format '("%b")
      frame-resize-pixelwise t
      default-directory "~/")

;; from: https://github.com/magnars/.emacs.d/blob/master/settings/appearance.el
;; from: https://www.emacswiki.org/emacs/AlarmBell
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)

(setq custom-file (locate-user-emacs-file "custom.el")) ; customize is not used

(setq use-short-answers t) ; same as `(fset 'yes-or-no-p 'y-or-n-p)'
(setq confirm-nonexistent-file-or-buffer nil) ; no annoying confirmation if a file or buffer does not exist when you use C-x C-f or C-x b

(setq echo-keystrokes 0.1
      mode-line-percent-position "")

(setq-default line-spacing 0.15)

(set-face-attribute 'default nil
                    :family "Consolas"
                    :height 100
                    :weight 'normal) ; (set-frame-font "Consolas 10" nil t) ;; wsl --> Monospace 12
(set-background-color "#fefefc")
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
                    :box `(:line-width 3 :color ,"#f5f2ef" :style nil))


(add-hook 'emacs-startup-hook ; could be also `after-init-hook'
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (emacs-init-time "%.2f seconds")
                     gcs-done)))


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


;; -------------------------------------


;; Eye level window centering in Emacs
;; from: https://www.n16f.net/blog/eye-level-window-centering-in-emacs/
(defcustom g-recenter-window-eye-level 0.2
  "The relative position of the line considered as eye level in the
current window, as a ratio between 0 and 1.")

(defun g-recenter-window ()
  "Scroll the window so that the current line is at eye level."
  (interactive)
  (let ((line (round (* (window-height) g-recenter-window-eye-level))))
    (recenter line)))

(global-set-key (kbd "C-l") 'g-recenter-window)


;;; --------------------------------------------------------------------
;;;
;;; use-package with :ensure nil


(use-package ido
  :ensure nil
  :init
  (setq ido-create-new-buffer 'always
        ido-file-extensions-order '(".lisp" ".el" ".txt"))
  :config
  (ido-mode +1))


(use-package fringe
  :ensure nil
  :custom-face (fringe ((t (:background "#fefefc"))))
  :config
  (set-fringe-mode 8))


(use-package hl-line
  :ensure nil
  :custom-face (hl-line ((t (:background "#f0ffe0")))) ; same as: (set-face-background 'hl-line "#f0ffe0")
  :config
  (global-hl-line-mode +1))


(use-package files
  :ensure nil
  :config
  (setq confirm-kill-processes nil
        create-lockfiles nil ; don't create .# files (crashes 'npm start')
        make-backup-files nil)
  (setq-default require-final-newline t))


(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode +1)) ; revert buffers automatically when underlying files are changed externally


(use-package mwheel
  :ensure nil
  :config (setq mouse-wheel-scroll-amount '(2 ((shift) . 1)) ; '(7 ((shift) . 1) ((meta) . hscroll) ((control) . text-scale))
                mouse-wheel-progressive-speed nil))


(use-package paren
  :ensure nil
  :init (setq show-paren-delay 0)
  :config
  ;; (show-paren-mode +1) ; global activation...
  (setq show-paren-style 'expression)
  (set-face-attribute 'show-paren-match-expression nil :background "papaya whip")
  (set-face-attribute 'show-paren-match nil :weight 'bold)
  :hook (prog-mode . show-paren-mode))


;; Auto-insert matching parenthesis.
(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))


;; from: http://trey-jackson.blogspot.pt/2008/01/emacs-tip-11-uniquify.html -- for reverse
;; from: http://pragmaticemacs.com/emacs/uniquify-your-buffer-names/
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*")) ; don't muck with special buffers


;; Delete intermediate buffers when navigating through dired.
(use-package dired
  :ensure nil
  :config
  (setq delete-by-moving-to-trash t)
  (setq ls-lisp-dirs-first t)
  (eval-after-load "dired"
    #'(lambda ()
        (put 'dired-find-alternate-file 'disabled nil)
        (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file))))


;; The Emacs default split doesn't seem too intuitive for most users.
(use-package emacs ; from: https://github.com/ianyepan/yay-evil-emacs/blob/master/config.org
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


(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-idle-delay 0.4))


(use-package ediff
  :ensure nil
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

  (setq extra-gitgrep-file-extensions "*.lisp *.cl"))


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


;;; --------------------------------------------------------------------
;;;
;;; use-package with default :ensure, t (aka download from the internet)


(use-package minions
  :hook (prog-mode . minions-mode))


(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode))


(use-package csv-mode
  :mode "\\.csv\\'")


(use-package sly
  :config
  (setq inferior-lisp-program (expand-file-name "C:/bin/scoop/apps/sbcl/current/sbcl.exe")))


(use-package sly-overlay)


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
  (("C-x g" . magit-status) ; from: http://blog.jr0cket.co.uk/2012/12/driving-git-with-emacs-pure-magic-with.html
   ("C-c b" . magit-blame)
   :map magit-status-mode-map
   ("q" . magit-quit-session)
   ("<return>" . magit-diff-visit-file-other-window))
  :config
  (advice-add 'magit-status :around #'magit-status-around) ; check: https://www.gnu.org/software/emacs/manual/html_node/elisp/Porting-old-advice.html
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (magit-auto-revert-mode +1))


(use-package magit-delta ; check  or https://scripter.co/using-git-delta-with-magit
  ;; for delta ensure in .gitconfig
  ;; [core]
  ;;     pager = delta
  ;;     editor = \"C:/home/scoop/apps/gitextensions/current/gitextensions.exe\" fileeditor
  ;; [interactive]
  ;;     diffFilter = delta --color-only
  ;; [delta]
  ;;     navigate = true
  ;;     light = false
  :after magit
  :hook (magit-mode . magit-delta-mode))


(use-package powershell)


(use-package ahk-mode
  :config
  (setq ahk-indentation 2))


(use-package eros ; from: https://www.reddit.com/r/emacs/comments/bi4xk1/evaluation_overlays_in_slime_for_common_lisp/?rdt=52664
  :config
  (eros-mode))


;; Editable and saveable grep buffer
(use-package wgrep)


;; Expand region increases the selected region by semantic units. Just keep pressing the key until it selects what you want.
;; from: https://susamn.medium.com/ultimate-emacs-setup-with-documentation-in-org-mode-8ed32e2b3487
;; from: https://www.reddit.com/r/emacs/comments/yf14tl/what_are_your_favorite_packages_for_improving/
(use-package expand-region
  :bind
  ("C-+" . er/expand-region)
  ("C--" . er/contract-region))


(use-package crux
  :bind
  ([remap kill-line] . crux-smart-kill-line)
  ([remap indent-pp-sexp] . crux-indent-defun)
  ([remap move-beginning-of-line] . crux-move-beginning-of-line)
  ("C-S-k" . crux-kill-whole-line)
  ("C-c d" . crux-duplicate-current-line-or-region)
  ([(C <return>)] . crux-smart-open-line)
  ([(S <return>)] . crux-smart-open-line-above))


;; displays current match and total matches information in the mode-line in various search modes
(use-package anzu
  :bind
  (("<remap> <query-replace>" . 'anzu-query-replace)
   ("<remap> <query-replace-regexp>" . 'anzu-query-replace-regexp))
  :config
  (global-anzu-mode t)
  (set-face-foreground 'anzu-mode-line "#FF6F00"))


;; adapted from: https://github.com/shaneikennedy/emacs-light/blob/master/init.el
(use-package company
  :bind (:map company-mode-map
              ("C-M-i" . company-indent-or-complete-common)
         :map company-active-map
              ("<tab>" . company-complete-common-or-cycle) ; company-indent-or-complete-common
              ("<backtab>" . (lambda ()
                               (interactive)
                               (company-complete-common-or-cycle -1)))
              ("M-/" . company-complete)
              ("M-." . company-show-location)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay 0)
  (company-dabbrev-downcase nil)
  (company-show-numbers t)
  (company-tooltip-limit 10)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                       company-echo-metadata-frontend))
  :config
  ;; (global-company-mode) ; global activation...
  ;; use numbers 0-9 to select company completion candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                        `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9))))


(use-package company-quickhelp
  :after company
  :hook (company-mode . company-quickhelp-local-mode)
  :config
  (setq company-quickhelp-delay 0.5)
  (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))


;; displays the key bindings following your currently entered incomplete command
(use-package which-key
  :config
  (which-key-mode +1)
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.4))


;; sort M-x by recently used
(use-package smex
  :bind ("M-x" . smex)
  :config (smex-initialize))


(use-package rainbow-delimiters
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil :foreground (face-background 'default) :background (face-foreground 'error)) ; from: https://writequit.org/eos/eos-appearance.html

  ;; use color: DarkGoldenrod3

  ;; (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
  ;; (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
  ;; (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
  ;; (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
  ;; (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
  ;; (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
  ;; (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
  ;; (set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
  ;; (set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray
  :hook (prog-mode . rainbow-delimiters-mode))


;; -- custom code --------------------------------------------------------------


(defun open-file (path)
  `(lambda ()
     (interactive)
     (find-file ,path)))


(defun my-just-one-space ()
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
;;           https://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
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


(defun scroll-up-with-fixed-cursor ()
  (interactive)
  (forward-line 1)
  (scroll-up 1))


(defun scroll-down-with-fixed-cursor ()
  (interactive)
  (forward-line -1)
  (scroll-down 1))


(defun find-my-buffer (name)
  (dolist (buffer (buffer-list))
    (when (string-match name (buffer-name buffer))
      (cl-return buffer))))


;; -- global keys --------------------------------------------------------------


(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
;; (global-set-key (kbd "C-z") #'undo)

(global-set-key "\M-i" "\C-u1\M-v")
(global-set-key "\M-k" "\C-u1\C-v")

(global-set-key (kbd "M-<up>") "\C-u1\M-v")
(global-set-key (kbd "M-<down>") "\C-u1\C-v")

(global-set-key (kbd "C-M-i") 'scroll-down-with-fixed-cursor)
(global-set-key (kbd "C-M-k") 'scroll-up-with-fixed-cursor)

(global-set-key (kbd "M-<up>") "\C-u1\M-v") ; scroll up
(global-set-key (kbd "M-<down>") "\C-u1\C-v") ; scroll down

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "C-M-#") (open-file user-init-file))
(global-set-key (kbd "C-M-$") (open-file "d:/.autohotkey/autohotkey.ahk"))
(global-set-key (kbd "C-M-&") (open-file "d:/.bashrc"))

(global-set-key (kbd "C-c c") 'comment-or-uncomment-region-or-line)
(global-set-key [remap just-one-space] 'my-just-one-space)

(global-set-key [f5] 'revert-buffer-no-confirm)
(global-set-key [f6] (lambda () (interactive) (revert-buffer-no-confirm) (hs-hide-all-comments)))
(global-set-key [f8] 'toggle-window-dedicated)
(global-set-key [f9] 'whitespace-mode)
(global-set-key [f10] 'toggle-truncate-lines) ; alternative: visual-line-mode
(global-set-key [f11] 'prelude-copy-file-name-to-clipboard)
(global-set-key [f12] 'open-buffer-path)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-S-<tab>") (lambda () (interactive) (other-window -1)))

;; (global-set-key (kbd "C-c u") #'browse-url-at-point

(global-set-key '[C-M-up] 'sacha/search-word-backward)
(global-set-key '[C-M-down] 'sacha/search-word-forward)

(define-key isearch-mode-map (kbd "C-d") 'sacha/isearch-yank-current-word) ; Type C-s (isearch-forward) to start interactively searching forward, and type C-x to get the current word.


(push "c:/bin/scoop/apps/git/current/usr/bin" exec-path)
