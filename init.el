;;; init.el --- Init File -*- lexical-binding: t; byte-compile-warnings: nil; eval: (outline-hide-body)-*-

;;; Commentary:
;; Nothing to see here...
;;; Code:
;; This section makes the linter happy.
;;; Disable GC during startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 2 1000 1000))))
;;; Networking
(require 'gnutls)
(with-eval-after-load 'gnutls

  ;; Do not allow insecure TLS connections.
  (setq gnutls-verify-error t)

  ;; Bump the required security level for TLS to an acceptably modern
  ;; value.
  (setq gnutls-min-prime-bits 3072))

;;; Packaging
;;;; use-package
(require 'use-package)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq use-package-always-defer t)

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
Passes NAME and ARGS to use-package."
  (declare (indent defun))
  `(use-package ,name
     ,@args))

;;;; Guix
(add-to-list 'load-path "~/.guix-home/profile/share/emacs/site-lisp")

;; (when (fboundp 'guix-emacs-autoload-packages)
;;   (guix-emacs-autoload-packages))

(use-package guix
  :bind ("C-c C-g" . guix))
;;; Configure ~/.emacs.d paths
;; Package `no-littering' changes the default paths for lots of
;; different packages, with the net result that the ~/.emacs.d folder
;; is much more clean and organized.
(use-package no-littering
  :demand t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file))

;;; Defaults
(setq inhibit-startup-screen t)

(when (display-graphic-p)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (winner-mode 1)
  (show-paren-mode 1))

(setq-default fill-column 80)
;;; Theme
;;;; Font
(use-package ligature
  :demand t
  :disabled t
  :config
  (set-frame-font "Fira Code-11")
  ;; Enable the www ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))

  ;; Enable ligatures in programming modes                                                           
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  ;; Add hex ligatures
  (add-to-list 'ligature-composition-table `(t ("0" . ,(rx "x" (+ (or hex-digit hex))))))
  
  (global-ligature-mode 't))
;;;; Color theme
(use-package doom-themes
  :demand t
  :disabled t
  :config
  (setq doom-one-padded-modeline t
	doom-one-brighter-modeline nil)
  (if (daemonp)
      (add-hook 'after-make-frame-functions
		(lambda (frame)
		  (select-frame frame)
		  (load-theme 'doom-one t)))
    (load-theme 'doom-one t)))

(use-package nord-theme
  :demand t
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
		(lambda (frame)
		  (select-frame frame)
		  (load-theme 'nord t)))
    (load-theme 'nord t)))
;;;; Modeline
(use-package doom-modeline
  :demand t
  :hook ((after-init . doom-modeline-mode)
         (after-init . display-time-mode))
  :config
  (line-number-mode 0)
  :custom
  (display-time-24hr-format t)
  (display-time-interval 5)
  (display-time-update)
  (display-time-default-load-average nil)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-enable-word-count t)
  (doom-modeline-percent-position nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-buffer-state-icon nil))

;;;; Posframe
(use-package ivy-posframe
  :after ivy
  :disabled t
  :hook (after-init . ivy-posframe-mode)
  :custom-face
  (ivy-posframe-border ((t (:inherit ivy-posframe))))
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center))
        ivy-posframe-height-alist '((t . 20))
        ivy-posframe-parameters '((internal-border-width . 10)
                                  (parent-frame . nil)))
  (setq ivy-posframe-width 90))
;;; System
;;;; exwm-randr
(use-feature exwm-randr
  :custom
  (exwm-randr-workspace-output-plist '(1 "eDP-1"))
  :config
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output eDP-1 --right-of HDMI-1 --auto")))
  (exwm-randr-enable))
;;;; exwm
(use-package exwm
  :demand t
  :init
  (require 'exwm-config)
  ;; Start a server for external processes to communicate with.
  (server-start)

  (setq exwm-workspace-number 1)
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-class-name))))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
                        (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-title))))
  ;; 's-r': Reset
  (exwm-input-set-key (kbd "s-r") #'exwm-reset)
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  ;; Line-editing shortcuts
  (setq
   exwm-manage-force-tiling t
   exwm-input-simulation-keys
   '(
     ;; movement
     ([?\C-b] . [left])
     ([?\M-b] . [C-left])
     ([?\C-f] . [right])
     ([?\M-f] . [C-right])
     ([?\C-p] . [up])
     ([?\C-n] . [down])
     ([?\C-a] . [home])
     ([?\C-e] . [end])
     ([?\M-v] . [prior])
     ([?\C-v] . [next])
     ([?\C-d] . [delete])
     ([?\C-k] . [S-end delete])
     ;; cut/paste.
     ([?\C-w] . [?\C-x])
     ([?\M-w] . [?\C-c])
     ([?\C-y] . [?\C-v])
     ;; search
     ([?\C-s] . [?\C-f])
     ;; save
     ([?\C-x?\C-s] . [?\C-s])
     ;; quit
     ([?\C-g] . [escape])))
  ;; Enable EXWM
  (exwm-enable))

;;;; Brightness
(defun message-brightness ()
  "Add a message to the mode line with the current brightness."
  (message "Brightness: %s%%"
           ;; Round to nearest 5.
           (* 5 (round
                 (/ (string-to-number
                     (shell-command-to-string "light"))
                    5)))))

(exwm-input-set-key (kbd "<XF86MonBrightnessUp>")
                    (lambda ()
                      (interactive)
                      (shell-command-to-string "light -A 5")
                      (message-brightness)))

(exwm-input-set-key (kbd "<XF86MonBrightnessDown>")
                    (lambda ()
                      (interactive)
                      (shell-command-to-string "light -U 5")
                      (message-brightness)))

;;;; Volume
(defun message-volume ()
  "Add a message to the mode line with the current brightness."
  (message "Volume: %s%%"
           (round
            (string-to-number
             (shell-command-to-string "pamixer --get-volume")))))



(defun get-muted ()
  "Check if the volume is muted."
  (if (string= "true\n" (shell-command-to-string
                         "pamixer --get-mute"))
      "Muted"
    "Unmuted"))

(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")
                    (lambda ()
                      (interactive)
                      (shell-command-to-string "pamixer -ui 5")
                      (message-volume)))

(exwm-input-set-key (kbd "<XF86AudioLowerVolume>")
                    (lambda ()
                      (interactive)
                      (shell-command-to-string "pamixer -ud 5")
                      (message-volume)))

(exwm-input-set-key (kbd "<XF86AudioMute>")
                    (lambda ()
                      (interactive)
                      (shell-command-to-string "pamixer -t")
                      (message (get-muted))))


;;;; Battery
(use-feature battery
  :after doom-modeline
  :demand t
  :config
  (display-battery-mode))

;;;; Pinentry
(use-package pinentry
  :hook (after-init . pinentry-start)
  :config
  (setenv "PINENTRY_USER_DATA" "USE_CURSES=0")
  (setenv "GPG_TTY" "/dev/pts/1")
  (setq epa-pinentry-mode 'loopback
        pinentry-popup-prompt-window nil))

(use-package exec-path-from-shell
  :hook (after-init . exec-path-from-shell-initialize)
  :config
  (setq
   exec-path-from-shell-variables
   (append
    '("SSH_AUTH_SOCK" "SSH_AGENT_PID")
    exec-path-from-shell-variables)))
;;;; Applications
(use-package app-launcher
  :bind ("s-SPC" . app-launcher-run-app))
;;; Candidate selection
(use-package vertico
  :init
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Grow and shrink minibuffer
  ;;(setq resize-mini-windows t)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  (setq vertico-cycle t)
  
  (vertico-mode))

(use-package consult
  :bind ("C-s" . consult-line))

(use-package orderless
  :init
  (setq completion-styles '(basic orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))
;;; Avy
(use-package avy
  :preface
  (defun my-avy-goto-char (char)
    (interactive (list (read-char "char: " t)))
    (avy-goto-char char)
    ;; Position cursor AFTER character
    (forward-char))
  :bind ("C-'" . my-avy-goto-char))
;;; Saving files
;; Don't make backup files.
(setq make-backup-files nil)

;; Don't make autosave files.
(setq auto-save-default nil)

;; Don't make lockfiles.
(setq create-lockfiles nil)
;;; Editing
;;;; Text Formatting
;;;;; Auto Fill
;; When filling paragraphs, assume that sentences end with one space
;; rather than two.
(setq sentence-end-double-space nil)

;; Trigger auto-fill after punctutation characters, not just
;; whitespace.
(mapc
 (lambda (c)
   (set-char-table-range auto-fill-chars c t))
 "!-=+]};:'\",.?")

;; Enable auto-fill-mode in text-mode.
(add-hook 'text-mode-hook #'auto-fill-mode)
;;;; Undo/redo
(use-package undo-tree
  :demand t
  :disabled t
  :bind (;; By default, `undo' (and by extension `undo-tree-undo') is
         ;; bound to C-_ and C-/, and `undo-tree-redo' is bound to
         ;; M-_. It's logical to also bind M-/ to `undo-tree-redo'.
	 ;; This overrides the default binding of M-/, which is to
         ;; `dabbrev-expand'.
         :map undo-tree-map
	 ("M-/" . undo-tree-redo))
  :config
  (global-undo-tree-mode 1))

;;;; Inter-program Killing
(setq save-interprogram-paste-before-kill t)
;;; IDE Features
;;;; Autocomplete
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  "Enables yasnippets for all company BACKENDs."
  (if (or (not company-mode/enable-yas)
          (and (listp backend)
               (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(use-package company
  :custom
  (company-idle-delay t)
  :init
  (global-company-mode)
  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

;;;; Auto commit
(use-package git-auto-commit-mode)
;;;; Folding
(use-package outshine
  :hook (prog-mode . outshine-mode))
;;;; Indentation
;; Don't use tabs for indentation. Use only spaces. Frankly, the fact
;; that `indent-tabs-mode' is even *available* as an *option* disgusts
;; me, much less the fact that it's *enabled* by default (meaning that
;; *both* tabs and spaces are used at the same time).
(setq-default indent-tabs-mode nil)

(use-package aggressive-indent
  :hook (prog-mode . aggressive-indent-mode))

;;;; Linting
(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  ;; Enable proselint for emails.
  (flycheck-add-mode 'proselint 'mu4e-compose-mode))
;;;; LSP
(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-prefer-flymake nil))

(use-package lsp-ui
  :after lsp
  :config
  (lsp-ui-flycheck-enable t))
;;;; DAP
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode 1))
;;;; Git
(setq user-full-name "Reily Siegel")
(setq user-mail-address "mail@reilysiegel.com")

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1 ""))

(use-package forge
  :after magit
  :config
  (setq forge-owned-accounts
        '(("ReilySiegel" . nil))))

(use-package git-email
  :demand t
  :config
  (git-email-notmuch-mode 1))

(use-package git-email-magit
  :demand t)

;;;; Paredit
(use-package paredit
  :hook (prog-mode . enable-paredit-mode))
;;;; Yasnippet
(use-package yasnippet-snippets) 

(use-package yasnippet
  :hook (after-init . yas-global-mode)) 
;;; Language Support
;;;; Clojure
(use-package clojure-mode
  :custom
  (clojure-align-forms-automatically t)
  (clojure-toplevel-inside-comment-form t))

(use-package cider
  :hook (cider-mode . eldoc-mode)
  :custom
  (cider-font-lock-dynamically '(macro core function var))
  (cider-clojure-cli-global-options "-A:dev")
  :config
  (setq cider-scratch-initial-message
        "(ns user\n  (:require [reilysiegel.scratch :refer :all]))\n\n"
        cider-repl-pop-to-buffer-on-connect nil)
  ;; Put *cider-scratch* buffers in a Clojure project, so that I get middleware.
  (advice-add 'cider-scratch :after (lambda () (cd "~/.clojure/scratch/"))))

(use-package clj-refactor
  :init
  (setq cljr-warn-on-eval t)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-r")
  (add-hook 'clojure-mode-hook #'clj-refactor-mode))

(use-package flycheck-clj-kondo
  :after flycheck
  :disabled t
  :demand t)

;;;; Gnuplot
(use-package gnuplot)
;;;; Java
(use-package lsp-java
  :hook (java-mode . lsp)
  :config
  (require 'dap-java))
;;;; Prose
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))
;;;; Racket
(use-package racket-mode)
;;;; Scheme
(use-feature geiser
  :custom
  (geiser-default-implementation 'guile))
;;;; Yaml
(use-package yaml-mode)
;;; Eshell
(use-feature eshell
  :bind ("C-c s" . eshell)
  :demand t
  :config
  (require 'esh-module)
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (setq eshell-destroy-buffer-when-process-dies t
        eshell-history-size 1024
        remote-file-name-inhibit-cache nil
        vc-ignore-dir-regexp
        (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp)
        eshell-visual-commands '("htop" "nmtui" "vim" "watch")))
;;; ERC
(use-feature erc
  :config
  (setq erc-prompt-for-password nil))
;;; Notmuch
(use-package notmuch
  :bind
  ("C-c m" . notmuch)
  :preface
  (defun reily/select-smtp-send-it ()
    (let* ((from (message-fetch-field "From"))
           (smtp (cond
                  ((string-match "mail@reilysiegel.com" from) "smtp.zoho.com")
                  ((string-match "rsiegel@wpi.edu" from) "smtp.office365.com"))))
      (setq smtpmail-smtp-server smtp)
      (message smtp)
      (smtpmail-send-it)))
  :init
  (setq message-send-mail-function 'reily/select-smtp-send-it
        starttls-use-gnutls t
        smtpmail-stream-type 'starttls
        message-cite-reply-position 'below
        message-kill-buffer-on-exit t
        notmuch-search-oldest-first nil
        notmuch-show-logo nil
        notmuch-fcc-dirs '(("mail@reilysiegel.com" . "personal/sent"))
        notmuch-hello-sections '(notmuch-hello-insert-header
                                 notmuch-hello-insert-saved-searches)
        notmuch-saved-searches
        '((:name "[i]nbox" :query "tag:unread AND tag:inbox" :key "i")
          (:name "[w]atching" :query "tag:unread AND thread{tag:flagged}" :key "w")
          ;;Show unread automated messages
          (:name "auto[m]ated" :query "tag:unread AND tag:automated" :key "m")
          ;;Show unread promotional messages
          (:name "[p]romotional" :query "tag:unread AND tag:promotional" :key "p")
          (:name "[f]lagged" :query "tag:flagged" :key "f")
          (:name "[s]ent" :query "tag:sent" :key "s")
          (:name "[d]rafts" :query "tag:draft" :key "d")
          (:name "[a]ll mail" :query "*" :key "a"))))
;;; Pass
(use-package transient-pass
  :disabled t
  :bind ("C-c p" . transient-pass)
  :config
  (setq password-store-password-length 16))

(use-feature auth-source-pass
  :defer 1
  :config
  (auth-source-pass-enable))
;;; Org Mode
(require 'plantuml-mode)

(use-feature org
  :hook ((org-clock-in . save-buffer)
         (org-clock-out . save-buffer))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)
     (scheme . t)
     (gnuplot . t)
     (java . t)
     (python . t)
     (clojure . t)
     (R . t)))

  (setq org-file-apps
        (butlast org-file-apps))

  (setq org-directory "~/Dropbox/org"
        org-plantuml-jar-path
        plantuml-jar-path
        
        ;; org-latex-pdf-process
        ;; '("xelatex -interaction nonstopmode %f"
        ;;   "xelatex -interaction nonstopmode %f")
        )
  ;; Set (no) indentation
  (setq org-adapt-indentation nil)
  ;; Log time a task was set to Done.
  (setq org-log-done (quote time))

  ;; Don't log the time a task was rescheduled or redeadlined.
  (setq org-log-redeadline nil)
  (setq org-log-reschedule nil)
  ;; Refresh org-agenda after rescheduling a task.
  (defun org-agenda-refresh ()
    "Refresh all `org-agenda' buffers."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'org-agenda-mode)
          (org-agenda-maybe-redo)))))

  (defadvice org-schedule (after refresh-agenda activate)
    "Refresh org-agenda."
    (org-agenda-refresh)))

;;;; Org agenda
(use-feature org-agenda
  :after org
  :preface
  (defun org-agenda-show-agenda-and-todo (&optional arg)
    (interactive "P")
    (org-agenda arg "n"))
  :bind
  (("C-c a" . org-agenda-show-agenda-and-todo)
   ("C-c o" . org-capture))
  :config
  (setq
   org-agenda-window-setup 'current-window
   org-agenda-files '("~/Dropbox/org/" "~/WPI/2020/")
   org-agenda-span 'week
   org-enforce-todo-dependencies t
   org-log-done (quote time)
   org-log-schedule (quote time)
   org-log-redeadline (quote time)
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)
   org-agenda-todo-ignore-deadlines (quote all)
   org-agenda-todo-ignore-scheduled (quote all))
  (setq
   org-capture-templates
   `(("t" "Task" entry
      (file+headline "organizer.org" "Tasks")
      "** TODO %?"
      :prepend t)
     ("e" "Email" entry
      (file+headline "organizer.org" "Tasks")
      "** %a %?"
      :prepend t)
     ("p" "Page" entry
      (file+headline "axp.org" "Book")
      ,(string-join '("** %^{Scroll Number} - %^{Name}"
                      ":PROPERTIES:"
                      ":CUSTOM_ID: %\\1"
                      ":END:"
                      "| Big Brother | [[#%^{Big's Scroll Number}][%^{Big's Name}]] |"
                      "| Birthday | %^{Birthday}t |"
                      "| Phone Number | %^{Phone Number} |"
                      "| WPI Email | %^{WPI Email} |"
                      "| Email | %^{Email} |"
                      "| Local Address | %^{Local Address} |"
                      "| Home Address | %^{Home Address} |"
                      "| Major | %^{Major} |"
                      "| Minor | %^{Minor} |"
                      "| Concentration | %^{Concentration} |"
                      "| Year of Graduation | %^{Year of Graduation} |"
                      "| IQP | %^{IQP} |"
                      "| MQP | %^{MQP} |"
                      "| Humanities Project | %^{HUA Project} |"
                      "| Clubs and Organizations | %^{Clubs and Organizations} |"
                      "| House Positions | %^{Positions} |"
                      "| Nicknames | %^{Nicknames} %?|")
                    "\n")))
   org-refile-targets `((nil . (:level . 1))
                        (,(org-agenda-files) . (:maxlevel . 1)))))
;;;;; Org Super Agenda
(use-package org-super-agenda
  :after org-agenda
  :demand t
  :disabled t
  :config
  (setq org-super-agenda-groups
        '((:name "Today"
                 :time-grid t
                 :scheduled today
                 ;; Scheduled tasks will only be shown on day.
                 :scheduled future)
          (:name "Due today"
                 :deadline today)
          (:name "Important"
                 :priority "A")
          (:name "Overdue"
                 :deadline past)
          (:name "Due soon"
                 :deadline future)
          (:name "Other items"
                 :priority<= "B")))
  (org-super-agenda-mode))
;;;; Org habit
(use-feature org-habit
  :after org-agenda
  :config
  (add-to-list 'org-modules 'org-habit))
;;;; Org ox-latex
(use-feature ox-latex
  :config
  (add-to-list 'org-latex-classes
               '("paper" "\\documentclass[12pt]{report} \\PassOptionsToPackage{hyphens}{url} \n [DEFAULT-PACKAGES] [PACKAGES] [EXTRA] \n \\usepackage{setspace} \\doublespacing \\usepackage{fontspec} \\setmainfont{Times Newer Roman} \\usepackage[margin=1.25in]{geometry}"
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-classes
               '("per-file-class"
                 "\\documentclass{moderncv}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-src-fontify-natively t
        org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  :custom
  (org-format-latex-options '(:foreground default
                                          :background default
                                          :scale 2.5
                                          :html-foreground "Black"
                                          :html-background "Transparent"
                                          :html-scale 1.0 :matchers
                                          ("begin" "$1" "$" "$$" "\\(" "\\["))))
;;;; Org recur
(use-package org-recur
  :after org-agenda
  :disabled t
  :hook ((org-mode . org-recur-mode)
         (org-agenda-mode . org-recur-agenda-mode))
  :config
  (define-key org-recur-mode-map (kbd "C-c d") 'org-recur-finish)

  ;; Rebind the 'd' key in org-agenda (default: `org-agenda-day-view').
  (define-key org-recur-agenda-mode-map (kbd "d") 'org-recur-finish)
  (define-key org-recur-agenda-mode-map (kbd "C-c d") 'org-recur-finish)

  (setq org-recur-finish-done t
        org-recur-finish-archive t))
;;;; Org Latex
(use-package calctex
  :disabled t)

;; Auto toggle latex fragments
(use-package org-fragtog
  :hook ((org-mode . org-fragtog-mode)))
;; Use CDLaTeX for common shortcuts
(use-package cdlatex
  :disabled t
  :hook (org-mode . turn-on-org-cdlatex))
;;; Reading
;;;; PDF
(use-package pdf-tools
  :hook (after-init . pdf-loader-install))
;;; AUCtex
(use-package auctex)
;;; QREncode
(defun qr-code-region (start end)
  "Show a QR code of the region."
  (interactive "r")
  (let ((buf (get-buffer-create "*QR*"))
	(inhibit-read-only t)
        (display-buffer-base-action)
        (display-buffer-overriding-action '(display-buffer-same-window)))
    (with-current-buffer buf
      (erase-buffer))
    (let ((coding-system-for-read 'raw-text))
      (shell-command-on-region start end "qrencode -o -" buf))
    (switch-to-buffer buf)
    (image-mode)
    (if (> (window-width) (window-height))
        (image-transform-fit-to-height)
      (image-transform-fit-to-width))))
;;; Calc
(use-feature calc
  :bind ("C-c c" . calc))
;;; Feature discovery
(use-package which-key
  :config
  (which-key-mode 1))

(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))
;;; Miscelenious
;;;; y-or-n-p
;; Replace `yes-or-no-p' with `y-or-n-p`, as I cannot be bothered to
;; type 2 or 3 characters.
(defalias 'yes-or-no-p 'y-or-n-p)
;;; init.el ends here
