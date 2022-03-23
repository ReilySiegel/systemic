;;; init.el --- Init File -*- lexical-binding: t; byte-compile-warnings: nil; eval: (outline-hide-body)-*-

;;; Commentary:
;; Nothing to see here...
;;; Code:
;; This section makes the linter happy.
;;; Packaging
;;;; use-package
(require 'use-package)

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
  :bind ("C-c g" . guix))
;;; Configure ~/.emacs.d paths
;; Package `no-littering' changes the default paths for lots of
;; different packages, with the net result that the ~/.emacs.d folder
;; is much more clean and organized.
(use-package no-littering
  :demand t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file))
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
  :defer 5
  :custom
  (company-idle-delay t)
  :config
  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (setq company-backends (mapcar #'company-mode/backend-with-yas
                                 company-backends))
  (global-company-mode))

;;;; Indentation
;; Don't use tabs for indentation. Use only spaces. Frankly, the fact
;; that `indent-tabs-mode' is even *available* as an *option* disgusts
;; me, much less the fact that it's *enabled* by default (meaning that
;; *both* tabs and spaces are used at the same time).
(setq-default indent-tabs-mode nil)

(use-package aggressive-indent
  :hook ((prog-mode . aggressive-indent-mode)
         (java-mode . (lambda nil (aggressive-indent-mode -1))))
  :init
  (electric-indent-mode -1))

;;;; Linting
(use-package flycheck
  :defer 10
  :config
  ;; Enable proselint for emails.
  (flycheck-add-mode 'proselint 'mu4e-compose-mode)
  (global-flycheck-mode))
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
;;;; Paredit
(use-package paredit
  :hook (prog-mode . enable-paredit-mode))
;;;; Yasnippet
(use-package yasnippet-snippets) 

(use-package yasnippet
  :defer 5
  :config
  (yas-global-mode)) 
;;; Language Support
;;;; Clojure
(use-package clojure-mode
  :custom
  (clojure-align-forms-automatically t)
  (clojure-toplevel-inside-comment-form t)
  :config
  ;; Define indentation for guardrails macros
  (put-clojure-indent '>def :defn)
  (put-clojure-indent '>defn :defn)
  (put-clojure-indent '>defn- :defn)
  (put-clojure-indent '>fdef :defn)

  ;; Define docstring positions for guardrails macros
  (put '>def 'clojure-doc-string-elt 2)
  (put '>defn 'clojure-doc-string-elt 2)
  (put '>defn- 'clojure-doc-string-elt 2))

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

(use-feature geiser-guile
  :config
  (add-to-list 'geiser-guile-load-path
               "~/.config/guix/current/share/guile/site/3.0/"))
;;; Eshell
(use-feature eshell
                              :bind ("C-c s" . eshell)
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
        (setq erc-prompt-for-password nil
        erc-default-server "irc.libera.chat"))
;;; Pass
(use-feature auth-source-pass
          :defer 1
          :config
          (auth-source-pass-enable))
(use-feature ox-latex
  :config
  (add-to-list 'org-latex-classes
               '("paper" "\\documentclass[12pt]{report} \\PassOptionsToPackage{hyphens}{url} \n [DEFAULT-PACKAGES] [PACKAGES] [EXTRA] \n \\usepackage{setspace} \\doublespacing \\usepackage{fontspec} \\usepackage[margin=1.25in]{geometry}"
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
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

;; Auto toggle latex fragments
(use-package org-fragtog
  :hook ((org-mode . org-fragtog-mode)))

(use-package org-transclusion
  :bind (("C-c n t" . org-transclusion-mode)))
;;; Reading
;;;; PDF
(use-package pdf-tools
  :defer 20
  :config
  (pdf-loader-install))
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
(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))
;;; init.el ends here
