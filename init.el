;;; init.el --- Init File -*- lexical-binding: t; byte-compile-warnings: nil;-*-

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
;;; Saving files
;; Don't make backup files.
(setq make-backup-files nil)

;; Don't make autosave files.
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
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
;;;; Indentation
;; Don't use tabs for indentation. Use only spaces. Frankly, the fact
;; that `indent-tabs-mode' is even *available* as an *option* disgusts
;; me, much less the fact that it's *enabled* by default (meaning that
;; *both* tabs and spaces are used at the same time).
(setq-default indent-tabs-mode nil)

;;;; Paredit
(use-package paredit
  :hook (lisp-mode . enable-paredit-mode))
;;;; Yasnippet
(use-package yasnippet-snippets)

(use-package yasnippet
  :defer 5
  :config
  (yas-global-mode)) 
;;; Language Support
;;;; Gnuplot
(use-package gnuplot)
;;;; Java
(use-package lsp-java
  :hook (java-mode . lsp)
  :config
  (require 'dap-java))
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
               '("paper" "\\documentclass[12pt]{report}
               \\PassOptionsToPackage{hyphens}{url} \n
               [DEFAULT-PACKAGES] [PACKAGES] [EXTRA] \n
               \\usepackage{setspace} \\doublespacing
               \\usepackage{fontspec}
               \\usepackage[margin=1.25in]{geometry}
               \\usepackage{xurl}
               \\usepackage[section]{placeins}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")))
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
