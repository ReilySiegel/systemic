(define-module (systemic home emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (systemic home desktop)
  #:use-module (systemic home mail)
  #:use-module (systemic home emacs-utils)
  #:use-module (systemic home emacs org)
  #:use-module (systemic packages emacs-xyz)
  #:export (emacs-packages))


(define default-configuration
  (elisp-configuration-package
   "defaults"
   `(;; Use reasonable defaults
     (setq inhibit-startup-screen t)
     (when (display-graphic-p)
       (menu-bar-mode -1)
       (tool-bar-mode -1)
       (scroll-bar-mode -1)
       (winner-mode 1)
       (show-paren-mode 1))

     (setq-default fill-column 80
                   undo-limit (* 10 1024 1024))

     ;; Bump the required security level for TLS to an acceptably modern value.
     (require 'gnutls)
     (setq gnutls-verify-error t
           gnutls-min-prime-bits 3072)

     ;; Replace `yes-or-no-p' with `y-or-n-p`, as I cannot be bothered to
     ;; type 2 or 3 characters.
     (defalias 'yes-or-no-p 'y-or-n-p))
   #:autoloads? #t))

(define theme-configuration
  (elisp-configuration-package
   "theme"
   `((set-frame-font "Fira Code-11")

     (require 'nord-theme)
     (if (daemonp)
         (add-hook 'after-make-frame-functions
		   (lambda (frame)
		     (select-frame frame)
		     (load-theme 'nord t)))
         (load-theme 'nord t))

     (line-number-mode 0)

     (require 'time)
     (require 'doom-modeline)

     (doom-modeline-mode 1)
     
     (setq display-time-24hr-format t
           display-time-interval 5
           display-time-default-load-average nil
           doom-modeline-enable-word-count nil
           doom-modeline-buffer-file-name-style 'truncate-with-project
           doom-modeline-enable-word-count t
           doom-modeline-percent-position nil
           doom-modeline-buffer-encoding nil
           doom-modeline-major-mode-icon nil
           doom-modeline-buffer-modification-icon nil
           doom-modeline-buffer-state-icon nil)
     (display-time-mode 1)
     (display-battery-mode 1))
   #:elisp-packages (list emacs-doom-modeline
                          emacs-nord-theme)
   #:autoloads? #t))

(define emacs-packages
  (list
   emacs-use-package emacs-vertico emacs-orderless
   emacs-marginalia emacs-consult
   emacs-avy emacs-no-littering
   emacs-company emacs-git-auto-commit-mode emacs-outshine emacs-aggressive-indent
   emacs-flycheck emacs-lsp-mode emacs-lsp-ui emacs-dap-mode emacs-magit
   emacs-magit-todos emacs-forge emacs-paredit emacs-yasnippet
   emacs-yasnippet-snippets emacs-clojure-mode emacs-cider emacs-gnuplot
   emacs-lsp-java emacs-esup  emacs-flyspell-correct emacs-racket-mode emacs-geiser
   emacs-yaml-mode emacs-plantuml-mode emacs-org
   emacs-org-fragtog emacs-pdf-tools emacs-auctex emacs-which-key
   emacs-discover-my-major emacs-guix emacs-git-email
   emacs-clj-refactor emacs-origami-el emacs-org-roam emacs-org-transclusion
   emacs-pass emacs-project
   default-configuration
   theme-configuration
   exwm-configuration
   notmuch-emacs-configuration
   org-agenda-configuration
   org-minutes-configuration))
