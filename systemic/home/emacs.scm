(define-module (systemic home emacs)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages statistics)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix transformations)
  #:use-module (rrr packages emacs-xyz)
  #:use-module (systemic home emacs-utils)
  #:use-module ((systemic home emacs completion) #:prefix completion:)
  #:use-module ((systemic home emacs org) #:prefix org:)
  #:use-module ((systemic home emacs pass) #:prefix pass:)
  #:use-module (systemic packages emacs)
  #:use-module (systemic packages emacs-xyz)
  #:export (services))

(define theme-service
  (simple-service
   'emacs-theme-service
   home-emacs-service-type
   (emacs-configuration-extension
    ("font"
     (set-frame-font "Hack-11"))
    (emacs-doom-themes
     (load-theme 'doom-nord t)
     ;; Fix for issue with tab-bar-mode
     (custom-set-faces
      `(tab-bar
        ((t (:background ,(doom-color 'bg-alt) :foreground ,(doom-color 'fg-alt)))))))
    (emacs-doom-modeline
     (require 'doom-modeline)

     ;; Use default battery display.
     (remove-hook 'display-battery-mode-hook #'doom-modeline-override-battery-modeline)
     (remove-hook 'doom-modeline-mode-hook #'doom-modeline-override-battery-modeline)

     (setopt display-time-24hr-format t
             display-time-day-and-date t
             display-time-interval 5
             display-time-default-load-average nil
             battery-update-interval 5
             battery-mode-line-format "[%p%% %b%t] "
             doom-modeline-enable-word-count nil
             doom-modeline-buffer-file-name-style 'truncate-with-project
             doom-modeline-enable-word-count t
             doom-modeline-percent-position nil
             doom-modeline-buffer-encoding nil
             doom-modeline-major-mode-icon nil
             doom-modeline-buffer-modification-icon nil
             doom-modeline-buffer-state-icon nil)

     ;; Use tab-bar as global modeline.
     (setopt tab-bar-format '(tab-bar-format-align-right tab-bar-format-global)
             tab-bar-mode t)

     (doom-modeline-mode)
     (display-time-mode)
     (display-battery-mode)))))

(define emacs-lsp-java-transform
  (options->transformation
   '((with-branch . "emacs-lsp-java=master"))))

(define packages
  (list
   emacs-use-package emacs-esup
   emacs-avy emacs-no-littering
   emacs-company emacs-outshine emacs-aggressive-indent
   emacs-flycheck emacs-lsp-mode emacs-lsp-ui emacs-dap-mode
   emacs-paredit emacs-yasnippet
   emacs-yasnippet-snippets emacs-clojure-mode emacs-cider emacs-gnuplot
   (emacs-lsp-java-transform emacs-lsp-java)
   emacs-ess
   emacs-esup  emacs-flyspell-correct emacs-racket-mode emacs-geiser
   emacs-yaml-mode
   emacs-org-fragtog emacs-pdf-tools emacs-auctex
   emacs-discover-my-major emacs-guix
   emacs-clj-refactor emacs-origami-el))

(define emacs
  (service home-emacs-service-type
           (home-emacs-configuration
            (package emacs-next-no-pgtk)
            (rebuild-elisp-packages? #f)
            (elisp-packages packages)
            (init-el
             `(;; #' exports a scheme (syntax ...) form. Treat this as a
               ;; (function ...) form.
               (defalias 'syntax 'function)
               
               (setq inhibit-startup-screen t)
               (when (display-graphic-p)
                 (menu-bar-mode -1)
                 (tool-bar-mode -1)
                 (scroll-bar-mode -1)
                 (line-number-mode -1)
                 (winner-mode 1)
                 (show-paren-mode 1))

               (setq-default fill-column 80
                             undo-limit (* 10 1024 1024)
                             browse-url-browser-function #'browse-url-chromium)

               ;; Bump the required security level for TLS to an acceptably
               ;; modern value.
               (require 'gnutls)
               (setq gnutls-verify-error t
                     gnutls-min-prime-bits 3072)

               ;; Replace `yes-or-no-p' with `y-or-n-p`, as I cannot be
               ;; bothered to type 2 or 3 characters.
               (defalias 'yes-or-no-p 'y-or-n-p)

               ;; FIXME: Waiting on upstream emacs-next-pgtk fix
               (require 'tramp)
               (setopt tramp-remote-path '(tramp-default-remote-path
                                           "~/.guix-profile/bin" "~/.guix-profile/sbin"
                                           "/run/current-system/profile/bin"
                                           "/run/current-system/profile/sbin"))
               ,(slurp-file-gexp (local-file "../../init.el"))))
            (early-init-el
             '((set 'gc-cons-threshold most-positive-fixnum)
               (run-at-time "20 sec" nil
                            (lambda ()
                              (set 'gc-cons-threshold 800000)))
               
               (setq package-enable-at-startup nil)
               
               (defvar my/file-name-handler-alist file-name-handler-alist)
               (setq file-name-handler-alist nil)
               (add-hook 'emacs-startup-hook
                         (lambda ()
                           (setq file-name-handler-alist
                                 my/file-name-handler-alist))))))))

(define services
  (list
   emacs
   theme-service
   org:service
   completion:service
   pass:service))
