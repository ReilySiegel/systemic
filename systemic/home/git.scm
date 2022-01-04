(define-module (systemic home git)
  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (systemic home emacs-utils)
  #:use-module (systemic packages emacs-xyz)
  #:export (systemic-git-service-type))

(define emacs-git-configuration
  (elisp-configuration-package
   "git"
   `((setq user-full-name "Reily Siegel")
     (setq user-mail-address "mail@reilysiegel.com")

     (global-set-key (kbd "C-x g") 'magit-status)
     (global-set-key (kbd "C-x M-g") 'magit-dispatch)

     (with-eval-after-load
      'magit
      (setq magit-display-buffer-function
            (function magit-display-buffer-same-window-except-diff-v1))
      (magit-todos-mode 1)

      (require 'magit-extras)
      (require 'git-email))

     (with-eval-after-load
      'git-email
      (require 'git-email-magit)
      (git-email-notmuch-mode 1)
      (setq git-email-headers '(subject cc in-reply-to message-id references))

      (defvar my/git-email-to)
      (advice-add 'git-email--compose-email 
                  :before
                  (lambda (patch)
                    (let ((to (with-temp-buffer
                               (insert-file-contents patch)
                               (git-email--extract-header 'to))))
                      (setq my/git-email-to
                            (and (not (string-equal to "")) to)))))

      (add-to-list 'git-email-get-to-address-functions
                   (lambda nil my/git-email-to))))
   #:elisp-packages (list emacs-magit emacs-magit-todos emacs-git-email)
   #:autoloads? #t))

(define (emacs-extension config)
  (home-emacs-extension
   (elisp-packages
    (list emacs-git-configuration))))

(define (git-extension config)
  (home-git-extension
   (config
    `((user
       ((name . "Reily Siegel")
        (email . "mail@reilysiegel.com")))
      (gpg
       ((program . ,(file-append gnupg "/bin/gpg"))))
      (sendmail
       ((annotate . #t)))
      (commit
       ((gpgsign . #t)))))))

(define systemic-git-service-type
  (service-type
   (name 'systemic-git)
   (default-value #f)
   (extensions
    (list
     (service-extension home-emacs-service-type emacs-extension)
     (service-extension home-git-service-type git-extension)))))
