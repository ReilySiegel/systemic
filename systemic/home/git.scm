(define-module (systemic home git)
  #:use-module (gnu home services)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (rde home services emacs)
  #:use-module (systemic home emacs-utils)
  #:use-module (systemic packages emacs-xyz)
  #:export (systemic-git-service-type))

(define (emacs-extension config)
  (emacs-configuration-extension
   ("git"
    (setopt user-full-name ,(car config))
    (setopt user-mail-address ,(cadr config)))
   (emacs-magit
    (keymap-global-set "C-x M-g" #'magit-status)
    (setopt magit-clone-default-directory "~/src/")

    (require 'project)
    ;; Remove VC-DIR from project keymap
    (keymap-unset project-prefix-map "v")
    (assq-delete-all 'project-vc-dir project-switch-commands)
    ;; Add magit in its place.
    (keymap-set project-prefix-map "m" #'magit-project-status)
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))
   (emacs-magit-todos
    (with-eval-after-load 'magit
      (magit-todos-mode 1)))
   (emacs-magit-email)))

(define (git-extension config)
  (home-git-extension
   (config
    `((user
       ((name . ,(car config))
        (email . ,(cadr config))))
      (gpg
       ((program . ,(file-append gnupg "/bin/gpg"))))
      (sendmail
       ((annotate . #t)))
      (commit
       ((gpgsign . ,(caddr config))))
      (diff
       ((algorithm . "histogram")))
      (format
       ((signature . "")))))))

(define systemic-git-service-type
  (service-type
   (name 'systemic-git)
   (description "A basic git service.")
   ;; TODO: Better config.
   (default-value '("Reily Siegel" "mail@reilysiegel.com" #t))
   (extensions
    (list
     (service-extension home-emacs-service-type emacs-extension)
     (service-extension home-git-service-type git-extension)))))


