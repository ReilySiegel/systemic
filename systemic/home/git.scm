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

(define (emacs-extension config)
  (emacs-configuration-extension
   ("git"
    (setopt user-full-name "Reily Siegel")
    (setopt user-mail-address "mail@reilysiegel.com"))
   (emacs-magit
    (keymap-global-set "C-x g" #'magit-status)
    (keymap-global-set "C-x M-g" #'magit-dispatch)

    (require 'project)
    ;; Remove VC-DIR from project keymap
    (keymap-unset project-prefix-map "v")
    (assq-delete-all 'project-vc-dir project-switch-commands)
    ;; Add magit in its place.
    (keymap-set project-prefix-map "m" #'magit-project-status)
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)


    (setopt magit-display-buffer-function
            (function magit-display-buffer-same-window-except-diff-v1)))
   (emacs-magit-todos
    (with-eval-after-load 'magit
      (magit-todos-mode 1)))
   (emacs-magit-email)))

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
       ((gpgsign . #t)))
      (diff
       ((algorithm . "histogram")))
      (format
       ((signature . "")))))))

(define systemic-git-service-type
  (service-type
   (name 'systemic-git)
   (default-value #f)
   (extensions
    (list
     (service-extension home-emacs-service-type emacs-extension)
     (service-extension home-git-service-type git-extension)))))
