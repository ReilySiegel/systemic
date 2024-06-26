(define-module (systemic home emacs pass)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu services)
  #:use-module (rde home services emacs)
  #:export (service))

(define service
  (simple-service
   'emacs-pass-service
   home-emacs-service-type
   (home-emacs-extension
    (elisp-packages (list
                     password-store
                     pass-otp
                     emacs-pass
                     emacs-password-store-otp
                     qrencode
                     imagemagick
                     zbar)))))
