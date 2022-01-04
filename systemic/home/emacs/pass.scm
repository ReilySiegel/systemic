(define-module (systemic home emacs pass)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages password-utils)
  #:use-module (systemic home emacs-utils)
  #:use-module (systemic packages emacs-xyz)
  #:export (pass-configuration))

(define pass-configuration
  (elisp-configuration-package
   "pass"
   '()
   #:elisp-packages (list emacs-pass
                          emacs-password-store-otp
                          pass-otp
                          qrencode
                          imagemagick
                          zbar)))
