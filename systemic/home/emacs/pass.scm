(define-module (systemic home emacs pass)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages password-utils)
  #:use-module (systemic home emacs-utils)
  #:use-module (systemic packages emacs-xyz)
  #:use-module (systemic packages)
  #:export (pass-configuration))

;; HACK: Remove this once tests for password-store are fixed.
(define pass-configuration
  (without-tests
   password-store
   (elisp-configuration-package
    "pass"
    '()
    #:elisp-packages (list emacs-pass
                           emacs-password-store-otp
                           pass-otp
                           qrencode
                           imagemagick
                           zbar))))
