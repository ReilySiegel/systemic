(define-module (systemic home emacs editing)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu services)
  #:use-module (systemic home emacs-utils)
  #:export (service))

(define service
  (simple-service
   'emacs-edititng home-emacs-service-type
   (emacs-configuration-extension
    (emacs-avy
     (keymap-global-set "C-'" 'avy-goto-char)))))
