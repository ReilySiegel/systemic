(define-module (systemic home emacs-utils)
  #:use-module (gnu home-services emacs)
  #:use-module (guix packages)
  #:export (emacs-configuration-extension))

(define-syntax-rule (emacs-configuration-extension (package exp ...) ...)
  (home-emacs-extension
   (elisp-packages (filter package? (list package ...)))
   (init-el (quasiquote (exp ... ...)))))
