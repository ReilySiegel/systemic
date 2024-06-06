(define-module (systemic home emacs-utils)
  #:use-module (guix packages)
  #:use-module (rde home services emacs)
  #:export (emacs-configuration-extension))

(define-syntax-rule (emacs-configuration-extension (package exp ...) ...)
  (home-emacs-extension
   (elisp-packages (filter package? (list package ...)))
   (init-el (quasiquote (exp ... ...)))))
