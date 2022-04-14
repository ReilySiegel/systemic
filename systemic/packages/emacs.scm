(define-module (systemic packages emacs)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (rrr packages emacs))

(define-public emacs-next-no-pgtk
  (package
    (inherit emacs-rrr-next)
    (name "emacs-next-no-pgtk")
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-rrr-next)
       ((#:configure-flags flags)
        `(delete "--with-native-compilation" (delete "--with-pgtk" ,flags)))))))
