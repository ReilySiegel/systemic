(define-module (systemic home csharp)
  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (systemic home emacs-utils)
  #:export (services))

(define services
  (list
   (simple-service 'csharp-emacs home-emacs-service-type
                   (emacs-configuration-extension
                    (emacs-csharp-mode)
                    (emacs-eglot
                     (add-hook 'csharp-mode-hook #'eglot-ensure)
                     (with-eval-after-load 'eglot
                       (add-to-list 'eglot-server-programs
                                    `(csharp-mode . ("omnisharp" "-lsp")))))))))
