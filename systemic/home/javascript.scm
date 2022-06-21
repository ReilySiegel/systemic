(define-module (systemic home javascript)
  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages node)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (systemic home emacs-utils)
  #:export (services))

(define services
  (list
   (simple-service 'js-packages home-profile-service-type (list node-lts))
   (simple-service 'javascript-emacs home-emacs-service-type
                   (emacs-configuration-extension
                    (emacs-typescript-mode)
                    (emacs-web-mode)
                    (emacs-eglot
                     (add-hook 'js-mode-hook #'eglot-ensure)
                     (add-hook 'typescript-mode-hook #'eglot-ensure)
                     (with-eval-after-load 'eglot
                       (add-to-list 'eglot-server-programs
                                    `((js-mode typescript-mode) .
                                      ("npx" "typescript-language-server" "--stdio")))))))))
