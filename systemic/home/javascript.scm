(define-module (systemic home javascript)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages node)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (rde home services emacs)
  #:use-module (systemic home emacs-utils)
  #:export (services))

(define services
  (list
   (simple-service 'js-packages home-profile-service-type (list node-lts))
   (simple-service 'js-env-vars home-environment-variables-service-type
                   `(("npm_config_prefix" . "$HOME/.local")
                     ("NODE_PATH" . "$HOME/.local/lib/node_modules:$NODE_PATH")))
   (simple-service
    'javascript-emacs home-emacs-service-type
    (emacs-configuration-extension
     (emacs-typescript-mode)
     (emacs-web-mode)
     (emacs-eglot
      (add-hook 'js-mode-hook #'eglot-ensure)
      (add-hook 'typescript-mode-hook #'eglot-ensure)
      (with-eval-after-load 'eglot
        (add-to-list 'eglot-server-programs
                     (cons '(js-mode typescript-mode)
                           (eglot-alternatives
                            '(("typescript-language-server"  "--stdio")
                              ("npx" "typescript-language-server" "--stdio")))))))))))
