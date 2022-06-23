(define-module (systemic home emacs editing)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu services)
  #:use-module (systemic home emacs-utils)
  #:use-module (systemic packages emacs-xyz)
  #:export (service))

(define service
  (simple-service
   'emacs-edititng home-emacs-service-type
   (emacs-configuration-extension
    ('flymake
     (add-hook 'text-mode-hook #'flymake-mode)
     (add-hook 'prog-mode-hook #'flymake-mode))
    (emacs-flymake-languagetool
     (setopt
      flymake-languagetool-check-spelling t
      flymake-languagetool-server-jar
      (substitute-in-file-name "$XDG_DATA_HOME/languagetool/languagetool-server.jar"))
     (add-hook 'text-mode-hook #'flymake-languagetool-load))
    ('eldoc
     (setopt eldoc-documentation-strategy 'eldoc-documentation-compose))
    (emacs-eglot
     (setopt eglot-events-buffer-size 0)
     (with-eval-after-load 'eglot
       (add-hook 'eglot--managed-mode-hook
                 (lambda nil
                   (setq-local eldoc-documentation-strategy
                               'eldoc-documentation-compose)))))
    (emacs-avy
     (keymap-global-set "C-'" 'avy-goto-char)))))
