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
    ('flymake
     (add-hook 'text-mode-hook #'flymake-mode)
     (add-hook 'prog-mode-hook #'flymake-mode))
    (emacs-flymake-proselint
     (add-hook 'text-mode-hook #'flymake-proselint-setup))
    (emacs-avy
     (keymap-global-set "C-'" 'avy-goto-char)))))
