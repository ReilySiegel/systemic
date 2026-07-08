(define-module (systemic home shell)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (rde home services emacs)
  ;; TODO: Update use rde ghostel when updated
  #:use-module (systemic packages ghostty)
  #:use-module (systemic home emacs-utils)
  #:export (systemic-shell-service-type))

(define (emacs-extension _)
  (emacs-configuration-extension
   ("tramp"
    (with-eval-after-load 'tramp
      (setopt tramp-show-ad-hoc-proxies t)))
   (emacs-ghostel
    (keymap-global-set
     "C-c s"
     (lambda ()
       (interactive)
       (let* ((default-directory "~"))
         (ghostel))))

    (with-eval-after-load 'project
      (keymap-unset project-prefix-map "e")
      (assq-delete-all 'project-eshell project-switch-commands)
      (keymap-set project-prefix-map "s" #'ghostel-project)
      (add-to-list 'project-switch-commands '(ghostel-project "Shell") t)))
   (emacs-buffer-env
    (setopt buffer-env-script-name "guix.scm")
    (add-hook 'hack-local-variables-hook #'buffer-env-update)
    (add-hook 'comint-mode-hook #'buffer-env-update))
   (emacs-inheritenv
    (with-eval-after-load 'comint
      (require 'inheritenv)
      (inheritenv-add-advice #'make-comint)))))


(define systemic-shell-service-type
  (service-type
   (name 'systemic-shell)
   (description "Shell configuration")
   (default-value #f)
   (extensions
    (list
     (service-extension home-emacs-service-type emacs-extension)))))
