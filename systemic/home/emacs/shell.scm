(define-module (systemic home emacs shell)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu services)
  #:use-module (systemic home emacs-utils)
  #:export (service))

(define service
  (simple-service
   'emacs-eshell-service
   home-emacs-service-type
   (emacs-configuration-extension
    ("eshell"
     (keymap-global-set "C-c s" #'eshell)
     (with-eval-after-load 'eshell
       (setopt eshell-destroy-buffer-when-process-dies t
               eshell-history-size 1024
               remote-file-name-inhibit-cache nil
               vc-ignore-dir-regexp
               (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp)
               eshell-visual-commands '("htop" "nmtui" "vim" "watch")))))))
