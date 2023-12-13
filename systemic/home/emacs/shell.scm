(define-module (systemic home emacs shell)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu packages emacs-xyz)
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
       (setopt eshell-history-size 4096
               eshell-hist-ignoredups t
               remote-file-name-inhibit-cache nil
               vc-ignore-dir-regexp
               (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))))
    ("tramp"
     (with-eval-after-load 'tramp
       (setopt tramp-show-ad-hoc-proxies t
               tramp-ssh-controlmaster-options
               (concat
                "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                "-o ControlMaster=auto -o ControlPersist=yes"))))
    (emacs-eat
     (setopt eshell-visual-commands nil
             ;; Better remote compatibility.
             eat-term-name "xterm-256color")
     (add-hook 'eshell-load-hook #'eat-eshell-mode)))))
