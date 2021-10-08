;; -*- mode: scheme -*-

(define-module (home))

(use-modules
 (gnu home)
 (gnu home-services emacs)
 (gnu home-services gnupg)
 (gnu home-services mail)
 (gnu home-services password-utils)
 (gnu home services shells)
 (gnu home services shepherd)
 (gnu home-services ssh)
 (gnu home-services version-control)
 (gnu home-services-utils)
 (gnu home services)
 (gnu services)
 (gnu packages compton)
 (gnu packages gnupg)
 (gnu packages linux)
 (gnu packages mail)
 (gnu packages pulseaudio)
 (guix gexp)
 (srfi srfi-26)
 (systemic home desktop)
 (systemic home emacs)
 (systemic home mail))

(home-environment
 (services
  (list
   (service picom-service-type #f)
   (simple-service 'syncthing
                   home-shepherd-service-type
                   (list (shepherd-service
                          (documentation "Run Syncthing.")
                          (provision '(syncthing))
                          (start #~(make-forkexec-constructor '("syncthing"
                                                                "-no-browser"
                                                                "-no-restart")))
                          (stop #~(make-kill-destructor)))))
   isync-service
   notmuch-service
   notmuch-cron-service 
   (service home-gnupg-service-type
	    (home-gnupg-configuration
             (gpg-agent-config
              (home-gpg-agent-configuration
               (ssh-agent? #t)
               (pinentry-flavor 'emacs)))))
   (service home-git-service-type
	    (home-git-configuration
	     (config
	      `((core
                 ((checkStat . "minimal")))
                (user
                 ((name . "Reily Siegel")
                  (email . "mail@reilysiegel.com")))
	        (gpg
		 ((program . ,(file-append gnupg "/bin/gpg"))))
	        (sendmail
		 ((annotate . #t)))
                (commit
                 ((gpgsign . #t)))
                (lfs
                 ((storage . "/home/reily/.local/share/lfs/")))))
             (config-extra-content (slurp-file-gexp
                                    (local-file "./gitconfig")))))
   (service home-ssh-service-type
            (home-ssh-configuration
             (toplevel-options
              '((match . "host * exec \"gpg-connect-agent UPDATESTARTUPTTY /bye\"")))))
   (service home-bash-service-type
            (home-bash-configuration))
   (service home-emacs-service-type
            (home-emacs-configuration
             (rebuild-elisp-packages? #f)
             (elisp-packages emacs-packages)
             (init-el
              (list (slurp-file-gexp (local-file "./init.el")))))))))
