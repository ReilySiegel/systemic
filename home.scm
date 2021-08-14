;; -*- mode: scheme -*-
(define-module (home))

(use-modules
 (gnu home)
 (gnu home-services base)
 (gnu home-services emacs)
 (gnu home-services files)
 (gnu home-services gnupg)
 (gnu home-services mail)
 (gnu home-services mcron)
 (gnu home-services password-utils)
 (gnu home-services shells)
 (gnu home-services shepherd)
 (gnu home-services ssh)
 (gnu home-services version-control)
 (gnu home-services-utils)
 (gnu home-services)
 (gnu packages compton)
 (gnu packages gnupg)
 (gnu packages linux)
 (gnu packages mail)
 (gnu packages pulseaudio)
 (guix gexp)
 (ice-9 popen)
 (ice-9 rdelim)
 (srfi srfi-26)
 (systemic home emacs))

(define (pass service)
  (let* ((port (open-input-pipe (string-append "pass " service)))
         (str (read-line port)))
    (close-pipe port)
    str))

(home-environment
 (services
  (list
   (home-generic-service
    'picom
    #:files `(("config/picom/picom.conf"
               ,(local-file "./picom.conf")))
    #:packages (list picom)
    #:extensions
    `((,home-shepherd-service-type
       .
       ,(list
         (shepherd-service
          (documentation "Run picom.")
          (provision '(picom))
          (start #~(make-system-constructor "picom -b"))
          (stop #~(make-system-destructor "pkill picom -SIGKILL")))))))
   (simple-service 'syncthing
                   home-shepherd-service-type
                   (list (shepherd-service
                          (documentation "Run Syncthing.")
                          (provision '(syncthing))
                          (start #~(make-forkexec-constructor '("syncthing"
                                                                "-no-browser"
                                                                "-no-restart")))
                          (stop #~(make-kill-destructor)))))
   (service home-isync-service-type
            (home-isync-configuration
             (config
              `((Create Both)
                (Expunge Both)
                (SyncState *)
                ,#~""
                (IMAPAccount personal)
                (Host "imap.mailbox.org")
                (User "mail@reilysiegel.com")
                (Pass ,(pass "mailbox.org"))
                (SSLType "IMAPS")
                ,#~""
                (IMAPStore personal-remote)
                (Account personal)
                ,#~"" 
                (MaildirStore personal-local)
                (Path "~/.mail/personal/")
                (Inbox "~/.mail/personal/inbox")
                ,#~"" 
                (Channel personal-inbox)
                (Far ":personal-remote:INBOX")
                (Near ":personal-local:inbox")
                ,#~"" 
                (Channel personal-sent)
                (Far ":personal-remote:Sent")
                (Near ":personal-local:sent")
                ,#~""
                (Channel personal-drafts)
                (Far ":personal-remote:Drafts")
                (Near ":personal-local:drafts")
                ,#~""
                (IMAPAccount school)
                (Host "outlook.office365.com")
                (User "rsiegel@wpi.edu")
                (Pass ,(pass "wpi"))
                (Port 993)
                (SSLType "IMAPS")
                ,#~""
                (IMAPStore school-remote)
                (Account school)
                ,#~"" 
                (MaildirStore school-local)
                (Path "~/.mail/school/")
                (Inbox "~/.mail/school/inbox")
                (SubFolders Verbatim)
                ,#~"" 
                (Channel school-inbox)
                (Far ":school-remote:INBOX")
                (Near ":school-local:inbox")
                ,#~"" 
                (Channel school-sent)
                (Far ":school-remote:Sent Items")
                (Near ":school-local:sent")
                ,#~"" 
                (Channel school-drafts)
                (Far ":school-remote:Drafts")
                (Near ":school-local:drafts")))))
   (service home-notmuch-service-type
            (home-notmuch-configuration
             (pre-new
              (list
               #~(system "mbsync -a")))
             (post-new
              (list
               ;; Tag messages with unsubscribe options as promotional
               #~(system "notmuch tag +promotional -inbox unsubscribe")
               ;; Tag messages from noreply as automated
               ;; Use reply to catch all variations
               #~(system "notmuch tag +automated -inbox from:reply")
               #~(system "notmuch tag +automated -inbox from:noreply")
               #~(system "notmuch tag +automated -inbox from:donotreply")
               ;; Mail sent by me is sent
               #~(system "notmuch tag +sent -inbox from:mail@reilysiegel.com")
               #~(system "notmuch tag +sent -inbox from:rsiegel@wpi.edu")
               ;; Remove unread from mail in an ignored thread.
               #~(system "notmuch tag -unread tag:unread AND thread:{tag:ignore}")))
             (config
              '((user
                 ((name "Reily Siegel")
                  (primary_email "mail@reilysiegel.com")
                  (other_email "rsiegel@wpi.edu")))
                (database
                 ((path "/home/reily/.mail")
                  (mail_root "/home/reily/.mail")))
                (maildir
                 ((synchronize_flags "true")))
                (new
                 ((ignore ".mbsyncstate,.uivalidity")))))))
   (simple-service 'notmuch-cron
                   home-mcron-service-type
                   (list #~(job "*/5 * * * *" "notmuch new")))
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
