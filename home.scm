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
 (gnu home-services)
 (gnu home-services-utils) 
 (gnu packages compton)
 (gnu packages emacs-xyz)
 (gnu packages gnupg)
 (gnu packages linux)
 (gnu packages mail)
 (gnu packages pulseaudio)
 (gnu packages)
 (gnu services)
 (guix build-system emacs)
 (guix gexp)
 (guix git-download)
 (guix packages)
 (ice-9 popen)
 (ice-9 rdelim)
 (srfi srfi-26)
 ((guix licenses) #:prefix license:))

(define emacs-git-email
  (package
    (name "emacs-git-email")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~yoctocell/git-email")
                    (commit (string-append "v" version))))
              (snippet
               ;; Not yet in Guix proper
               '(delete-file "git-email-piem.el"))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09vmh3x1rjxxl9g9p01afil1zlpk7rf0pjmzyvcbid9wczyllkhq"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-magit" ,emacs-magit)
       ("notmuch" ,notmuch)))
    (home-page "https://sr.ht/~yoctocell/git-email/")
    (synopsis "Integrates git and email with Emacs")
    (description "git-email provides functions for formatting and sending Git patches
via email, without leaving Emacs.")
    (license license:gpl3+)))

(define emacs-app-launcher
  (let ((commit "80a9ed37892ee6e21fe44487ed11f66a15e3f440")
        (revision "1"))
    (package
      (name "emacs-app-launcher")
      (version (git-version "0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/SebastienWae/app-launcher")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1ywhfx8604ifmvcy2397bmvq2wj03jyqnm0g7lmqqi5p97rjbdgc"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/SebastienWae/app-launcher")
      (synopsis "Launch system applications from emacs")
      (description "Uses Emacs standard completion feature to select an
application installed on your machine and launch it.")
      (license license:gpl3+))))

(define emacs-inflections
  (package
    (name "emacs-inflections")
    (version "2.5")
    (home-page "https://github.com/eschulte/jump.el")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ig1wdjg914p9ng1nir2fid4mb3xz2dbpmkdnfy1klq2zp0xw2s3"))
              (snippet
               '(begin (delete-file "jump.el")
                       #t))))
    (build-system emacs-build-system)
    (synopsis "Emacs utility to convert english words between singular and
plural")
    (description "Emacs utility to convert english words between singular and
plural")
    (license license:gpl3+)))

(define emacs-clj-refactor
  (package
    (name "emacs-clj-refactor")
    (version "2.5.1")
    (home-page "https://github.com/clojure-emacs/clj-refactor.el")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pyskl9xcqrk6r2zbp5i9402inngqps7wwb4nbdbrgi4di9b8in7"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("yasnippet" ,emacs-yasnippet)
       ("paredit" ,emacs-paredit)
       ("multiple-cursors" ,emacs-multiple-cursors)
       ("clojure-mode" ,emacs-clojure-mode)
       ("cider" ,emacs-cider)
       ("parseedn" ,emacs-parseedn)
       ("inflections" ,emacs-inflections)
       ("hydra" ,emacs-hydra)))
    (synopsis "Support for refactoring Clojure code in Emacs.")
    (description "@code{clj-refactor} provides refactoring support for Clojure
projects. It complements the refactoring functionality you'd find in
clojure-mode and CIDER.")
    (license license:gpl3+)))


(define emacs-packages
  (list
   emacs-nord-theme emacs-use-package emacs-exwm emacs-vertico emacs-orderless
   emacs-marginalia emacs-consult emacs-doom-modeline emacs-pinentry 
   emacs-exec-path-from-shell emacs-avy emacs-undo-tree emacs-app-launcher
   emacs-company emacs-git-auto-commit-mode emacs-outshine emacs-aggressive-indent
   emacs-flycheck emacs-lsp-mode emacs-lsp-ui emacs-dap-mode emacs-magit
   emacs-magit-todos emacs-forge emacs-paredit emacs-yasnippet
   emacs-yasnippet-snippets emacs-clojure-mode emacs-cider emacs-gnuplot
   emacs-lsp-java emacs-esup  emacs-flyspell-correct emacs-racket-mode emacs-geiser
   emacs-yaml-mode emacs-plantuml-mode emacs-org emacs-org-super-agenda
   emacs-org-fragtog emacs-pdf-tools emacs-auctex emacs-which-key
   emacs-discover-my-major emacs-no-littering emacs-guix emacs-git-email
   emacs-clj-refactor emacs-origami-el notmuch))

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
                (Host "imappro.zoho.com")
                (User "mail@reilysiegel.com")
                (Pass ,(pass "zoho"))
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
