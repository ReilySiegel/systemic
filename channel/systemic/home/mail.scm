(define-module (systemic home mail)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services mail)
  #:use-module (gnu home-services mcron)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (systemic home emacs-utils)
  #:use-module (systemic pass)
  #:export (isync-service
            notmuch-service
            notmuch-cron-service
            notmuch-emacs-configuration))

(define isync-service
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
               (Near ":school-local:drafts"))))))


(define notmuch-service
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
                ((ignore ".mbsyncstate,.uivalidity"))))))))

(define notmuch-cron-service
  (simple-service 'notmuch-cron
                  home-mcron-service-type
                  (list #~(job "*/5 * * * *" "notmuch new"))))

(define notmuch-emacs-configuration
  (elisp-configuration-package
   "notmuch"
   `((global-set-key (kbd "C-c m") 'notmuch)
     
     (defun reily/select-smtp-send-it ()
       (let* ((from (message-fetch-field "From"))
              (smtp (cond
                     ((string-match "mail@reilysiegel.com" from)
                      '("smtp.mailbox.org" "mail@reilysiegel.com" 465 ssl))
                     ((string-match "rsiegel@wpi.edu" from)
                      '("smtp.office365.com" "rsiegel@wpi.edu" 587 starttls)))))
         (setq smtpmail-smtp-server (car smtp)
               smtpmail-smtp-user (cadr smtp)
               smtpmail-smtp-service (caddr smtp)
               smtpmail-stream-type (cadddr smtp))
         (smtpmail-send-it)))

     (setq message-send-mail-function 'reily/select-smtp-send-it
           starttls-use-gnutls t
           message-cite-reply-position 'below
           message-kill-buffer-on-exit t
           notmuch-search-oldest-first nil
           notmuch-show-logo nil
           notmuch-hello-sections '(notmuch-hello-insert-header
                                    notmuch-hello-insert-saved-searches)
           notmuch-fcc-dirs '(("mail@reilysiegel.com" . "personal/sent"))
           notmuch-saved-searches
           '((:name "[i]nbox" :query "tag:unread AND tag:inbox" :key "i")
             (:name "[w]atching" :query "tag:unread AND thread{tag:flagged}" :key "w")
             ;;Show unread automated messages
             (:name "auto[m]ated" :query "tag:unread AND tag:automated" :key "m")
             ;;Show unread promotional messages
             (:name "[p]romotional" :query "tag:unread AND tag:promotional" :key "p")
             (:name "[f]lagged" :query "tag:flagged" :key "f")
             (:name "[s]ent" :query "tag:sent" :key "s")
             (:name "[d]rafts" :query "tag:draft" :key "d")
             (:name "[a]ll mail" :query "*" :key "a"))))
   #:elisp-packages (list emacs-notmuch)
   #:autoloads? #t))
