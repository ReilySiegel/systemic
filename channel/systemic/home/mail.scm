(define-module (systemic home mail)
  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services mail)
  #:use-module (gnu home services mcron)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (systemic home emacs-utils)
  #:use-module (systemic pass)
  #:export (systemic-mail-service-type))

(define (isync-extension config)
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
    (Near ":school-local:drafts")))


(define (notmuch-extension config)
  (home-notmuch-extension
   (pre-new
    (list
     #~(system "mbsync -a")))
   (post-new
    (list
     #~(system "afew -tnC ~/.config/notmuch/default/config")
     ;; Tag messages with unsubscribe options as promotional
     #~(system "notmuch tag +promotional -inbox unsubscribe")
     ;; Tag messages from noreply as automated
     ;; Use reply to catch all variations
     #~(system "notmuch tag +automated -inbox from:reply")
     #~(system "notmuch tag +automated -inbox from:noreply")
     #~(system "notmuch tag +automated -inbox from:donotreply")
     ;; Messages to mailing lists should not be in inbox unless they are to me
     #~(system "notmuch tag -inbox tag:lists AND NOT tag:to-me")))
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
       ((ignore ".mbsyncstate,.uivalidity")
        (tags "new")))))))

(define emacs-notmuch-configuration
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

     (setq
      message-send-mail-function 'reily/select-smtp-send-it
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
        (:name "[l]ists" :query "tag:unread AND tag:lists" :key "l")
        ;;Show unread automated messages
        (:name "auto[m]ated" :query "tag:unread AND tag:automated" :key "m")
        ;;Show unread promotional messages
        (:name "[p]romotional" :query "tag:unread AND tag:promotional" :key "p")
        (:name "[f]lagged" :query "tag:flagged" :key "f")
        (:name "[s]ent" :query "tag:sent" :key "s")
        (:name "[d]rafts" :query "tag:draft" :key "d")
        (:name "[a]ll mail" :query "*" :key "a")))
     ;; Notmuch abuses internal details of the CRM API, which breaks 3rd party
     ;; implementations. This should eventually be fixed upstream.
     (advice-add (function notmuch-read-tag-changes)
                 :filter-return (lambda (x) (mapcar (function string-trim) x))))
   #:elisp-packages (list emacs-notmuch)
   #:autoloads? #t))

(define (emacs-extension config)
  (home-emacs-extension
   (elisp-packages
    (list emacs-notmuch-configuration))))

(define (add-afew-config-file config)
  `(("config/afew/config"
     ,(mixed-text-file
       "config"
       "[SpamFilter]
[KillThreadsFilter]
[ListMailsFilter]
[MeFilter]
[SentMailsFilter]
sent_tag = sent
[ArchiveSentMailsFilter]
[InboxFilter]"))))

(define systemic-mail-service-type
  (service-type
   (name 'systemic-mail)
   (default-value #f)
   (extensions
    (list
     (service-extension home-emacs-service-type emacs-extension)
     (service-extension home-isync-service-type isync-extension)
     (service-extension home-notmuch-service-type notmuch-extension)
     (service-extension home-profile-service-type (lambda _ (list afew)))
     (service-extension home-mcron-service-type
                        (lambda _ (list #~(job "*/5 * * * *" "notmuch new"))))
     (service-extension home-files-service-type add-afew-config-file)))))
