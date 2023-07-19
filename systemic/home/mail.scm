
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


(define %mail-address "mail@reilysiegel.com")
(define %imap-address "imap.zoho.com")
(define %smtp-address "smtppro.zoho.com")
(define %pass-entry "zoho.com")
(define %secret-location "isync/secret")

(define (isync-extension config)
  `((Create Both)
    (Expunge Both)
    (SyncState *)
    ,#~""
    (IMAPAccount personal)
    (Host ,%imap-address)
    (User ,%mail-address)
    (PassCmd ,(string-append "cat $XDG_STATE_HOME/" %secret-location))
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
    ,#~""))

(define (notmuch-extension config)
  (home-notmuch-extension
   (pre-new
    (list
     #~(system (string-append "notmuch search --output=files --format=text0"
                              " tag:deleted | "
                              "xargs -r0 rm"))
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
    `((user
       ((name "Reily Siegel")
        (primary_email ,%mail-address)))
      (database
       ((path "/home/reily/.mail")
        (mail_root "/home/reily/.mail")))
      (maildir
       ((synchronize_flags "true")))
      (new
       ((ignore ".mbsyncstate,.uivalidity")
        (tags "new")))))))

(define (emacs-extension config)
  (emacs-configuration-extension
   (emacs-notmuch
    (keymap-global-set "C-c m" 'notmuch)

    (require 'notmuch-mua)

    (setopt mail-user-agent 'notmuch-user-agent
            message-send-mail-function 'smtpmail-send-it
            starttls-use-gnutls t
            message-cite-reply-position 'below
            message-kill-buffer-on-exit t
            notmuch-search-oldest-first nil
            notmuch-show-logo nil
            notmuch-hello-sections '(notmuch-hello-insert-header
                                     notmuch-hello-insert-saved-searches)
            notmuch-fcc-dirs '((,%mail-address . "personal/sent"))
            smtpmail-smtp-server ,%smtp-address
            smtpmail-smtp-user ,%mail-address
            smtpmail-smtp-service 587
            smtpmail-stream-type 'starttls
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
                :filter-return (lambda (x) (mapcar (function string-trim) x)))

    (with-eval-after-load 'message
      (setq message-signature-file "~/.config/signature")))
   ;; HACK: Notmuch uses deprecated completion API which is not supported by
   ;; corfu.
   ("corfu"
    (with-eval-after-load 'corfu
      (add-hook 'notmuch-message-mode-hook (lambda nil (corfu-mode 0)))))
   ("meow"
    (with-eval-after-load 'meow
      (dolist (mode '(notmuch-hello-mode
                      notmuch-search-mode
                      notmuch-tree-mode
                      notmuch-show-mode))
              (add-to-list 'meow-mode-state-list (cons mode 'motion)))))))

(define (add-afew-config-file config)
  `(("afew/config"
     ,(mixed-text-file
       "config"
       "[SpamFilter]
[KillThreadsFilter]
[ListMailsFilter]
[MeFilter]
[SentMailsFilter]
sent_tag = sent
[ArchiveSentMailsFilter]
[HeaderMatchingFilter.1]
header = Subject
pattern = PATCH
tags = +patch
[HeaderMatchingFilter.2]
header = Subject
pattern = RFC
tags = +rfc
[HeaderMatchingFilter.3]
header = Subject
pattern = EXCUSE
tags = +excuse;-new
[HeaderMatchingFilter.4]
header = Subject
pattern = AGENDA
tags = +agenda;-new
[InboxFilter]"))))

(define systemic-mail-service-type
  (service-type
   (name 'systemic-mail)
   (description "A basic mail service.")
   (default-value #f)
   (extensions
    (list
     (service-extension home-emacs-service-type emacs-extension)
     (service-extension home-isync-service-type isync-extension)
     (service-extension home-notmuch-service-type notmuch-extension)
     (service-extension home-profile-service-type (lambda _ (list afew)))
     (service-extension home-mcron-service-type
                        (lambda _ (list #~(job "*/5 * * * *" "notmuch new"))))
     (service-extension home-xdg-configuration-files-service-type
                        add-afew-config-file)
     (service-extension home-activation-service-type
                        (lambda _ ((pass-activation %pass-entry)
                                   %secret-location)))))))
