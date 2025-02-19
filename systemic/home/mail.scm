(define-module (systemic home mail)
  #:use-module (dtao-guile configuration blocks)
  #:use-module (dtao-guile home-service)
  #:use-module (gnu home services)
  #:use-module (gnu home-services mail)
  #:use-module (gnu home services mcron)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages python-web)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (rde home services emacs)
  #:use-module (systemic home emacs-utils)
  #:export (systemic-mail-service-type

            systemic-mail-configuration
            systemic-mail-configuration?
            systemic-mail-configuration-imap
            systemic-mail-configuration-smtp
            systemic-mail-configuration-address
            systemic-mail-configuration-secret))

(define-configuration/no-serialization systemic-mail-configuration
  (address
   (string #f)
   "Mail address to use.")
  (imap
   (string #f)
   "IMAP address to use.")
  (smtp
   (string #f)
   "SMTP address to use.")
  (auth-mechs
   (string "*")
   "AuthMechs to use.")
  (secret
   (gexp #f)
   "Command returning the secret to use.")
  (notmuch-tags
   (alist '())
   "Alist of notmuch search to list of tags to add or remove."))

(define (isync-extension config)
  `((Create Both)
    (Expunge Both)
    (SyncState *)
    ,#~""
    (IMAPAccount account)
    (Host ,(systemic-mail-configuration-imap config))
    (User ,(systemic-mail-configuration-address config))
    ,#~(string-append "PassCmd \""
                      #$(systemic-mail-configuration-secret config)
                      "\"")
    (AuthMechs ,(systemic-mail-configuration-auth-mechs config))
    (TLSType "IMAPS")
    (PipelineDepth 50)
    ,#~""
    (IMAPStore remote)
    (Account account)
    ,#~"" 
    (MaildirStore local)
    (Path "~/.mail/box/")
    (Inbox "~/.mail/box/inbox")
    (SubFolders Verbatim)
    ,#~"" 
    (Channel account)
    (Far ":remote:")
    (Near ":local:")
    (Patterns *)
    ,#~""))


(define (notmuch-extension config)
  (home-notmuch-extension
   (pre-new
    (list
     #~(system (string-append #$notmuch
                              "/bin/notmuch search --output=files --format=text0"
                              " tag:deleted | "
                              "xargs -r0 rm"))
     #~(system (string-append #$isync
                              "/bin/mbsync -ac ~/.config/isync/mbsyncrc"))))
   (post-new
    (cons*
     #~(system (string-append #$afew
                              "/bin/afew -tnC ~/.config/notmuch/default/config"))

     (map (match-lambda
            ((search tags)
             #~(system (string-append #$notmuch "/bin/notmuch tag "

                                      #$(string-join tags " ") " "
                                      #$search))))
          (systemic-mail-configuration-notmuch-tags config))))
   (config
    `((user
       ((name "Reily Siegel")
        (primary_email ,(systemic-mail-configuration-address config))))
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

    (defun fetch-access-token ()
      (with-temp-buffer
       (call-process ,(file-append oauth2ms "/bin/oauth2ms")
                     nil t nil "--encode-xoauth2")
       (buffer-string)))

    (with-eval-after-load 'smtpmail
      (cl-defmethod smtpmail-try-auth-method
                    (process (_mech (eql xoauth2)) user password)
                    (let* ((access-token (fetch-access-token)))
	              (smtpmail-command-or-throw
	               process
	               (concat "AUTH XOAUTH2 " access-token)
	               235)))
      (add-to-list 'smtpmail-auth-supported 'xoauth2))

    (setopt mail-user-agent 'notmuch-user-agent
            message-send-mail-function 'smtpmail-send-it
            starttls-use-gnutls t
            message-cite-reply-position 'below
            message-kill-buffer-on-exit t
            notmuch-search-oldest-first nil
            notmuch-show-logo nil
            notmuch-fcc-dirs nil
            notmuch-draft-folder "Drafts"
            notmuch-hello-sections '(notmuch-hello-insert-header
                                     notmuch-hello-insert-saved-searches)
            smtpmail-smtp-server ,(systemic-mail-configuration-smtp config)
            smtpmail-smtp-user ,(systemic-mail-configuration-address config)
            smtpmail-smtp-service 587
            smtpmail-stream-type 'starttls
            notmuch-saved-searches
            '((:name "[i]nbox" :query "tag:unread AND tag:inbox" :key "i")
              (:name "[l]ists" :query "tag:unread AND tag:lists" :key "l")
              ;;Show unread automated messages
              (:name "auto[m]ated" :query "tag:unread AND tag:automated" :key "m")
              ;;Show unread promotional messages
              (:name "[p]romotional" :query "tag:unread AND tag:promotional" :key "p")
              (:name "[b]od" :query "tag:unread AND tag:lists/bod" :key "b")
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
[InboxFilter]"))))


(define render-block
  #~(let ((unread (call-with-port (open-input-pipe
                                   (string-append #$notmuch
                                                  "/bin/notmuch"
                                                  " count tag:unread"))
                    (compose read-line))))
      (if (zero? (string->number unread))
          ""
          (string-append "✉ " unread))))

(define (dtao-extension config)
  (list
   (dtao-block
    (interval 10)
    (render render-block))))

(define systemic-mail-service-type
  (service-type
   (name 'systemic-mail)
   (description "A basic mail service.")
   (extensions
    (list
     (service-extension home-emacs-service-type emacs-extension)
     (service-extension home-isync-service-type isync-extension)
     (service-extension home-notmuch-service-type notmuch-extension)
     (service-extension home-profile-service-type (lambda _ (list afew)))
     (service-extension
      home-mcron-service-type
      (lambda _ (list #~(job "*/2 * * * *"
                             (string-append #$notmuch "/bin/notmuch new")))))
     (service-extension home-xdg-configuration-files-service-type
                        add-afew-config-file)
     (service-extension home-dtao-guile-service-type dtao-extension)))))
