(define-module (systemic channels)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (ice-9 pretty-print)
  #:export (channels->file
            %systemic-channels
            channel-skel))

;; TODO: Upstream
(define (channels->file channels)
  (plain-file
   "channels.scm"
   (call-with-output-string
     (lambda (port)
       (pretty-print (cons 'list (map channel->code channels)) port)))))

(define %systemic-channels
  (list
   (channel
    (name 'systemic)
    (url "https://github.com/ReilySiegel/systemic")
    (introduction
     (make-channel-introduction
      "89ff5b4374e472194eff08f2a69153d5cde6784e"
      (openpgp-fingerprint
       "0FA2 FE4C 164F 60C6 7F6B  EA7E 508A 5AD0 A50F 88AF"))))
   (channel
    (name 'guix)
    (url "https://github.com/ReilySiegel/guix.git")
    (introduction
     (make-channel-introduction
      "fe46280dbf2f518b9c96059df2f4d0e60b6fb3c7q"
      (openpgp-fingerprint
       "0FA2 FE4C 164F 60C6 7F6B  EA7E 508A 5AD0 A50F 88AF"))))))


(define (channel-skel skeleton)
  (cons*
   (list ".config/guix"
         (computed-file "config-guix"
                        (with-imported-modules
                            '((guix build utils))
                          #~(begin
                              (use-modules (guix build utils))
                              (mkdir-p #$output)))))
   (list ".config/guix/channels.scm"
         (channels->file %systemic-channels))
   skeleton))
