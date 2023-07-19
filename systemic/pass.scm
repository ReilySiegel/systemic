(define-module (systemic pass)
  #:use-module (gnu packages password-utils)
  #:use-module (guix gexp)
  #:export (pass-activation))

(define (pass-activation entry)
  (lambda (file)
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 rdelim)
                       (ice-9 popen))
          (when #$entry
            (let* ((port (open-input-pipe
                          (string-append
                           #$password-store
                           "/bin/pass "
                           #$entry)))
                   (str (read-line port))
                   (secret (if (eof-object? str) #f str)))
              (close-pipe port)
              (when secret
                (let* ((file (string-append (getenv "XDG_STATE_HOME")
                                            "/" #$file))
                       (dir (dirname file)))
                  (unless (file-exists? dir)
                    (mkdir (dirname file)))
                  (when (file-exists? file)
                    (delete-file file))
                  (call-with-output-file file
                    (lambda (port)
                      (display secret port)
                      (chmod file #o400)))))))))))
