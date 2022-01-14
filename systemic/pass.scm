(define-module (systemic pass)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (pass))

(define (pass service)
  (let* ((port (open-input-pipe (string-append "pass " service)))
         (str (read-line port)))
    (close-pipe port)
    (if (eof-object? str)
        ""
        str)))
