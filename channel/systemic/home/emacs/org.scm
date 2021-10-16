(define-module (systemic home emacs org)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (systemic home emacs-utils)
  #:use-module (systemic packages emacs-xyz)
  #:use-module (guix transformations)
  #:export (org-agenda-configuration
            org-minutes-configuration))


(define org-agenda-configuration
  (elisp-configuration-package
   "org-agenda"
   `((with-eval-after-load
      'org
      (require 'org-super-agenda)
      ;; Default to day view
      (setq org-agenda-span 1)
      (global-set-key (kbd "C-c a") 'org-agenda-list)
      (org-super-agenda-mode 1)
      (setq org-super-agenda-groups
            '((:log t)  ; Automatically named "Log"
              (:name "Schedule"
               :time-grid t)
              (:name "Today"
               :scheduled today)
              (:habit t)
              (:name "Due today"
               :deadline today)
              (:name "Overdue"
               :deadline past)
              (:name "Due soon"
               :deadline future)
              (:name "Todo"
	       :todo "TODO")))
      (defun org-class-days (y1 m1 d1 y2 m2 d2 days)
        (seq-filter (lambda (day) (org-class y1 m1 d1 y2 m2 d2 day))
                    days))))
   #:autoloads? #t
   #:elisp-packages (list ((options->transformation
                            '((without-tests . "emacs-org-super-agenda")))
                           emacs-org-super-agenda))))

(define org-minutes-configuration
  (elisp-configuration-package
   "org-minutes"
   `((with-eval-after-load
      'org
      (require 'org-minutes-roam)
      (org-minutes-roam-init)))
   #:autoloads? #t
   #:elisp-packages (list emacs-org-minutes)))
