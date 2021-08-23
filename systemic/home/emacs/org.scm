(define-module (systemic home emacs org)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (systemic home emacs-utils)
  #:export (org-agenda-configuration))


(define org-agenda-configuration
  (elisp-configuration-package
   "org-agenda"
   `((require 'org-super-agenda)
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
                   days)))
   #:autoloads? #t
   #:elisp-packages (list emacs-org-super-agenda)))
