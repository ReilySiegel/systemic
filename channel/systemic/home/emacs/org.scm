(define-module (systemic home emacs org)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (systemic home emacs-utils)
  #:use-module (systemic packages emacs-xyz)
  #:use-module (guix transformations)
  #:export (org-agenda-configuration
            org-roam-configuration
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

(define org-directory '(file-truename "~/org"))

(define org-roam-configuration
  (elisp-configuration-package
   "org-roam"
   `((setq org-roam-directory ,org-directory)
     (setq org-roam-v2-ack t)

     (global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
     (global-set-key (kbd "C-c n f") 'org-roam-node-find)
     (global-set-key (kbd "C-c n g") 'org-roam-graph)
     (global-set-key (kbd "C-c n i") 'org-roam-node-insert)
     (global-set-key (kbd "C-c n c") 'org-roam-capture)
     (global-set-key (kbd "C-c n d") 'org-roam-dailies-capture-today)

     (with-eval-after-load
      'org-roam
      (org-roam-setup)))
   #:autoloads? #t
   #:elisp-packages (list emacs-org-roam)))

(define org-minutes-configuration
  (elisp-configuration-package
   "org-minutes"
   `((with-eval-after-load
      'org
      (require 'org-minutes-roam)
      (org-minutes-roam-init)
      
      ;; Add a keybinding to insert inactive time-stamps. This is useful for
      ;; having a uniform way to insert dates/times of events into a document.
      (define-key org-mode-map (kbd "C-c M-.") 'org-time-stamp-inactive)))
   #:autoloads? #t
   #:elisp-packages (list emacs-org-minutes)))
