(define-module (systemic home emacs org)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (systemic home emacs-utils)
  #:use-module (systemic packages emacs-xyz)
  #:use-module (guix transformations)
  #:export (org-configuration
            org-agenda-configuration
            org-roam-configuration))

(define org-directory '(file-truename "~/org"))

(define org-configuration
  (elisp-configuration-package
   "org"
   `((with-eval-after-load
      'org
      (require 'plantuml-mode)
      (setq org-directory ,org-directory
            org-edit-src-content-indentation 0
            org-confirm-babel-evaluate nil
            org-adapt-indentation nil
            org-log-done (quote time)
            org-log-redeadline nil
            org-log-reschedule nil
            ;; Force use emacs for PDF files.
            org-file-apps (butlast org-file-apps)
            org-plantuml-jar-path plantuml-jar-path
            org-latex-pdf-process
            '("xelatex -shell-escape -interaction nonstopmode %f"
              "xelatex -shell-escape -interaction nonstopmode %f"))
      
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((plantuml . t)
         (scheme . t)
         (gnuplot . t)
         (java . t)
         (clojure . t)
         (python . t)
         (R . t)))
      ;; Save buffer after clocking
      (add-hook 'org-clock-in-hook 'save-buffer)
      (add-hook 'org-clock-out-hook 'save-buffer)))
   #:autoloads? #t
   #:elisp-packages (list emacs-org emacs-plantuml-mode)))


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
	       :todo "TODO"))
            org-agenda-files (list ,org-directory
                                   (concat ,org-directory "/daily")))
      (defun org-class-days (y1 m1 d1 y2 m2 d2 days)
        (seq-filter (lambda (day) (org-class y1 m1 d1 y2 m2 d2 day))
                    days))))
   #:autoloads? #t
   #:elisp-packages (list emacs-org-super-agenda)))

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
     (global-set-key (kbd "C-c n t") 'org-roam-tag-add)
     
     (with-eval-after-load
      'org-roam
      (org-roam-setup)))
   #:autoloads? #t
   #:elisp-packages (list emacs-org-roam)))
