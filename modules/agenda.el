;;; Agenda -- Manages org agenda
;;; Org agenda
;;; Commentary:
;;; Code:
(use-package org-alert
  :defer t
  :ensure t
  :config
  (setq alert-default-style 'libnotify
	org-alert-notification-title "Org Agenda"))

(use-package org-timeline
  :defer t
  :ensure t
  :config
  (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append))



(use-package idle-org-agenda
     :after org-agenda
     :ensure t
     :config (idle-org-agenda-mode))

(setq org-agenda-window-setup 'current-window
      org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA"
      org-default-notes-file "/home/user/dp/agenda/refile.org"
      org-use-fast-todo-selection t)

;;; Agenda
(global-set-key "\C-ca" 'org-agenda)

;;;; Org Agenda Files

(setq org-agenda-files (file-expand-wildcards "~/dp/agenda/*"))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-capture-templates
       '(("t" "todo" entry (file org-default-notes-file)
	  "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
	 ("m" "Meeting" entry (file org-default-notes-file)
	  "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
	 ("j" "Journal" entry (file+datetree "/home/user/dp/agenda/journal.org")
	  "* %?\n%U\n" :clock-in t :clock-resume t)
	 ("i" "Idea" entry (file org-default-notes-file)
	  "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
	 ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
	  "** NEXT %? \nDEADLINE: %t") ))

(provide 'agenda)
;;; agenda.el ends here
