;; Org mode
;;; Org indent mode
(add-hook 'org-mode-hook 'org-indent-mode)

;;; Org bullets
(use-package org-bullets
  :ensure t
  :defer t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;; Org sidebar
(use-package org-sidebar
  :ensure t
  :defer t
  )

;;; Agenda
(global-set-key "\C-ca" 'org-agenda)

;;;; Org Agenda Files

(setq org-agenda-files (append
			(delete "~/pCloudDrive/agenda/dailyplan.org" (file-expand-wildcards "~/pCloudDrive/agenda/[a-zA-Z1-9]*.org"))))


;;;; Super Agenda
;;;;; Packets
(use-package org-super-agenda
  :ensure t
  :defer t
  :config
  (require 'org-habit)
  (org-super-agenda-mode)
  (define-key org-super-agenda-header-map (kbd "<tab>") 'origami-toggle-node)


;;;;; Config
  (setq org-agenda-time-grid
	'((daily today require-timed)
	  "----------------------"
	  ()))

  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-include-deadlines t)
  (setq org-agenda-include-diary t)
  (setq org-agenda-block-separator nil)
  (setq org-agenda-compact-blocks t)
  (setq spacemacs-theme-org-agenda-height nil)
  (setq org-agenda-start-with-log-mode t)
  (add-hook 'org-agenda-mode-hook '(lambda() (hl-line-mode 1)))


  (setq org-agenda-custom-commands (list(quote
        ("z" "Super zaen view" (
                                (agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '(
                       (:discard (:todo "DONE"))
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
                         (:name "Unimportant"
                                :todo ("SOMEDAY" "MAYBE" "CHECK" "TO-READ" "TO-WATCH")
                                :order 100)
                         (:name "Waiting..."
                                :todo "WAITING"
                                :order 98)
                         (:name "Scheduled earlier"
                                :scheduled past)
                                ))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '(
                        (:discard (:todo "DONE"))
                        (:name "Next to do"
                                 :todo "NEXT"
                                 :order 1)
                          (:name "Important"
                                 :tag "Important"
                                 :priority "A"
                                 :order 6)
                          (:name "Due Today"
                                 :deadline today
                                 :order 2)
                          (:name "Due Soon"
                                 :deadline future
                                 :order 8)
                          (:name "Overdue"
                                 :deadline past
                                 :order 7)
                          (:name "Currently working on"
                                 :todo ("INPRO" "READING")
                                 :order 9)
                          (:name "Assignments"
                                 :tag "Assignment"
                                 :order 10)
                          (:name "Issues"
                                 :tag "Issue"
                                 :order 12)
                          (:name "Projects"
                                 :tag "Project"
                                 :order 14)
                          (:name "Emacs"
                                 :tag "Emacs"
                                 :order 13)
                          (:name "Research"
                                 :tag "Research"
                                 :order 15)
                          (:name "Read next"
                                 :todo "NEXTREAD"
                                 :order 29)
                          (:name "Read eventually"
                                 :todo "TOREAD"
                                 :order 30)
                          (:name "Waiting"
                                 :todo "WAITING"
                                 :order 20)
                          (:name "trivial"
                                 :priority<= "C"
                                 :tag ("Trivial" "Unimportant")
                                 :todo ("SOMEDAY" )
                                 :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))
                          (:discard (:todo "DONE"))
                          ))
                       )))
         ) ; Super zaen view
        ))))

;;;; Calendar
(use-package calfw
  :ensure t
  :defer t)
(use-package calfw-cal
  :ensure t
  :defer t)
(use-package calfw-org
  :ensure t
  :defer t)
(use-package calfw-ical
  :ensure t
  :defer t)

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; orgmode source
    (cfw:cal-create-source "Orange") ; diary source
   ))) 
(setq calendar-month-name-array
  ["January" "February" "March"     "April"   "May"      "June"
   "July"    "August"   "September" "October" "November" "December"])

;; Week days
(setq calendar-day-name-array
      ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])

;; First day of the week
(setq calendar-week-start-day 1) ; 0:Sunday, 1:Monday
(setq cfw:fchar-junction ?╋
      cfw:fchar-vertical-line ?┃
      cfw:fchar-horizontal-line ?━
      cfw:fchar-left-junction ?┣
      cfw:fchar-right-junction ?┫
      cfw:fchar-top-junction ?┯
      cfw:fchar-top-left-corner ?┏
      cfw:fchar-top-right-corner ?┓)

;;; code edit in same window
(setq org-src-window-setup 'current-window)

;;; Org in scratch buffer
(setq initial-major-mode 'org-mode)

;;; Images
;;;; Iimage mode
(add-hook 'markdown-mode-hook 'iimage-mode)
(add-hook 'org-mode-hook 'iimage-mode)

;;;; Resize images in org-mode
(setq imagemagick-enabled-types t)

;;;; Paste images
(use-package org-download
  :ensure t
  :defer nil
  :config
  (define-key org-mode-map (kbd "C-+") 'org-download-clipboard))


(defun image-p (obj)
  "Return non-nil if OBJ is an image"
  (eq (car-safe obj) 'image))


(defun iimage-scale-to-fit-width ()
  "Scale over-sized images in the active buffer to the width of the currently selected window.
  (imagemagick must be enabled)"
  (interactive)
  (let ((max-width (window-width (selected-window) t)))
    (alter-text-property (point-min) (point-max)
                         'display
                         (lambda (prop)
                           (when (image-p prop)
                             (plist-put (cdr prop) :type 'imagemagick)
                             (plist-put (cdr prop) :max-width max-width)
                             prop)))))

(defun iimage-scale-on-window-configuration-change ()
  "Hook function for major mode that display inline images:
Adapt image size via `iimage-scale-to-fit-width' when the window size changes."
  (add-hook 'window-configuration-change-hook #'iimage-scale-to-fit-width t t))

(add-hook 'markdown-mode-hook #'iimage-scale-on-window-configuration-change)
(add-hook 'org-mode-hook #'iimage-scale-on-window-configuration-change)


;;; Capture
;;;; Settings
(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(use-package noflet
  :ensure t
  :defer t)
(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
	  (org-capture)))

;;;; Org capture
(global-set-key (kbd "<f6>") 'org-capture)


;;; Org reveal
(use-package htmlize
  :ensure t
  :defer t)

(use-package ox-reveal
  :ensure t
  :defer t
  :config
  (require 'ox-reveal)
  (setq org-reveal-root "reveal.js/")
  )

(defun console-make ()
  (interactive)
  (start-process "reveal-make" nil "make"))

(defun toggle-org-reveal-export-on-save ()
  (interactive)
  (if (memq 'org-reveal-export-to-html after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'org-reveal-export-to-html t)
        (remove-hook 'after-save-hook 'console-make nil t)
        (message "Disabled org reveal export on save for current buffer..."))
    (add-hook 'after-save-hook 'org-reveal-export-to-html nil t)
    (add-hook 'after-save-hook 'console-make nil t)
    (message "Enabled org reveal export on save for current buffer...")))


;;; Export to csv

(require 'org)

(defun my-tbl-export (name)
  "Search for table named `NAME` and export."
  (interactive "s")
  (show-all)
  (let ((case-fold-search t))
    (if (search-forward-regexp (concat "#\\+NAME: +" name) nil t)
    (progn
      (next-line)
      (org-table-export (format "%s.csv" name) "orgtbl-to-csv")))))


;;; Beauty tweaks
(setq org-startup-indented t
      org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
      org-ellipsis "  " ;; folding symbol
      org-pretty-entities t
      org-hide-emphasis-markers t
      ;; show actually italicized text instead of /italicized text/
      org-agenda-block-separator ""
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)


(provide 'org-mode)
