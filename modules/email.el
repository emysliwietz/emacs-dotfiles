;; Email
(add-to-list 'load-path "/home/user/dox/install/mu/mu4e")

(require 'mu4e)
(require 'smtpmail)

(setq
 message-send-mail-function 'smtpmail-send-it
 starttls-use-gnutls t
 mu4e-sent-messages-behavior 'sent
 )

(define-key mu4e-view-mode-map (kbd "f") 'mu4e-view-go-to-url)

(setq mu4e-maildir "~/Maildir"
      mu4e-trash-folder "/Trash"
      mu4e-refile-folder "/Archive"
      mu4e-get-mail-command "mbsync -ac ~/.config/mu4e/.mbsyncrc"
      mu4e-update-interval 300 ;; second
      mu4e-compose-signature-auto-include nil
      mu4e-view-show-images t
      smtpmail-stream-type 'starttls
      mu4e-view-show-addresses t
      mu4e-attachment-dir "~/Downloads"
      mu4e-use-fancy-chars t)

;;; Bookmarks
(setq mu4e-bookmarks
      `(
      ("maildir:/e.p.mysliwietz/INBOX" "Inbox" ?i)
      ("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
	("flag:unread" "Unread messages" ?n)
        ("date:today..now" "Today's messages" ?t)
        ("date:7d..now" "Last 7 days" ?w)
        ("flag:trashed" "Deleted" ?d)
        ("mime:image/*" "Messages with images" ?p)
        (,(mapconcat 'identity
                     (mapcar
                      (lambda (maildir)
                        (concat "maildir:" (car maildir)))
                      mu4e-maildir-shortcuts) " OR ")
         "All inboxes" ?I)))


(require 'org-mu4e)
(setq org-mu4e-convert-to-html t)

(use-package org-mime
  :defer t
  :ensure t)

(require 'org-mime)

;; this seems to fix the babel file saving thing
(defun org~mu4e-mime-replace-images (str current-file)
  "Replace images in html files with cid links."
  (let (html-images)
    (cons
     (replace-regexp-in-string ;; replace images in html
      "src=\"\\([^\"]+\\)\""
      (lambda (text)
        (format
         "src=\"./:%s\""
         (let* ((url (and (string-match "src=\"\\([^\"]+\\)\"" text)
                          (match-string 1 text)))
                (path (expand-file-name
                       url (file-name-directory current-file)))
                (ext (file-name-extension path))
                (id (replace-regexp-in-string "[\/\\\\]" "_" path)))
           (add-to-list 'html-images
                        (org~mu4e-mime-file
			(concat "image/" ext) path id))
           id)))
      str)
     html-images)))

(add-to-list 'mu4e-view-actions
'("ViewInBrowser" . mu4e-action-view-in-browser) t)




(use-package mu4e-alert
  :defer t
  :ensure t)


(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)


;;need this for hash access
(require 'subr-x)


;; we seem to need this to fix the org-store-link issue
 (org-link-set-parameters "mu4e" :follow #'org-mu4e-open :store 'org-mu4e-store-link)


;; contact tweaks

;;(setq mu4e-compose-complete-only-after t)
;;(setq mu4e-compose-complete-only-personal t)
;;(use-package mu4e-conversation
;;:ensure t
;;)

(setq mu4e-contexts
    `( ,(make-mu4e-context
	  :name "emysliwietz"
	  :enter-func (lambda () (mu4e-message "Entering emysliwietz context"))
          :leave-func (lambda () (mu4e-message "Leaving emysliwietz context"))
	  ;; we match based on the contact-fields of the message
	  :vars '( ( user-mail-address	    . "egidius@mysliwietz.de"  )
                 ( smtpmail-smtp-user . "egidius@mysliwietz.de")
                 ( smtpmail-mail-address . "egidius@mysliwietz.de")
                 ( mu4e-sent-folder . "/emysliwietz/Sent" )
                 ( mu4e-drafts-folder . "/emysliwietz/Drafts" )
                 ( user-mail-address . "egidius@mysliwietz.de" )
                 ( user-full-name . "Egidius Mysliwietz" )
                 ( smtpmail-default-smtp-server . "smtp.strato.de" )
                 ( smtpmail-local-domain . "strato.de" )
                 ( smtpmail-smtp-server . "smtp.strato.de" )
                 ( smtpmail-smtp-service . 587 )
		   ( user-full-name	    . "Egidius Mysliwietz" )))
       ,(make-mu4e-context
	  :name "Uni"
	  :enter-func (lambda () (mu4e-message "Switch to the Uni context"))
	  :vars '( ( user-mail-address	     . "e.mysliwietz@student.ru.nl" )
                 ( smtpmail-smtp-user . "s1000796")
                 ( smtpmail-mail-address . "e.mysliwietz@student.ru.nl")
                 ( mu4e-sent-folder . "/uni/Sent" )
                 ( mu4e-drafts-folder . "/uni/Drafts" )
                 ( user-full-name . "Egidius Mysliwietz" )
                 ( smtpmail-local-domain . "ru.nl" )
                 ( smtpmail-default-smtp-server . "smtp-auth.ru.nl" )
                 ( smtpmail-smtp-server . "smtp-auth.ru.nl" )
                 ( smtpmail-smtp-service . 587 )
		   ( user-full-name	    . "Egidius Mysliwietz" )))

                  ))
(setq mu4e-context-policy 'pick-first)
  ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
  ;; guess or ask the correct context, e.g.

  ;; start with the first (default) context; 
  ;; default is to ask-if-none (ask when there's no context yet, and none match)
  ;; (setq mu4e-context-policy 'pick-first)

  ;; compose with the current context is no context matches;
  ;; default is to ask 
  ;; (setq mu4e-compose-context-policy nil)


(provide 'email)
