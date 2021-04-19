;; Email
;(add-to-list 'load-path "/home/user/dox/install/mu/mu4e")

(require 'mu4e)
(require 'smtpmail)

(setq
 message-send-mail-function 'smtpmail-send-it
 starttls-use-gnutls t
 mu4e-sent-messages-behavior 'sent
 )

(define-key mu4e-view-mode-map (kbd "f") 'mu4e-view-go-to-url)

(setq mu4e-maildir "~/mail"
      mu4e-trash-folder "/Trash"
      mu4e-refile-folder "/Archive"
      mu4e-get-mail-command "offlineimap"
      mu4e-update-interval 300 ;; second
      mu4e-compose-signature-auto-include nil
      mu4e-view-show-images t
      mu4e-view-prefer-html t
      mu4e-headers-auto-update t
      mu4e-compose-format-flowed t
      smtpmail-stream-type 'starttls
      mu4e-view-show-addresses t
      mu4e-attachment-dir "~/Downloads"
      smtpmail-queue-mail nil
      mu4e-compose-in-new-frame t
      message-kill-buffer-on-exit t
      mu4e-compose-dont-reply-to-self t
      mu4e-headers-date-format "%Y-%m-%d %H:%M"
      mu4e-use-fancy-chars t)

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;;; Bookmarks
(setq mu4e-bookmarks
      `(
      ("maildir:/egidius/INBOX" "Inbox" ?i)
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

(require 'email-accounts)

;(setq mu4e-contexts
;    `( ,(make-mu4e-context
;	  :name "test"
;	  :enter-func (lambda () (mu4e-message "Switch to test context"))
;          :leave-func (lambda () (mu4e-message "Leaving test context"))
;	  ;; we match based on the contact-fields of the message
;	  :vars '(
;		  ( user-full-name               . "test user" )
;		  ( user-mail-address	         . "test@example.com"  )
;                 ( smtpmail-mail-address        . "test@example.com")
;		  ( smtpmail-smtp-user           . "test@example.com")
;                 ( mu4e-sent-folder             . "/test/Sent" )
;                 ( mu4e-drafts-folder           . "/test/Drafts" )
;                 ( smtpmail-default-smtp-server . "smtp.test.com" )
;                 ( smtpmail-smtp-server         . "smtp.test.com" )
;		  ( smtpmail-local-domain        . "test.com" )
;                 ( smtpmail-smtp-service        . 587 )
;		  ))
;       ,(make-mu4e-context
;	  :name "test2"
;	  :enter-func (lambda () (mu4e-message "Switch to test2 context"))
;	  :leave-func (lambda () (mu4e-message "Leaving test2 context"))
;	  :vars '(
;		  ( user-full-name	         . "Test Testington" )
;		  ( user-mail-address	         . "test@student.test.nl" )
;                 ( smtpmail-mail-address        . "test@student.test.nl")
;		  ( smtpmail-smtp-user           . "s1234567")
;                 ( mu4e-sent-folder             . "/test2/Sent" )
;                 ( mu4e-drafts-folder           . "/test2/Drafts" )
;                 ( smtpmail-default-smtp-server . "smtp-auth.test.nl" )
;                 ( smtpmail-smtp-server         . "smtp-auth.test.nl" )
;		  ( smtpmail-local-domain        . "test.nl" )
;                 ( smtpmail-smtp-service        . 587 )
;		  ))
;
;       ))
(setq mu4e-context-policy 'pick-first)
  ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
  ;; guess or ask the correct context, e.g.

  ;; start with the first (default) context; 
  ;; default is to ask-if-none (ask when there's no context yet, and none match)
  ;; (setq mu4e-context-policy 'pick-first)

  ;; compose with the current context is no context matches;
  ;; default is to ask 
;; (setq mu4e-compose-context-policy nil)

(require 'mu4e-thread-folding)


(add-to-list 'mu4e-header-info-custom
             '(:empty . (:name "Empty"
                         :shortname ""
                         :function (lambda (msg) "  "))))
(setq mu4e-headers-fields '((:empty         .    2)
                            (:human-date    .   12)
                            (:flags         .    6)
                            (:mailing-list  .   10)
                            (:from          .   22)
                            (:subject       .   nil)))

(define-key mu4e-headers-mode-map (kbd "<tab>")     'mu4e-headers-toggle-at-point)
(define-key mu4e-headers-mode-map (kbd "<left>")    'mu4e-headers-fold-at-point)
(define-key mu4e-headers-mode-map (kbd "<S-left>")  'mu4e-headers-fold-all)
(define-key mu4e-headers-mode-map (kbd "<right>")   'mu4e-headers-unfold-at-point)
(define-key mu4e-headers-mode-map (kbd "<S-right>") 'mu4e-headers-unfold-all)


(provide 'email)
