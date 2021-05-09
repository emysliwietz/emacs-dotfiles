(use-package elfeed
  :ensure t
  :defer t
  :bind ("C-z f" . elfeed)
  :bind (:map elfeed-search-mode-map
	      ("q" . bjm/elfeed-save-db-and-bury)
	      ("Q" . bjm/elfeed-save-db-and-bury)
	      ("m" . elfeed-youtube)
	      ("M" . elfeed-youtube)
	      ("f" . elfeed-toggle-star)
	      ("F" . elfeed-toggle-star)
	      )
  :init
  (setq elfeed-search-filter "+unread") ;@3-days-ago
    (setq browse-url-generic-program "firefox")
  :config

  ;;
  ;; linking and capturing
  ;;

  (defun elfeed-link-title (entry)
    "Copy the entry title and URL as org link to the clipboard."
    (interactive)
    (let* ((link (elfeed-entry-link entry))
	   (title (elfeed-entry-title entry))
	   (titlelink (concat "[[" link "][" title "]]")))
      (when titlelink
	(kill-new titlelink)
	(x-set-selection 'PRIMARY titlelink)
	(message "Yanked: %s" titlelink))))

  (defun elfeed-youtube (&optional use-generic-p)
  "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument, visit the current entry in the
browser defined by `browse-url-generic-program'."
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (progn
		  (message "Video loading...")
		  (call-process-shell-command (concat "mpv --no-terminal " it " \&") nil 0)
		 ))
    (mapc #'elfeed-search-update-entry entries)
    (unless (or elfeed-search-remain-on-entry (use-region-p))
      (forward-line))))

  ;; show mode

  (defun elfeed-show-link-title ()
    "Copy the current entry title and URL as org link to the clipboard."
    (interactive)
    (elfeed-link-title elfeed-show-entry))

  (defun elfeed-show-quick-url-note ()
    "Fastest way to capture entry link to org agenda from elfeed show mode"
    (interactive)
    (elfeed-link-title elfeed-show-entry)
    (org-capture nil "n")
    (yank)
    (org-capture-finalize))

  (bind-keys :map elfeed-show-mode-map
	     ("l" . elfeed-show-link-title)
	     ("v" . elfeed-show-quick-url-note))

  ;; search mode

  (defun elfeed-search-link-title ()
    "Copy the current entry title and URL as org link to the clipboard."
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
	       when (elfeed-entry-link entry)
	       do (elfeed-link-title entry))))

  (defun elfeed-mark-all-as-read ()
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))


  ;;functions to support syncing .elfeed between machines
  ;;makes sure elfeed reads index from disk before launching
  (defun bjm/elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

  ;;write to disk when quiting
  (defun bjm/elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window))



  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'star))

  ;;next feed with spacebar

  (defun elfeed-scroll-up-command (&optional arg)
    "Scroll up or go to next feed item in Elfeed"
    (interactive "^P")
    (let ((scroll-error-top-bottom nil))
      (condition-case-unless-debug nil
          (scroll-up-command arg)
	(error (elfeed-show-next)))))

  (defun elfeed-scroll-down-command (&optional arg)
    "Scroll up or go to next feed item in Elfeed"
    (interactive "^P")
    (let ((scroll-error-top-bottom nil))
      (condition-case-unless-debug nil
          (scroll-down-command arg)
	(error (elfeed-show-prev)))))

  (define-key elfeed-show-mode-map (kbd "SPC") 'elfeed-scroll-up-command)
  (define-key elfeed-show-mode-map (kbd "S-SPC") 'elfeed-scroll-down-command)

  (defun elfeed-search-quick-url-note ()
    "In search mode, capture the title and link for the selected
     entry or entries in org aganda."
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
	       do (elfeed-untag entry 'unread)
	       when (elfeed-entry-link entry)
	       do (elfeed-link-title entry)
	       do (org-capture nil "n")
	       do (yank)
	       do (org-capture-finalize)
	       (mapc #'elfeed-search-update-entry entries))
      (unless (use-region-p) (forward-line))))

  (bind-keys :map elfeed-search-mode-map
	     ("l" . elfeed-search-link-title)
	     ("v" . elfeed-search-quick-url-note))

  ;;use eww and mpv to open browser stuff

  (setq browse-url-browser-function
	'(("https:\\/\\/www\\.youtu\\.*be." . browse-url-mpv)
          ("." . browse-url-generic)))

  (defun browse-url-mpv (url &optional single)
    (start-process "mpv" nil "mpv" (shell-quote-argument url)))

  (defun elfeed-show-eww-open (&optional use-generic-p)
    "open with eww"
    (interactive "P")
    (let ((browse-url-browser-function #'eww-browse-url))
      (elfeed-show-visit use-generic-p)))

  (defun elfeed-search-eww-open (&optional use-generic-p)
    "open with eww"
    (interactive "P")
    (let ((browse-url-browser-function #'eww-browse-url))
      (elfeed-search-browse-url use-generic-p)))

  (define-key elfeed-show-mode-map (kbd "B") 'elfeed-show-eww-open)
  (define-key elfeed-search-mode-map (kbd "B") 'elfeed-search-eww-open)
  
  )

(use-package elfeed-goodies
  :ensure t
  :config
  (elfeed-goodies/setup))

 (setq elfeed-goodies/switch-to-entry nil
       elfeed-show-entry-switch 'switch-to-buffer)

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.config/emacs/elfeed.org"))
  (setq elfeed-db-directory "~/.config/emacs/elfeeddb")
  )





(provide 'elfeed-tweaks)
