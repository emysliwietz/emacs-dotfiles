;; Shortcuts
;;; Typing shortcuts
;;;; YASnippet
(use-package yasnippet
  :ensure t
  :defer 2
  :init
  (yas-global-mode 1)
  :config
  (use-package yasnippet-snippets
    :ensure t
    :defer t
    )
  (yas-reload-all)
  )

;;;; Bracket highlighting and insertion
(show-paren-mode 1)
(electric-pair-mode 1)
(setq electric-pair-pairs '(
			    (?\" . ?\")
			    (?\{ . ?\})
			    (?\[ . ?\])
			    (?\$ . ?\$)))

;;;; Copy-whole-line
(fset 'copy-whole-line
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 67108896 5 134217847] 0 "%d")) arg)))

(global-set-key (kbd "C-c w l") 'copy-whole-line)

;;;; Copy-line-above and copy-line-below (and paste) 
(fset 'copy-line-above
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 16 1 67108896 5 134217847 14 25] 0 "%d")) arg)))

(global-set-key (kbd "C-c l a") 'copy-line-above)

(fset 'copy-line-below
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 14 1 67108896 5 134217847 16 25] 0 "%d")) arg)))

(global-set-key (kbd "C-c l b") 'copy-line-below)



;;; Kill word improved
;;; normal kill-word kills forward, but not whole word. This fixes that
(defun kill-whole-word ()
  (interactive)
  (backward-word)
  (kill-word 1))
(global-set-key (kbd "C-c k w") 'kill-whole-word)


;;; File shortcuts
;; Note taken on [2018-08-03 Fri 18:19]
(global-unset-key (kbd "C-z"))

(defadvice goto-line (after unfold-tree activate)
  (when (outline-invisible-p)
    (save-excursion
      (outline-previous-visible-heading 1)
      (org-show-subtree))))


(defun agenda-today ()
  (interactive)
  (goto-line (string-to-number (shell-command-to-string "~/.scripts/agendatoday")))
  (org-reveal 1))

(defun dailyplan()
  (interactive)
  (find-file (shell-command-to-string "date +'~/dp/dailyplan/%Y/%Y-%m/%Y-%m-%d.org' | tr -d '\n'"))
  (end-of-buffer))

;(add-hook 'find-file-hook 'dailyplan-hook)
;(defun dailyplan-hook ()
;  (when (string= (buffer-file-name) "dailyplan.org")
;    (agenda-today)))

(defun books()
  (interactive)
  (find-file "~/pCloudDrive/agenda/books.org"))

(defun thesis()
  (interactive)
  (find-file "~/nextcloud/bachelor/thesis/structure.tex"))

(defun projects()
  (interactive)
  (find-file "~/pCloudDrive/agenda/currprojects.org"))

(defun movies()
  (interactive)
  (find-file "~/pCloudDrive/agenda/movies.org"))

(defun reviews()
  (interactive)
  (find-file "~/pCloudDrive/agenda/reviews/2018.org")
  (split-and-follow-vertically)
  (find-file "~/pCloudDrive/agenda/reviews/template.org"))

(global-set-key (kbd "C-z d") 'dailyplan)
(global-set-key (kbd "C-z b") 'books)
(global-set-key (kbd "C-z m") 'movies)
(global-set-key (kbd "C-z r") 'reviews)
(global-set-key (kbd "C-z p") 'projects)
(global-set-key (kbd "C-z t") 'thesis)
(global-set-key (kbd "C-z e") 'mu4e)


;;; Rectangle mark mode 
(global-set-key (kbd "C-ö") (lambda () (interactive) (rectangle-mark-mode)))
;;; Sudo-edit
(use-package sudo-edit
  :ensure t
  :defer t
  :bind ("C-c s" . sudo-edit))

(provide 'shortcuts)
