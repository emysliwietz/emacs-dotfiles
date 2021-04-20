;; Dired
;;; Colourful dired
(use-package diredfl
  :ensure t
  :defer t
  :init (diredfl-global-mode 1))

(use-package all-the-icons-dired
  :ensure t
  :defer nil
  :config
  ;(all-the-icons-dired-mode 1)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)))


(define-minor-mode dired-follow-mode
  "Diplay file at point in dired after a move."
  :lighter " dired-f"
  :global t
  (if dired-follow-mode
      (advice-add 'dired-next-line :after (lambda (arg) (dired-display-file)))
    (advice-remove 'dired-next-line (lambda (arg) (dired-display-file)))))

(setq vc-follow-symlinks t)
(setq dired-listing-switches "-lt")

(use-package async
  :ensure t
  :defer t
  :init (dired-async-mode 1))

(use-package dired-quick-sort
  :defer t
  :config
  (dired-quick-sort-setup)
  (setq dired-quick-sort-suppress-setup-warning t))

(provide 'dired-tweaks)

