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

(setq vc-follow-symlinks t
      dired-listing-switches "-hlt"
      diredp-toggle-find-file-reuse-dir 1
      image-dired-thumb-size 100
      diredp-image-preview-in-tooltip 100
      dired-auto-revert-buffer t
      diredp-hide-details-initially-flag nil
      dired-hide-details-mode 0)

;(use-package dired-k
;  ;; use dired-k as alternative to revert buffer. This will refresh git status
;  :hook (dired-mode . dired-k)
;  :bind (:map dired-mode-map
;              ("g" . dired-k)))

(use-package diredful
  :config (diredful-mode 1))


(use-package async
  :ensure t
  :defer t
  :init (dired-async-mode 1))

(use-package dired-quick-sort
  :defer t
  :config
  (dired-quick-sort-setup)
  (setq dired-quick-sort-suppress-setup-warning t))

(use-package openwith
  :ensure t
  :defer t
  :config
  (setq openwith-associations
        (cond
         ((string-equal system-type "darwin")
          '(("\\.\\(dmg\\|doc\\|docs\\|xls\\|xlsx\\)$"
             "open" (file))
            ("\\.\\(mp4\\|mp3\\|webm\\|avi\\|flv\\|mov\\)$"
             "open" ("-a" "VLC" file))))
         ((string-equal system-type "gnu/linux")
          '(("\\.\\(mp4\\|m4a\\|mp3\\|webm\\|avi\\|flv\\|mov\\|pdf\\)$"
             "xdg-open" (file))))))
  (openwith-mode +1))

(provide 'dired-tweaks)

