;; Dired
;;; Colourful dired
(use-package diredfl
  :ensure t
  :defer t
  :init (diredfl-global-mode 1))

(use-package all-the-icons-dired
  :ensure t
  :defer t
  :config
  (all-the-icons-dired-mode 1))

(define-minor-mode dired-follow-mode
  "Diplay file at point in dired after a move."
  :lighter " dired-f"
  :global t
  (if dired-follow-mode
      (advice-add 'dired-next-line :after (lambda (arg) (dired-display-file)))
    (advice-remove 'dired-next-line (lambda (arg) (dired-display-file)))))

(setq vc-follow-symlinks t)


(provide 'dired-tweaks)

