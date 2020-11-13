;; Magit
(use-package magit
  :ensure t
  :defer f
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

;;; Magit Gitflow
(use-package magit-gitflow
  :ensure t
  :defer t
  :config
  (require 'magit-gitflow)
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))


(provide 'magit)
