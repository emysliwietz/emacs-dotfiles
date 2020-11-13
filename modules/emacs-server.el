;; Emacs Server
;;; Emacsclient C-x k to end client
(use-package server
  :ensure nil
  :defer t
  :hook (after-init . server-mode))
(add-hook 'server-switch-hook
	  (lambda ()
	    (when (current-local-map)
	      (use-local-map (copy-keymap (current-local-map))))
	    (local-set-key (kbd "C-x k") 'server-edit)))

(provide 'emacs-server)

