(define-derived-mode encrypted-org-mode
  org-mode "enc-Org"
  "major mode for editing AES encrypted org documents.")

(defun encrypt-aes ()
   "Encrypt file with AES"
   (interactive "P")
   (when (not (string-equal (buffer-name (current-buffer)) "*scratch*"))
   (shell-command-on-region
    ;; beginning and end of buffer
    (point-min)
    (point-max)
    ;; command and parameters
    "openssl enc -aes-256-cbc -md sha512 -pbkdf2 -iter 100000 -salt -pass file:/home/user/.config/dp.key"
    ;; output buffer
    (current-buffer)
    ;; replace?
    t
    ;; name of the error buffer
    "*AES Error Buffer*"
    ;; show error buffer?
    f)
   ))

(defun decrypt-aes ()
   "Decrypt file with AES"
   (interactive "P")
   (when (not (string-equal (buffer-name (current-buffer)) "*scratch*"))
   (with-silent-modifications
     (shell-command-on-region
      ;; beginning and end of buffer
      (point-min)
      (point-max)
      ;; command and parameters
      "openssl enc -d -aes-256-cbc -md sha512 -pbkdf2 -iter 100000 -salt -pass file:/home/user/.config/dp.key"
      ;; output buffer
      (current-buffer)
      ;; replace?
      t
      ;; name of the error buffer
      "*AES Error Buffer*"
      ;; show error buffer?
      t)
     )
   ))

(add-to-list 'auto-mode-alist '("\\.dp\\'" . encrypted-org-mode))

(add-hook 'encrypted-org-mode-hook
          (lambda ()
	    (decrypt-aes)
	    ))

(add-hook 'encrypted-org-mode-hook
	  (lambda ()
	    (add-hook 'before-save-hook (lambda () (encrypt-aes)) nil 'make-it-local)
	    ))

(add-hook 'encrypted-org-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook (lambda () (decrypt-aes)) nil 'make-it-local)
	    ))



(provide 'encrypted-org-mode)
