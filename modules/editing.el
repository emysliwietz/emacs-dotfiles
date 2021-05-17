;; Auto complete
;;; Company
(use-package company
  :ensure t
  :defer 1
  :diminish company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :bind (("M-/" . company-complete)
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . my-company-yasnippet)
         ;; ("C-c C-y" . my-company-yasnippet)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :init
  (defun my-company-yasnippet ()
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet))
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
        company-tooltip-limit 12            ; bigger popup window
        company-idle-delay .2               ; decrease delay before autocompletion popup shows
        company-echo-delay 0                ; remove annoying blinking
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)

  (defconst emacs/>=26p
    (>= emacs-major-version 26)
    "Emacs is 26 or above.")

  ;; Popup documentation for completion candidates
  (when (and (not emacs/>=26p) (display-graphic-p))
    (use-package company-quickhelp
      :ensure t
      :defer t
      :defines company-quickhelp-delay
      :bind (:map company-active-map
                  ("M-h" . company-quickhelp-manual-begin))
      :hook (global-company-mode . company-quickhelp-mode)
      :init (setq company-quickhelp-delay 0.8))))

;;; Auto-correct
;;; C-x a i g (Add inverse global)
(setq-default abbrev-mode t)

;;; Icons with company


(use-package company-box
  :ensure t
  :defer t
  :hook (company-mode . company-box-mode))

;;; Change major mode when lines are so long they affect performance
(global-so-long-mode t)

(use-package ivy-emoji
     :ensure t
     :defer t
     :bind ("s-u" . ivy-emoji))


(defun which-active-modes ()
  "Return which minor modes are enabled in the current buffer."
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                        (if (and (symbolp mode) (symbol-value mode))
                            (add-to-list 'active-modes mode))
                      (error nil) ))
          minor-mode-list)
    (format "%s" active-modes)))

(defun replace-regexp-entire-buffer (pattern replacement)
  "Perform regular-expression replacement throughout buffer."
  (interactive
   (let ((args (query-replace-read-args "Replace" t)))
     (setcdr (cdr args) nil)    ; remove third value returned from query---args
     args))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match replacement))))

(defun toggle-auto-fill ()
  "Toggle auto fill mode and reset buffer to non-auto-fill."
  (interactive)
  (if (string-match-p "auto-fill-function" (which-active-modes))
      (progn (replace-regexp-entire-buffer "\n" " ")
	     (auto-fill-mode nil))
    (progn
      (message "auto-fill-mode")
      (auto-fill-mode t)
      (insert "f")
      )))



(global-set-key (kbd "M-q") 'toggle-auto-fill)

(use-package undo-tree
  :ensure t
  :defer f
  :config
  (global-undo-tree-mode 1))


(provide 'editing)

