;; navigation
;;; Anvy - navigate by searching for a letter on the screen and jumping to it
(use-package avy
  :ensure t
  :defer t
  :bind ("M-s" . avy-goto-word-1)) ;; changed from char as per jcs

;; Kill minibuffer when loosing focus
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

(use-package sentence-navigation
  :ensure t
  :defer t
  :bind  ("M-e" . sentence-nav-forward)
         ("M-a" . sentence-nav-backward)
  )

;;; Switch window
(use-package switch-window
  :ensure t
  :defer t
  :config
  (setq switch-window-multiple-frames nil)
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
	'("j" "k" "l" "a" "s" "d" "f")) ; ö does not work without pressing RET
  :bind
  ([remap other-window] . switch-window))

;;; Temporarily maximize current buffer
(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
	   (jump-to-register '_)
	 (progn
	   (window-configuration-to-register '_)
	   (delete-other-windows))))


(use-package frog-jump-buffer
  :ensure t
  :defer t
  )
(defun transparent-buffer-advice
  (orig-fun &rest args)
  (shell-command "transset -p 1") ; 0.3
  (let
      ((res
	(apply orig-fun args)))
    (shell-command "transset -p 1")
    res))

(advice-add 'frog-jump-buffer :around #'transparent-buffer-advice)


;;; kill current buffer
(defun kill-curr-buffer ()
  (interactive)
  (if (not (string-equal (buffer-name (current-buffer)) "*scratch*"))
      (kill-buffer (current-buffer))
    (bury-buffer)
  ))

;;; move to start and end of buffer
(global-set-key (kbd "M-n") 'end-of-buffer)
(global-set-key (kbd "M-p") 'beginning-of-buffer)

;; Kill all buffers
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-x C-k k") 'close-all-buffers)

;; Kill unwanted buffers
(defun kill-if-unwanted (buffer)
  (let ((b (buffer-name buffer))
	(bfn (buffer-file-name buffer))
	(unwanted-buffers '(
			    "*Messages*"
			    "*Backtrace*"
			    "*Help*"
			    "*Warnings*"
			    "*Compile-Log*"
			    "*elfeed-log*"
			    "*system-packages*"
			    "*Async Shell Command*"
			    "*Flycheck errors*"
			    "*Flycheck error messages*"
			    "*Flymake log*"
			    "*Calendar*"
			    "*XELB-DEBUG*"
			    "*Read-Aloud Log*"
			    "*elfeed-search*"
			    "elfeed.org"
			    )))
    (cond ((member b unwanted-buffers) (kill-buffer buffer))
	  ((member bfn (mapcar 'expand-file-name org-agenda-files)) (kill-buffer buffer))
	  ((string-match "^\*tramp.*\*$" b) (kill-buffer buffer))
	  ((string-match "\.png$" b) (kill-buffer buffer))
	  ((string-match "\.jpg$" b) (kill-buffer buffer))
	  ((string-match "\.jpeg$" b) (kill-buffer buffer))
	  ((string-match "\.gif$" b) (kill-buffer buffer))
	  ((string-match "\.log$" b) (kill-buffer buffer))
	  ((string-match "^_region_.tex$" b) (kill-buffer buffer))
	  ((string-match "^\*helpful .*\*" b) (kill-buffer buffer))
	  ((string-match "^magit" b) (kill-buffer buffer))
	  ((string-match "^\*.*\*$" b) (kill-buffer buffer))
	  )))

(defun kill-unwanted-buffers ()
  (interactive)
  (mapc 'kill-if-unwanted (buffer-list)))

(global-set-key (kbd "C-x k") 'kill-unwanted-buffers)

;;; Window splitting
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(defun kill-and-balance ()
  (interactive)
  (delete-window)
  (balance-windows))
(global-set-key (kbd "C-x 0") 'kill-and-balance)


;;; IBuffer
(global-set-key (kbd "C-x b") 'ibuffer)

;;; Subword moving
(global-subword-mode 1)

;;; Cycle though tabs
(global-set-key (kbd "<C-tab>") 'next-buffer)
(global-set-key (kbd "<C-iso-lefttab>") 'previous-buffer)

;;; Winum mode for easy moving through windows
(use-package winum
  :ensure t
  :defer t
  :config
  (setq winum-auto-setup-mode-line nil
	winum-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "s-^") 'winum-select-window-by-number)
      (define-key map (kbd "s-0") 'winum-select-window-0)
      (define-key map (kbd "s-1") 'winum-select-window-1)
      (define-key map (kbd "s-2") 'winum-select-window-2)
      (define-key map (kbd "s-3") 'winum-select-window-3)
      (define-key map (kbd "s-4") 'winum-select-window-4)
      (define-key map (kbd "s-5") 'winum-select-window-5)
      (define-key map (kbd "s-6") 'winum-select-window-6)
      (define-key map (kbd "s-7") 'winum-select-window-7)
      (define-key map (kbd "s-8") 'winum-select-window-8)
      (define-key map (kbd "s-9") 'winum-select-window-9)
      map))
  (winum-mode t)
;  :bind (
;	 ("s-0" . winum-select-window-0)
;	 ("s-1" . winum-select-window-1)
;	 ("s-2" . winum-select-window-2)
;	 ("s-3" . winum-select-window-3)
;	 ("s-4" . winum-select-window-4)
;	 ("s-5" . winum-select-window-5)
;	 ("s-6" . winum-select-window-6)
;	 ("s-7" . winum-select-window-7)
;	 ("s-8" . winum-select-window-8)
;	 ("s-9" . winum-select-window-9)
;	 ("s-^" . winum-select-window-by-number))
  )

(provide 'navigation)
