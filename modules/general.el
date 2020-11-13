;; Encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setenv "LANG" "en_US.UTF-8")

(setq mouse-autoselect-window t
      focus-follows-mouse t)


;; Disable backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Delete selection when pasting
(delete-selection-mode 1)

(provide 'general)
