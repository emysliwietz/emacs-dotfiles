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

(defmacro image-view (direction)
  `(lambda ()
     (interactive)
     (quit-window)
     (let ((pt (point))
           filename)
       (or (ignore-errors
             (catch 'filename
               (while (dired-next-line ,direction)
                 (when (image-type-from-file-name
                        (setq filename (dired-get-filename)))
                   (throw 'filename filename)))))
           (goto-char pt))
       (dired-view-file))))

(eval-after-load "image-mode"
  '(progn
    (define-key image-mode-map "n" (image-view 1))
    (define-key image-mode-map "p" (image-view -1))))

;(use-package dired-k
;  ;; use dired-k as alternative to revert buffer. This will refresh git status
;  :hook (dired-mode . dired-k)
;  :bind (:map dired-mode-map
;              ("g" . dired-k)))

(use-package diredful
  :ensure t
  :defer t
  :config (diredful-mode 1))

; I don't like colors that much, icons already do everything
;(use-package dired-rainbow
;  :ensure t
;  :defer t
;  :config
;  (progn
;    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
;    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
;    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
;    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
;    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
;    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
;    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
;    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
;    (dired-rainbow-define log "#c17d11" ("log"))
;    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
;    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
;    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
;    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
;    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
;    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
;    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
;    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
;    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
;    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
;    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
;    ))

(use-package dired-git-info
  :ensure t
  :defer t
  :config
  (setq dgi-auto-hide-details-p nil)
  (add-hook 'dired-after-readin-hook 'dired-git-info-auto-enable))


(use-package async
  :ensure t
  :defer t
  :init (dired-async-mode 1))

(use-package dired-quick-sort
  :ensure t
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

