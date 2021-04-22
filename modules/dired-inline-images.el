(defun dired-preview--dired-line-is-previewable ()
  "Return non-nil if line under point is previewable"
  (let* ((fname (dired-get-filename nil))
         (ext (upcase (file-name-extension fname)))
         (allowed-extensions '("PBM" "XBM" "XPM" "GIF" "JPEG" "JPG" "TIFF" "TIF" "PNG" "SVG"))
         (search-fun (apply-partially (lambda (a b) (string= a b)) ext))
         (is-ext-allowed (seq-find search-fun allowed-extensions nil)))
    is-ext-allowed))

(defun dired-preview--readin (filename)
  "Read in the file.

Return a string suitable for insertion in `dired' buffer."
    (let ((preview-image (create-image filename 'imagemagick nil :height 200)))
      (with-temp-buffer
        (insert-image preview-image)
        (insert "\n")
        (buffer-string))))

(defun dired-preview-insert ()          ;; Copied more or less directly from dired-subtree
  "Insert preview under this file."
  (interactive)
  (when (and (dired-preview--dired-line-is-previewable)
             (not (dired-subtree--is-expanded-p)))
    (let* ((filename (dired-get-filename nil))
           (listing (dired-preview--readin filename))
           beg end)
      (read-only-mode -1)
      (move-end-of-line 1)
      ;; this is pretty ugly, I'm sure it can be done better
      (save-excursion
        (insert listing)
        (setq end (+ (point) 2)))
      (newline)
      (setq beg (point))
      (let ((inhibit-read-only t))
        (remove-text-properties (1- beg) beg '(dired-filename)))
      (let* ((ov (make-overlay beg end))
             (parent (dired-subtree--get-ov (1- beg)))
             (depth (or (and parent (+ 2 (overlay-get parent 'dired-subtree-depth)))
                        2))
             (face (intern (format "dired-subtree-depth-%d-face" depth))))
        (when dired-subtree-use-backgrounds
          (overlay-put ov 'face face))
        ;; refactor this to some function
        (overlay-put ov 'line-prefix
                     (if (stringp dired-subtree-line-prefix)
                         (if (not dired-subtree-use-backgrounds)
                             (apply 'concat (-repeat depth dired-subtree-line-prefix))
                           (cond
                            ((eq nil dired-subtree-line-prefix-face)
                             (apply 'concat
                                    (-repeat depth dired-subtree-line-prefix)))
                            ((eq 'subtree dired-subtree-line-prefix-face)
                             (concat
                              dired-subtree-line-prefix
                              (propertize
                               (apply 'concat
                                      (-repeat (1- depth) dired-subtree-line-prefix))
                               'face face)))
                            ((eq 'parents dired-subtree-line-prefix-face)
                             (concat
                              dired-subtree-line-prefix
                              (apply 'concat
                                     (--map
                                      (propertize dired-subtree-line-prefix
                                                  'face
                                                  (intern (format "dired-subtree-depth-%d-face" it)))
                                      (number-sequence 1 (1- depth))))))))
                       (funcall dired-subtree-line-prefix depth)))
        (overlay-put ov 'dired-subtree-name filename)
        (overlay-put ov 'dired-subtree-parent parent)
        (overlay-put ov 'dired-subtree-depth depth)
        (overlay-put ov 'evaporate t)
        (push ov dired-subtree-overlays))
      (goto-char (- beg 1))
      (dired-move-to-filename)
      (read-only-mode 1)
      (run-hooks 'dired-subtree-after-insert-hook))))

(defun dired-preview-insert-preview-or-subtree (orig-fun)
  "Call the right insert function for a preview or a subtree"
  (interactive)
  (cond ((dired-subtree--dired-line-is-directory-or-link-p) (apply orig-fun nil))
        ((dired-preview--dired-line-is-previewable) (dired-preview-insert))))

(advice-add 'dired-subtree-insert :around #'dired-preview-insert-preview-or-subtree)

(provide 'dired-inline-images)
