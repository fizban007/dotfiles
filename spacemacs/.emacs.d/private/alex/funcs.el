(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t
          ;; (backward-char 1)
          (if (looking-at "->") t
            (backward-char 1)
            (if (looking-at "->\ ") t nil)))))))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(defun do-yas-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defun yas-advise-indent-function (function-symbol)
  (eval `(defadvice ,function-symbol (around yas/try-expand-first activate)
           ,(format
             "Try to expand a snippet before point, then call `%s' as usual"
             function-symbol)
           (let ((yas/fallback-behavior nil))
             (unless (and (called-interactively-p)
                          (yas-expand))
               ad-do-it)))))

;; Find file as root
(defun find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
    root-privileges (using tramp/sudo), if the file is not writable by
    user."
  (interactive)
  (let ((file (read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

;; These settings make evil folding work better with LaTeX mode
(defun evil-outline-folding-latex ()
  (evil-define-command latex-evil-open-fold ()
    "Open one fold under the cursor."
    (outline-minor-mode)
    (show-children))
  (evil-define-command latex-evil-close-fold ()
    "Close one fold under the cursor."
    (outline-minor-mode)
    (hide-children))
  (evil-define-command latex-evil-open-folds-at-point ()
    "Open all folds under the cursor recursively."
    (outline-minor-mode)
    (show-subtree))
  (evil-define-command latex-evil-close-folds-at-point ()
    "Close all folds under the cursor recursively."
    (outline-minor-mode)
    (hide-subtree))
  (evil-define-command latex-evil-close-all-folds ()
    "Close all folds."
    (outline-minor-mode)
    (hide-sublevels 1))
  (evil-define-command latex-evil-open-all-folds ()
    "Open all folds."
    (outline-minor-mode)
    (show-all))
  (evil-define-command latex-evil-fold-more ()
    "Fold more."
    (outline-minor-mode)
    (when (> evil-fold-level 0)
      (decf evil-fold-level)
      (hide-sublevels (+ evil-fold-level 1))))
  (evil-define-command latex-evil-fold-less ()
    "Reduce folding."
    (outline-minor-mode)
    (incf evil-fold-level)
    (hide-sublevels (+ evil-fold-level 1)))

  (evil-declare-key 'normal LaTeX-mode-map
    "zo" #'latex-evil-open-folds-at-point
    "zO" #'latex-evil-open-fold
    "zc" #'latex-evil-close-folds-at-point
    "zC" #'latex-evil-close-fold
    "zm" #'latex-evil-close-all-folds
    "zM" #'latex-evil-open-all-folds
    "zr" #'latex-evil-fold-less
    "zR" #'latex-evil-fold-more))
