;;; ~/dotfiles/doom-emacs/.doom.d/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +spacemacs/window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows"
  (interactive)
  (if (= (count-windows) 2)
    (let* ((window-tree (car (window-tree)))
           (current-split-vertical-p (car window-tree))
           (first-window (nth 2 window-tree))
           (second-window (nth 3 window-tree))
           (second-window-state (window-state-get second-window))
           (splitter (if current-split-vertical-p
                         #'split-window-horizontally
                       #'split-window-vertically)))
      (delete-other-windows first-window)
      ;; `window-state-put' also re-selects the window if needed, so we don't
      ;; need to call `select-window'
      (window-state-put second-window-state (funcall splitter)))
    (error "Can't toggle window layout when the number of windows isn't two.")))

;;;###autoload
(defun +spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

;;;###autoload
(defun +spacemacs/alternate-window ()
  "Switch back and forth between current and last window in the
current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found."))
    (select-window prev-window)))

;;;###autoload
(defun +my-initialize-latex ()
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-newline-function 'newline-and-indent)
  ;; (setq TeX-engine 'xetex)
  (setq TeX-PDF-mode t)

  ;; Only change sectioning colour
  (setq font-latex-fontify-sectioning 'color)

  ;; super-/sub-script on baseline
  (setq font-latex-script-display (quote (nil)))

  (setq LaTeX-indent-level 4)

  (setq TeX-auto-untabify t) ; Automatically remove all tabs from a file before saving it.

  (setq TeX-math-close-double-dollar t)

  (setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))
  (global-set-key (kbd "C-c +") 'cdlatex-item)

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

	(map!
	 :map LaTeX-mode-map
	 :prefix "z"
	 "o" #'latex-evil-open-folds-at-point
	 "O" #'latex-evil-open-fold
	 "c" #'latex-evil-close-folds-at-point
	 "C" #'latex-evil-close-fold
	 "m" #'latex-evil-close-all-folds
	 "M" #'latex-evil-open-all-folds
	 "r" #'latex-evil-fold-less
	 "R" #'latex-evil-fold-more))
  (evil-outline-folding-latex))

;;;###autoload
(defun +setup-synctex-latex ()
  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)
  (setq TeX-source-correlate-method (quote synctex))
  (setq TeX-source-correlate-mode t)
  ;; (setq TeX-source-correlate-start-server t)
  ;; (setq TeX-view-program-list
  ;;   	(quote
  ;;   	 (("Okular" "okular --unique \"%o#src:%n$(pwd)/./%b\""))))
  (setq TeX-view-program-selection
		(quote
		 (((output-dvi style-pstricks)
		   "dvips and gv")
		  (output-dvi "xdvi")
		  (output-pdf "PDF Tools")
		  (output-html "xdg-open")))))
