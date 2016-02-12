;;; keybindings.el --- alex layer key binding file for Spacemacs.

;; Config evil keybinding for Colemak keymap
(define-key evil-normal-state-map "k" 'evil-next-visual-line)
(define-key evil-normal-state-map "h" 'evil-previous-visual-line)
(define-key evil-normal-state-map "j" 'evil-backward-char)
(define-key evil-visual-state-map "k" 'evil-next-visual-line)
(define-key evil-visual-state-map "h" 'evil-previous-visual-line)
(define-key evil-visual-state-map "j" 'evil-backward-char)
(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
(define-key evil-normal-state-map "\C-f" 'evil-forward-char)
(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
(define-key evil-normal-state-map "\C-b" 'evil-backward-char)
(define-key evil-insert-state-map "\C-b" 'evil-backward-char)
(define-key evil-visual-state-map "\C-b" 'evil-backward-char)
;;(define-key evil-normal-state-map "\C-d" 'evil-delete-char)
;;(define-key evil-insert-state-map "\C-d" 'evil-delete-char)
;;(define-key evil-visual-state-map "\C-d" 'evil-delete-char)
(define-key evil-normal-state-map "\C-n" 'evil-next-line)
(define-key evil-insert-state-map "\C-n" 'evil-next-line)
(define-key evil-visual-state-map "\C-n" 'evil-next-line)
(define-key evil-normal-state-map "\C-p" 'evil-previous-line)
(define-key evil-insert-state-map "\C-p" 'evil-previous-line)
(define-key evil-visual-state-map "\C-p" 'evil-previous-line)

;; Add a convenient fold toggle binding
(define-key evil-normal-state-map (kbd "z <SPC>") 'evil-toggle-fold)
(define-key evil-visual-state-map (kbd "z <SPC>") 'evil-toggle-fold)

;; Use tab to indent or complete depending on situation
(global-set-key (kbd "TAB") 'tab-indent-or-complete)

;; find file as root
(global-set-key (kbd "C-x F") 'find-file-as-root)

;;; keybindings.el ends here
