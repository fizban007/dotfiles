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
;; Redefine close all folds to close all levels
(define-key evil-normal-state-map (kbd "z m") 'hs-hide-level)
(define-key evil-visual-state-map (kbd "z m") 'hs-hide-level)

;; Use tab to indent or complete depending on situation
(global-set-key (kbd "TAB") 'tab-indent-or-complete)

;; find file as root
(global-set-key (kbd "C-x F") 'find-file-as-root)

;; Neotree bindings
;; (evilified-state-evilify-map neotree-mode-map
;;   :mode neotree-mode
;;   :bindings
;;   (kbd "TAB")  'neotree-stretch-toggle
;;   (kbd "RET") 'neotree-enter
;;   (kbd "|") 'neotree-enter-vertical-split
;;   (kbd "-") 'neotree-enter-horizontal-split
;;   (kbd "'") 'neotree-quick-look
;;   (kbd "c") 'neotree-create-node
;;   (kbd "C") 'neotree-copy-node
;;   (kbd "d") 'neotree-delete-node
;;   (kbd "gr") 'neotree-refresh
;;   (kbd "j") 'spacemacs/neotree-collapse-or-up
;;   (kbd "J") 'neotree-select-previous-sibling-node
;;   (kbd "k") 'neotree-next-line
;;   (kbd "K") 'neotree-select-down-node
;;   (kbd "h") 'neotree-previous-line
;;   (kbd "H") 'neotree-select-up-node
;;   (kbd "l") 'spacemacs/neotree-expand-or-open
;;   (kbd "L") 'neotree-select-next-sibling-node
;;   (kbd "q") 'neotree-hide
;;   (kbd "r") 'neotree-rename-node
;;   (kbd "R") 'neotree-change-root
;;   (kbd "?") 'spacemacs/neotree-transient-state/body
;;   (kbd "s") 'neotree-hidden-file-toggle)
;; (global-set-key (kbd "<f8>") 'neotree-toggle)
(with-eval-after-load 'neotree
  (evil-define-key 'evilified neotree-mode-map (kbd "j") 'spacemacs/neotree-collapse-or-up)
  (evil-define-key 'evilified neotree-mode-map (kbd "J") 'neotree-select-previous-sibling-node)
  (evil-define-key 'evilified neotree-mode-map (kbd "k") 'neotree-next-line)
  (evil-define-key 'evilified neotree-mode-map (kbd "K") 'neotree-select-down-node)
  (evil-define-key 'evilified neotree-mode-map (kbd "h") 'neotree-previous-line)
  (evil-define-key 'evilified neotree-mode-map (kbd "H") 'neotree-select-up-node)
  (global-set-key (kbd "<f8>") 'neotree-toggle)
  )

;;; keybindings.el ends here
