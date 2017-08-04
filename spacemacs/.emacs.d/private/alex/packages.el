;;; packages.el --- alex layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Alex Chen <alex@Chen-Beloborodov-929>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `alex-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `alex/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `alex/pre-init-PACKAGE' and/or
;;   `alex/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst alex-packages
  '(
    ag
    cc-mode
    company
    cuda-mode
    evil-surround
    ;; helm
    magit
    yasnippet
    nlinum
    eclim
    ;; company-emacs-eclim
    ;; gradle-mode
    wgrep-ag
    dropbox
    doom-themes
    (swig-mode :location local))
  "The list of Lisp packages required by the alex layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun alex/init-ag ()
  (use-package ag))

(defun alex/init-doom-themes ()
  (require 'doom-themes)

  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
  )

(defun alex/init-eclim ()
  (use-package eclim)
  (setq eclimd-autostart t)
  ;; (global-eclim-mode)
  (add-hook 'java-mode-hook 'eclim-mode)
  )

(defun alex/init-gradle-mode ()
  (use-package gradle-mode)
  (add-hook 'java-mode-hook 'gradle-mode))

(defun alex/init-company-emacs-eclim ()
  (require 'company-emacs-eclim)
  (add-hook 'java-mode-hook 'company-emacs-eclim-setup)
  ;; (company-emacs-eclim-setup)
  )

(defun alex/init-dropbox ()
  (use-package dropbox)
  (dropbox-connect))

(defun alex/init-nlinum ()
  (use-package nlinum))

(defun alex/init-cuda-mode ()
  (use-package cuda-mode
    :mode ("\\.cu\\'" "\\.cuh\\'")
    :config
    (add-hook 'cuda-mode-hook 'hs-minor-mode))
  )

(defun alex/init-wgrep-ag ()
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)
  )

(defun alex/init-swig-mode ()
  (use-package swig-mode
    :mode "\\.i\\'"))

;; (defun alex/post-init-helm ()
;;   (spacemacs|use-package-add-hook helm
;;     :post-init
;;     (when (executable-find "curl")
;;       (setq helm-google-suggest-use-curl-p t))

;;     (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;;     (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;;     (define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z
;;     (define-key helm-map (kbd "C-h") 'helm-find-files-up-one-level)
;;     ;; Load helm-mini instead of ido-switch-buffer
;;     (global-set-key (kbd "C-x b") 'helm-mini)
;;     (global-set-key (kbd "C-x C-f") 'helm-find-files)
;;     (global-set-key (kbd "M-y") 'helm-show-kill-ring))
;;   )

;; (defun alex/post-init-neotree ()
;;   (spacemacs|use-package-add-hook neotree
;;     :post-config
;;     (prog
;;      (evilified-state-evilify-map neotree-mode-map
;;        :mode neotree-mode
;;        :bindings
;;        (kbd "TAB")  'neotree-stretch-toggle
;;        (kbd "RET") 'neotree-enter
;;        (kbd "|") 'neotree-enter-vertical-split
;;        (kbd "-") 'neotree-enter-horizontal-split
;;        (kbd "'") 'neotree-quick-look
;;        (kbd "c") 'neotree-create-node
;;        (kbd "C") 'neotree-copy-node
;;        (kbd "d") 'neotree-delete-node
;;        (kbd "gr") 'neotree-refresh
;;        (kbd "j") 'spacemacs/neotree-collapse-or-up
;;        (kbd "J") 'neotree-select-previous-sibling-node
;;        (kbd "k") 'neotree-next-line
;;        (kbd "K") 'neotree-select-down-node
;;        (kbd "h") 'neotree-previous-line
;;        (kbd "H") 'neotree-select-up-node
;;        (kbd "l") 'spacemacs/neotree-expand-or-open
;;        (kbd "L") 'neotree-select-next-sibling-node
;;        (kbd "q") 'neotree-hide
;;        (kbd "r") 'neotree-rename-node
;;        (kbd "R") 'neotree-change-root
;;        (kbd "?") 'spacemacs/neotree-transient-state/body
;;        (kbd "s") 'neotree-hidden-file-toggle)
;;      (global-set-key (kbd "<f8>") 'neotree-toggle))
;;     )
;;   )

(defun alex/post-init-yasnippet ()
  ;; (yas-advise-indent-function 'cdlatex-tab)
  (yas-advise-indent-function 'org-cycle)
  (yas-advise-indent-function 'org-try-cdlatex-tab)
  )

(defun alex/post-init-magit ()
  (add-hook 'magit-status-refresh-hook '(lambda () (evil-emacs-state)))
  ;; (add-to-list 'evil-emacs-state-modes 'magit-status-mode)
  ;; (add-to-list 'evil-emacs-state-modes 'magit-mode)
  (global-set-key (kbd "C-c g") 'magit-status))

(defun alex/post-init-company ()
  ;; Use more natural company bindings
  (add-hook 'company-mode-hook '(lambda ()
                                  (define-key company-active-map (kbd "C-n") 'company-select-next)
                                  (define-key company-active-map (kbd "C-p") 'company-select-previous)
                                  )))

(defun alex/post-init-evil-surround ()
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (push '(?` . ("`" . "'")) evil-surround-pairs-alist)))
  (add-hook 'c++-mode-hook (lambda ()
                             (push '(?< . ("<" . ">")) evil-surround-pairs-alist))))

(defun alex/post-init-cc-mode ()
  (add-hook 'c-mode-common-hook (lambda ()
                                  (define-key c-mode-base-map (kbd "TAB") 'tab-indent-or-complete))))

;; TODO: Finish configuration of latex and org modes

;;; packages.el ends here
