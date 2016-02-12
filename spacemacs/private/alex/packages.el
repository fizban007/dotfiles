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
    auctex
    cc-mode
    cmake-ide
    company
    company-irony
    company-irony-c-headers
    cuda-mode
    evil
    evil-surround
    flycheck
    flycheck-irony
    google-c-style
    helm
    irony
    magit
    org
    paradox
    rtags
    yasnippet
    (cdlatex :location local))
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

(defun alex/init-irony ()
  (use-package irony
    :defer t
    :commands (irony-mode)
    :init
    (progn
      (add-hook 'c++-mode-hook 'irony-mode)
      (add-hook 'c-mode-hook 'irony-mode)
      (add-hook 'objc-mode-hook 'irony-mode))
    :config
    (progn
      ;; replace the `completion-at-point' and `complete-symbol' bindings in
      ;; irony-mode's buffers by irony-mode's function
      (defun my-irony-mode-hook ()
        (define-key irony-mode-map [remap completion-at-point]
          'irony-completion-at-point-async)
        (define-key irony-mode-map [remap complete-symbol]
          'irony-completion-at-point-async)
        (setq company-backends (remove 'company-clang company-backends))
        (add-to-list 'company-backends 'company-irony)
        (add-to-list 'company-backends 'company-irony-c-headers))
      (add-hook 'irony-mode-hook 'my-irony-mode-hook)
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
      )))

(defun alex/init-flycheck-irony ()
  (use-package flycheck-irony
    :init
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))

(defun alex/init-rtags ()
  (use-package rtags
    :init
    (defun use-rtags (&optional useFileManager)
      (and (rtags-executable-find "rc")
           (cond ((not (gtags-get-rootpath)) t)
                 ((and (not (eq major-mode 'c++-mode))
                       (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
                 (useFileManager (rtags-has-filemanager))
                 (t (rtags-is-indexed)))))

    (defun tags-find-symbol-at-point (&optional prefix)
      (interactive "P")
      (if (and (not (rtags-find-symbol-at-point prefix)) rtags-last-request-not-indexed)
          (gtags-find-tag)))
    (defun tags-find-references-at-point (&optional prefix)
      (interactive "P")
      (if (and (not (rtags-find-references-at-point prefix)) rtags-last-request-not-indexed)
          (gtags-find-rtag)))
    (defun tags-find-symbol ()
      (interactive)
      (call-interactively (if (use-rtags) 'rtags-find-symbol 'gtags-find-symbol)))
    (defun tags-find-references ()
      (interactive)
      (call-interactively (if (use-rtags) 'rtags-find-references 'gtags-find-rtag)))
    (defun tags-find-file ()
      (interactive)
      (call-interactively (if (use-rtags t) 'rtags-find-file 'gtags-find-file)))
    (defun tags-imenu ()
      (interactive)
      (call-interactively (if (use-rtags t) 'rtags-imenu 'idomenu)))

    (rtags-enable-standard-keybindings c-mode-base-map "\C-cr")
    (add-to-list 'evil-emacs-state-modes 'rtags-mode)
    ))

(defun alex/init-company-irony ()
  (use-package company-irony))

(defun alex/init-company-irony-c-headers ()
  (use-package company-irony-c-headers))

(defun alex/init-cmake-ide ()
  (use-package cmake-ide
    :defer t
    :commands (cmake-ide-setup)
    :init
    (progn
      (add-hook 'c-mode-common-hook (lambda ()
                                      (cmake-ide-setup)
                                      ;; (cmake-ide-maybe-run-cmake)
                                      ))
      )))

(defun alex/init-cdlatex ()
  (use-package cdlatex)
  (evil-outline-folding-latex)
  )

(defun alex/init-google-c-style ()
  (use-package google-c-style
    :commands (google-set-c-style)
    :init
    (add-hook 'c-mode-common-hook (lambda () (google-set-c-style)))))

(defun alex/init-cuda-mode ()
  (use-package cuda-mode
    :mode ("\\.cu\\'" "\\.cuh\\'"))
  )

(defun alex/post-init-auctex ()
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex))

(defun alex/post-init-helm ()
  (spacemacs|use-package-add-hook helm
    :post-config

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
    ;; Load helm-mini instead of ido-switch-buffer
    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring))
  )

(defun alex/post-init-cc-mode ()
  (add-hook 'c-mode-common-hook (lambda ()
                                  (define-key c-mode-base-map (kbd "<f5>") 'cmake-ide-compile)
                                  (define-key c-mode-base-map (kbd "<f6>") '(lambda ()(interactive)
                                                                              (my-cppcm-test "ninja" cmake-ide-dir)))
                                  (define-key c-mode-base-map (kbd "TAB") 'tab-indent-or-complete)
                                  ))
  ;; ;; Treat all .h files as c++ files
  ;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  )

(defun alex/post-init-paradox ()
  (setq paradox-github-token "cd93000d19abd085bf9a4bb4910e8a41024ac236")
  )

(defun alex/pre-init-yasnippet ()
  (push "~/.emacs.d/private/alex/snippets" auto-completion-private-snippets-directory))

(defun alex/post-init-yasnippet ()
  (yas-advise-indent-function 'cdlatex-tab)
  (yas-advise-indent-function 'org-cycle)
  (yas-advise-indent-function 'org-try-cdlatex-tab)
  )

(defun alex/post-init-magit ()
  (add-hook 'magit-status-mode-hook 'evil-emacs-state)
  (global-set-key (kbd "C-c g") 'magit-status))

(defun alex/post-init-company ()
  ;; Use more natural company bindings
  (add-hook 'company-mode-hook '(lambda ()
                                  (define-key company-active-map (kbd "C-n") 'company-select-next)
                                  (define-key company-active-map (kbd "C-p") 'company-select-previous)
                                  )))

(defun alex/post-init-org ()
  (add-hook 'org-mode-hook (lambda ()
                             ;; turn off line number for org-mode
                             (linum-mode -1))))

(defun alex/post-init-evil-surround ()
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (push '(?` . ("`" . "'")) evil-surround-pairs-alist)))
  (add-hook 'c++-mode-hook (lambda ()
                             (push '(?< . ("<" . ">")) evil-surround-pairs-alist))))

(defun alex/post-init-flycheck ()
  (spacemacs|use-package-add-hook flycheck
    :post-config
    (add-hook 'c-mode-common-hook (lambda () (progn
                                               (add-to-list 'flycheck-disabled-checkers 'c/c++-clang)
                                               (add-to-list 'flycheck-disabled-checkers 'c/c++-cppcheck)
                                               (add-to-list 'flycheck-disabled-checkers 'c/c++-gcc)
                                               ;; (setq flycheck-cppcheck-language-standard "c++11")
                                               )))))

;; TODO: Finish configuration of latex and org modes

;;; packages.el ends here
