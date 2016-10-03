;;; packages.el --- c++-ide layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Alex Chen <fizban007@gmail.com>
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
;; added to `c++-ide-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `c++-ide/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `c++-ide/pre-init-PACKAGE' and/or
;;   `c++-ide/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst c++-ide-packages
  '(
    cc-mode
    cmake-ide
    company
    company-irony
    company-irony-c-headers
    flycheck
    flycheck-irony
    google-c-style
    gtags
    irony
    rtags
    )
  "The list of Lisp packages required by the c++-ide layer.

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

(defun c++-ide/init-irony ()
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
      (defun c++-ide-irony-mode-hook ()
        (define-key irony-mode-map [remap completion-at-point]
          'irony-completion-at-point-async)
        (define-key irony-mode-map [remap complete-symbol]
          'irony-completion-at-point-async)
        (setq company-backends (remove 'company-clang company-backends))
        (add-to-list 'company-backends 'company-irony)
        (add-to-list 'company-backends 'company-irony-c-headers))
      (add-hook 'irony-mode-hook 'c++-ide-irony-mode-hook)
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
      )))

(defun c++-ide/init-flycheck-irony ()
  (use-package flycheck-irony
    :init
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))

(defun c++-ide/init-company-irony-c-headers ()
  (use-package company-irony-c-headers))

(defun c++-ide/init-rtags ()
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

(defun c++-ide/init-company-irony ()
  (use-package company-irony))

(defun c++-ide/init-cmake-ide ()
  (use-package cmake-ide
    :defer t
    :commands (cmake-ide-setup)
    :init
    (progn
      (add-hook 'c-mode-common-hook (lambda ()
                                      (cmake-ide-setup)
                                      ))
      )))

(defun c++-ide/init-google-c-style ()
  (when c++-ide-use-google-c-style
    (use-package google-c-style
      :commands (google-set-c-style)
      :init
      (add-hook 'c-mode-common-hook (lambda () (google-set-c-style))))))

(defun c++-ide/post-init-cc-mode ()
  (add-hook 'c-mode-common-hook (lambda ()
                                  (define-key c-mode-base-map (kbd "<f5>") 'cmake-ide-compile)
                                  (define-key c-mode-base-map (kbd "<f6>") '(lambda ()(interactive)
                                                                              (c++-ide-cmake-check c++-ide-build-command cmake-ide-dir)))
                                  ))
  (add-hook 'c-mode-common-hook #'smartparens-mode)
  )

(defun c++-ide/post-init-flycheck ()
  (spacemacs|use-package-add-hook flycheck
    :post-config
    (add-hook 'c-mode-common-hook (lambda () (progn
                                               ;; block other checkers except flycheck-irony
                                               (add-to-list 'flycheck-disabled-checkers 'c/c++-clang)
                                               (add-to-list 'flycheck-disabled-checkers 'c/c++-cppcheck)
                                               (add-to-list 'flycheck-disabled-checkers 'c/c++-gcc)
                                               ;; (setq flycheck-cppcheck-language-standard "c++11")
                                               )))))


;;; packages.el ends here
