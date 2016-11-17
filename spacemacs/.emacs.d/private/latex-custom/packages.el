;;; packages.el --- latex-custom layer packages file for Spacemacs.
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
;; added to `latex-custom-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `latex-custom/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `latex-custom/pre-init-PACKAGE' and/or
;;   `latex-custom/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst latex-custom-packages
  '(
    auctex
    pdf-tools
    zotelo
    (cdlatex :location local)
    )
  "The list of Lisp packages required by the latex-custom layer.

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


(defun latex-custom/init-cdlatex ()
  (use-package cdlatex)
  (evil-outline-folding-latex)
  )

(defun latex-custom/post-init-auctex ()
  ;; Here we configure synctex which provides bi-directional mapping
  ;; between the pdf file and the latex source file. Clicking on the
  ;; pdf file will allow you to jump to the corresponding line in
  ;; the latex source, and vice versa.
  (defun setup-synctex-latex ()
    (setq TeX-source-correlate-method (quote synctex))
    (setq TeX-source-correlate-mode t)
    (setq TeX-source-correlate-start-server t)
    ;; (setq TeX-view-program-list
    ;;       (quote
    ;;        (("Okular" "okular --unique \"%o#src:%n$(pwd)/./%b\""))))
    (setq TeX-view-program-selection
          (quote
           (((output-dvi style-pstricks)
             "dvips and gv")
            (output-dvi "xdvi")
            (output-pdf "PDF Tools")
            (output-html "xdg-open")))))

  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  ;; (add-hook 'LaTeX-mode-hook (lambda () (add-to-list 'TeX-command-list '("MkLaTeX" "latexmk -pdf -pdflatex='pdflatex -file-line-error -synctex=1' -pvc %t" TeX-run-command nil (latex-mode docTeX-mode)))))
  ;; (add-hook 'LaTeX-mode-hook (lambda () (setq TeX-command-default "MkLaTeX")))
  (add-hook 'LaTeX-mode-hook 'setup-synctex-latex)
  )

(defun latex-custom/init-pdf-tools ()
  (pdf-tools-install))

(defun latex-custom/init-zotelo ()
  (use-package zotelo)
  (add-hook 'LaTeX-mode-hook 'zotelo-minor-mode)
)

;;; packages.el ends here
