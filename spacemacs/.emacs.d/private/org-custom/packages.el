;;; packages.el --- org-custom layer packages file for Spacemacs.
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
;; added to `org-custom-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `org-custom/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `org-custom/pre-init-PACKAGE' and/or
;;   `org-custom/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst org-custom-packages
  '(
    org
    ox-pandoc
    )
  "The list of Lisp packages required by the org-custom layer.

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

(defun org-custom/init-ox-pandoc ()
  (use-package ox-pandoc
    :config
    (progn
      (setq org-pandoc-options-for-beamer-pdf '((latex-engine . "xelatex")))
      (setq org-pandoc-options-for-latex-pdf '((latex-engine . "xelatex")))
      ))
  )

(defun org-custom/post-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (progn
      (setq org-directory "~/.org/")

      ;; Org-babel hook
      (add-hook 'org-mode-hook (lambda ()
                                 ;; active Babel languages
                                 (org-babel-do-load-languages
                                  'org-babel-load-languages
                                  '((haskell . t)
                                    (python . t)
                                    (sh . t)
                                    (C . t)
                                    (R . t)
                                    (latex . t)
                                    (emacs-lisp . t)
                                    (scheme . t)
                                    ))
                                 ))

      ;; Setting up templates for org-capture
      (setq org-capture-templates
            '(("t" "Todo" entry (file+headline "~/.org/newgtd.org" "Tasks")
               "* TODO %^{Brief Description}  %^g\n%?\nAdded: %U")
              ("n" "Notes" entry (file+datetree "~/.org/notes/notes.org")
               "* %^{Topic} \n%i%?\n")
              ("b" "Birthday" plain (file+headline "~/.org/birthday.org" "Birthdays")
               "\%\%%?\(org-anniversary  %^{Date}\) %^{Name} would be \%d years old.\n")
              ("w" "Post" entry (file+datetree "~/org-jekyll/org/cyr.org")
               "* %^{Title}  :blog:\n  :PROPERTIES:\n  :on: %T\n  :END:\n  %?\n  %x")
              ("k" "Tricks" entry (file+datetree "~/.org/tricks.org" "Tricks")
               "* %^{Topic}  :tricks:\n  :PROPERTIES:\n  :on: %T\n  :END:\n  %?\n  %x")
              ))

      ;; Use cdlatex in org mode
      (add-hook 'org-mode-hook 'turn-on-org-cdlatex)

      ;; Turn off linum in org mode
      (add-hook 'org-mode-hook (lambda ()
                                 ;; turn off line number for org-mode
                                 (linum-mode -1)))


      ;; Add markdown export
      (add-to-list 'org-export-backends 'md)

      ;; Add new todo keywords for all org-mode buffers
      (setq org-todo-keywords
            '((sequence "TODO(t)" "URGENT(u)" "STARTED(s)" "WAITING(w)" "MAYBE(m)" "|" "DONE(d)" "CANCELED(c)" "DEFERRED(d)")))

      ;; Add new todo keyward faces
      (setq org-todo-keyword-faces
            '(("URGENT" . "red") ("TODO" . org-warning) ("STARTED" . "orange") ("APPT" . "lightblue") ("WAITING" . "lightgreen")))

      ;; Check if all subentries are done
      (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

      ;; Prevent problem with ^ and _ in cdlatex
      (defalias 'last-command-char 'last-command-event)

      ;; Misc settings
      (setq org-use-fast-todo-selection t)
      (setq org-confirm-babel-evaluate nil)

      ;; Setting up publish
      (use-package ox-latex
        :config
        (progn
          (add-to-list 'org-latex-packages-alist '("" "listings"))
          (add-to-list 'org-latex-packages-alist '("" "color"))
          (add-to-list 'org-latex-classes
                       '("cyr-org-article"
                         "\\documentclass[11pt,letterpaper]{article}
                    \\usepackage{graphicx}
                    \\usepackage{amsmath}
                    \\usepackage{tikz}
                    \\usepackage{hyperref}
                    \\usepackage{geometry}
                    \\geometry{letterpaper, textwidth=6.7in, textheight=10in,
                                marginparsep=7pt, marginparwidth=.6in}
                    \\pagestyle{empty}
                    \\title{}
                            [NO-DEFAULT-PACKAGES]
                            [PACKAGES]
                            [EXTRA]"
                         ("\\section{%s}" . "\\section*{%s}")
                         ("\\subsection{%s}" . "\\subsection*{%s}")
                         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                         ("\\paragraph{%s}" . "\\paragraph*{%s}")
                         ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

          ;; ;; Use xelatex to process the file
          ;; (setq org-latex-pdf-process
          ;;       '("pdflatex -interaction nonstopmode %f"
          ;;         "pdflatex -interaction nonstopmode %f")) ;; for multiple passes

          ;; Latex preview setting

          ;; the imagemagick background gives me better foreground colors
          ;; than the other background
          (setq org-latex-create-formula-image-program 'imagemagick)
          (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))
          (setq org-latex-listings t)))

      (use-package ox-html)
      (use-package ox-publish
        :config
        (setq org-publish-project-alist
              '(("notes-html"
                 :base-directory "~/.org/notes/"
                 :base-extension "org"
                 :publishing-directory "~/.org/notes/export/html"
                 :publishing-function org-html-publish-to-html
                 :section-numbers nil)
                ("notes-pdf"
                 :base-directory "~/.org/notes/"
                 :base-extension "org"
                 :publishing-directory "~/.org/notes/export/pdf"
                 :publishing-function org-latex-publish-to-pdf)
                ("notes" :components ("notes-html" "notes-pdf"))
                )))

      ;; (setq org-mobile-directory "~/Dropbox/Org")
      ;; (setq org-mobile-inbox-for-pull "~/.org/from-mobile.org")
      ;; (add-to-list 'org-mobile-files "~/.org/follow-up.org")
      ;; (add-to-list 'org-mobile-files "~/.org/someday.org")
      ;; (add-to-list 'org-mobile-files "~/.org/todo.org")

      ;; (defvar org-mobile-push-timer nil
      ;;   "Timer that `org-mobile-push-timer' used to reschedule itself, or nil.")

      ;; (defun org-mobile-push-with-delay (secs)
      ;;   (when org-mobile-push-timer
      ;;     (cancel-timer org-mobile-push-timer))
      ;;   (setq org-mobile-push-timer
      ;;         (run-with-idle-timer
      ;;          (* 1 secs) nil 'org-mobile-push)))

      ;; (add-hook 'after-save-hook
      ;;           (lambda ()
      ;;             (when (eq major-mode 'org-mode)
      ;;               (dolist (file (org-mobile-files-alist))
      ;;                 (if (string= (file-truename (expand-file-name (car file)))
      ;;                              (file-truename (buffer-file-name)))
      ;;                     (org-mobile-push-with-delay 30)))
      ;;               )))

      ;; (run-at-time "00:05" 86400 '(lambda () (org-mobile-push-with-delay 1))) ;; refreshes agenda file each day

      )))

;;; packages.el ends here
