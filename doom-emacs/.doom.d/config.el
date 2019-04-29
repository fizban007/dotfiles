;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; Preferences
(setq user-full-name "Alex Chen")
(setq user-mail-address "fizban007@gmail.com")
(setq user-organization "Princeton University")

(set-language-environment "English")

(prefer-coding-system 'utf-8)
(setq doom-font (font-spec :family "Source Code Pro" :size 13))
;; (setq doom-theme 'doom-one-light)
(setq doom-theme 'doom-one)
(setq which-key-idle-delay 0)
(setq doom--line-number-style 'normal)

;; Key bindings
(after! evil
  :init
  (map! :nv "k" #'evil-next-visual-line
        :nv "h" #'evil-previous-visual-line
        :nv "j" #'evil-backward-char
        :nv "C-e" #'evil-end-of-line
        :nv "C-a" #'doom/backward-to-bol-or-indent
        :nv "z m" #'hs-hide-level
        :nv "s" #'evil-substitute
        :nv "S" #'evil-change-whole-line
        :nv ";" #'evil-repeat-find-char

        :nvi "C-n" #'evil-next-line
        :nvi "C-p" #'evil-previous-line
        :nvi "M-;" #'comment-dwim

        :v "C-k" #'kill-region

        :nvi "C-y" #'evil-paste-before

        :nvi "C-c g" #'magit-status
        :nvi "C-x C-j" #'dired-jump

        (:leader
          (:desc "window" :prefix "w"
            :desc "evil-window-vsplit" :nv "/" #'evil-window-vsplit
            :desc "evil-window-delete" :nv "d" #'evil-window-delete
            :desc "alternate-window"   :nv "TAB" #'+spacemacs/alternate-window
            :desc "+evil/window-move-left" :nv "J" #'+evil/window-move-left
            :desc "+evil/window-move-up"   :nv "H" #'+evil/window-move-up)

          (:desc "project" :prefix "p"
            :desc "+ivy/projectile-find-file" :nv "f" #'+ivy/projectile-find-file
            :desc "+ivy/project-search"       :nv "s" #'+ivy/project-search)

          (:desc "file" :prefix "f"
            :desc "treemacs-toggle" :nv "n" #'+treemacs/toggle
            :desc "treemacs-find-file" :nv "t" #'+treemacs/find-file)

          (:desc "alternate-buffer" :nv "TAB" #'+spacemacs/alternate-buffer))
        ))



(map!
 "C-x p"   #'+popup/other
 "C-`"     #'+popup/toggle
 "C-~" #'+popup/raise
 ;; smartparens
 (:after smartparens
   :map smartparens-mode-map
   "C-M-a"     #'sp-beginning-of-sexp
   "C-M-e"     #'sp-end-of-sexp
   "C-M-f"     #'sp-forward-sexp
   "C-M-b"     #'sp-backward-sexp
   "C-M-d"     #'sp-splice-sexp
   "C-M-k"     #'sp-kill-sexp
   "C-M-t"     #'sp-transpose-sexp
   "C-<right>" #'sp-forward-slurp-sexp
   "M-<right>" #'sp-forward-barf-sexp
   "C-<left>"  #'sp-backward-slurp-sexp
   "M-<left>" #'sp-backward-barf-sexp)
 ;; company mode
 (:after company
   :map company-active-map
   "C-o"        #'company-search-kill-others
   "C-n"        #'company-select-next
   "C-p"        #'company-select-previous
   "C-h"        #'company-quickhelp-manual-begin
   "C-S-h"      #'company-show-doc-buffer
   "C-s"        #'company-search-candidates
   "M-s"        #'company-filter-candidates
   "<C-tab>"    #'company-complete-common-or-cycle
   [tab]        #'company-complete-common-or-cycle
   [backtab]    #'company-select-previous
   "C-RET"      #'counsel-company
   :map company-search-map
   "C-n"        #'company-search-repeat-forward
   "C-p"        #'company-search-repeat-backward
   "C-s" (λ! (company-search-abort) (company-filter-candidates)))
 (:when (featurep! :editor multiple-cursors)
   ;; evil-mc
   (:prefix "gz"
     :nv "d" #'evil-mc-make-and-goto-next-match
     :nv "D" #'evil-mc-make-and-goto-prev-match
     :nv "k" #'evil-mc-make-cursor-move-next-line
     :nv "h" #'evil-mc-make-cursor-move-prev-line
     :nv "m" #'evil-mc-make-all-cursors
     :nv "n" #'evil-mc-make-and-goto-next-cursor
     :nv "N" #'evil-mc-make-and-goto-last-cursor
     :nv "p" #'evil-mc-make-and-goto-prev-cursor
     :nv "P" #'evil-mc-make-and-goto-first-cursor
     :nv "q" #'evil-mc-undo-all-cursors
     :nv "t" #'+multiple-cursors/evil-mc-toggle-cursors
     :nv "u" #'evil-mc-undo-last-added-cursor
     :nv "z" #'+multiple-cursors/evil-mc-make-cursor-here)
   (:after evil-mc
     :map evil-mc-key-map
     :nv "C-n" #'evil-mc-make-and-goto-next-cursor
     :nv "C-N" #'evil-mc-make-and-goto-last-cursor
     :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
     :nv "C-P" #'evil-mc-make-and-goto-first-cursor)
   ;; evil-multiedit
   :v  "R"     #'evil-multiedit-match-all
   :n  "M-d"   #'evil-multiedit-match-symbol-and-next
   :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
   :v  "M-d"   #'evil-multiedit-match-and-next
   :v  "M-D"   #'evil-multiedit-match-and-prev
   :nv "C-M-d" #'evil-multiedit-restore
   (:after evil-multiedit
     (:map evil-multiedit-state-map
       "M-d"    #'evil-multiedit-match-and-next
       "M-D"    #'evil-multiedit-match-and-prev
       "RET"    #'evil-multiedit-toggle-or-restrict-region
       [return] #'evil-multiedit-toggle-or-restrict-region)
     (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
       "C-n" #'evil-multiedit-next
       "C-p" #'evil-multiedit-prev)))

 )

(after! treemacs-evil
  (add-hook 'treemacs-mode-hook (lambda ()
                                  (define-key! evil-treemacs-state-local-map
                                    "k" #'treemacs-next-line
                                    "h" #'treemacs-previous-line
                                    "j" #'treemacs-root-up
                                    "l" #'treemacs-root-down)
                                  )))

(after! pdf-tools
  (map! :map pdf-view-mode-map
        :gn "h" #'evil-collection-pdf-view-previous-line-or-previous-page
        :gn "k" #'evil-collection-pdf-view-next-line-or-next-page
        :gn "j" #'image-backward-hscroll))

(after! latex
  :config
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'LaTeX-mode-hook (lambda () (TeX-fold-mode 1)))
  (add-hook 'LaTeX-mode-hook #'+my-initialize-latex)
  (add-hook 'LaTeX-mode-hook #'+my-setup-synctex-latex)
  )

(def-package! zotelo
  :init
  (add-hook 'LaTeX-mode-hook 'zotelo-minor-mode))

(def-package! evil-nerd-commenter
  :init
  (map!
   (:leader
     (:desc "code" :prefix "c"
       :desc "evilnc-copy-and-comment-lines"         :nv "y" #'evilnc-copy-and-comment-lines
       :desc "evilnc-comment-or-uncomment-lines"     :nv "l" #'evilnc-comment-or-uncomment-lines)))
  )

(def-package! cdlatex
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex))

(def-package! wgrep-ag
  :init
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (add-hook 'ag-mode-hook 'wgrep-ag-setup))

(def-package! pkgbuild-mode
  :commands (pkgbuild-mode)
  :mode (("PKGBUILD\\'" . pkgbuild-mode)))

;; Completion at point
(after! company
  (defun +my-check-expansion ()
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

  (defun +my-do-yas-expand ()
    (let ((yas-fallback-behavior 'return-nil))
	  (yas/expand)))

  (defun +my-tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
	    (minibuffer-complete)
	  (if (or (not yas/minor-mode)
		      (null (+my-do-yas-expand)))
		  (if (+my-check-expansion)
		      (company-complete-common)
		    (indent-for-tab-command)))))
  (map! :i "TAB" '+my-tab-indent-or-complete)
  )

;; Handle email
(setq +email-backend 'offlineimap)
(def-package! mu4e
  :init
  (setq mu4e-maildir "~/mail"
        mu4e-attachment-dir "~/mail/.attachments")
  (setq my-mu4e-account-alist
        '(("Gmail"
           (mu4e-sent-folder "/Gmail/Sent_Mail")
           (mu4e-drafts-folder "/Gmail/Drafts")
           (user-mail-address "fizban007@gmail.com")
           (smtpmail-default-smtp-server "smtp.gmail.com")
           (smtpmail-local-domain "gmail.com")
           (smtpmail-smtp-user "fizban007")
           (smtpmail-smtp-server "smtp.gmail.com")
           (smtpmail-stream-type starttls)
           (smtpmail-smtp-service 587))
          ("Columbia"
           (mu4e-sent-folder "/Columbia/Sent_Mail")
           (mu4e-drafts-folder "/Columbia/Drafts")
           (user-mail-address "yuran.chen@columbia.edu")
           (smtpmail-default-smtp-server "smtp.gmail.com")
           (smtpmail-local-domain "columbia.edu")
           (smtpmail-smtp-user "yc2627@columbia.edu")
           (smtpmail-smtp-server "smtp.gmail.com")
           (smtpmail-stream-type starttls)
           (smtpmail-smtp-service 587))
          ("Princeton"
           (mu4e-sent-folder "/Princeton/Sent_Mail")
           (mu4e-drafts-folder "/Princeton/Drafts")
           (user-mail-address "yuran.chen@princeton.edu")
           (smtpmail-default-smtp-server "smtp.princeton.edu")
           (smtpmail-local-domain "princeton.edu")
           (smtpmail-smtp-user "yuranc")
           (smtpmail-smtp-server "smtp.princeton.edu")
           (smtpmail-stream-type starttls)
           (smtpmail-smtp-service 587))
          ("Astro"
           (mu4e-sent-folder "/Astro/Sent_Mail")
           (mu4e-drafts-folder "/Astro/Drafts")
           (user-mail-address "alexc@astro.princeton.edu")
           (smtpmail-default-smtp-server "mail.astro.princeton.edu")
           (smtpmail-local-domain "astro.princeton.edu")
           (smtpmail-smtp-user "alexc")
           (smtpmail-smtp-server "mail.astro.princeton.edu")
           (smtpmail-stream-type starttls)
           (smtpmail-smtp-service 587))
          ("DPSE"
           (mu4e-sent-folder "/dpse/Sent")
           (mu4e-drafts-folder "/dpse/Draft")
           (user-mail-address "alex.c@deepsensing.cn")
           (smtpmail-default-smtp-server "smtp.mxhichina.com")
           (smtpmail-local-domain "deepsensing.cn")
           (smtpmail-smtp-user "alex.c@deepsensing.cn")
           (smtpmail-smtp-server "smtp.mxhichina.com")
           (smtpmail-stream-type ssl)
           (smtpmail-smtp-service 465))))
  (defun my-mu4e-set-account ()
    "Set the account for composing a message."
    (let* ((account
            (if mu4e-compose-parent-message
                (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                  (string-match "/\\(.*?\\)/" maildir)
                  (match-string 1 maildir))
              (completing-read (format "Compose with account: (%s) "
                                       (mapconcat #'(lambda (var) (car var))
                                                  my-mu4e-account-alist "/"))
                               (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                               nil t nil nil (caar my-mu4e-account-alist))))
           (account-vars (cdr (assoc account my-mu4e-account-alist))))
      (if account-vars
          (mapc #'(lambda (var)
                    (set (car var) (cadr var)))
                account-vars)
        (error "No email account found"))))
  (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
  (add-hook 'mu4e-compose-mode-hook #'turn-on-auto-fill)

  (setq mu4e-maildir-shortcuts
        '( ("/Gmail/Primary"       . ?g)
           ("/Gmail/Updates"       . ?u)
           ("/Columbia/INBOX"      . ?c)
           ("/Princeton/INBOX"     . ?p)
           ("/Astro/INBOX"         . ?a)))
  (setq browse-url-browser-function 'browse-url-firefox)

  (set-evil-initial-state!
    '(mu4e-main-mode
      mu4e-view-mode
      mu4e-headers-mode
      ;; mu4e-compose-mode
      mu4e~update-mail-mode)
    'emacs)
  )
