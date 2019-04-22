;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; Preferences
(setq user-full-name "Alex Chen")
(setq user-mail-address "fizban007@gmail.com")
(setq user-organization "Princeton University")

(set-language-environment "English")

(prefer-coding-system 'utf-8)
(setq doom-font (font-spec :family "Source Code Pro" :size 13))
(setq which-key-idle-delay 0)

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

        :nvi "C-n" #'evil-next-line
        :nvi "C-p" #'evil-previous-line
        :nvi "M-;" #'comment-dwim

        :v "C-k" #'kill-region

        :nvi "C-y" #'evil-paste-before

        :nvi "C-c g" #'magit-status

        (:leader
          (:desc "window" :prefix "w"
            :desc "evil-window-vsplit" :nv "/" #'evil-window-vsplit
            :desc "evil-window-delete" :nv "d" #'evil-window-delete))

        (:leader
          (:desc "project" :prefix "p"
            :desc "+ivy/projectile-find-file" :nv "f" #'+ivy/projectile-find-file
            :desc "+ivy/project-search"       :nv "s" #'+ivy/project-search))

        (:leader
          (:desc "file" :prefix "f"
            :desc "neotree-show" :nv "n" #'neotree-toggle))
        ;; :leader (:prefix ("c" . "code")
        ;;   :desc "Comment or uncomment lines"    "l" #'evil-commentary-line
        ;;   :desc "evil-commentary-yank-line"        "y" #'evil-commentary-yank-line)

        (:leader (:desc "workspace" :prefix "k"))
        ))

(def-package! evil-nerd-commenter
  :init
  (map! (:leader (:desc "code" :prefix "c"
                   :desc "evilnc-copy-and-comment-lines" :nv "y" #'evilnc-copy-and-comment-lines
                   :desc "evilnc-comment-or-uncomment-lines"     :nv "l" #'evilnc-comment-or-uncomment-lines)))
  )

(def-package! auctex-latexmk
  :init
  (auctex-latexmk-setup))

(def-package! cdlatex
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex))

;; (map! :nvi "C-c g" #'magit-status)

;; Completion at point
(after! company
  :config
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

  (defun do-yas-expand ()
	(let ((yas-fallback-behavior 'return-nil))
	  (yas/expand)))

  (defun tab-indent-or-complete ()
	(interactive)
	(if (minibufferp)
	    (minibuffer-complete)
	  (if (or (not yas/minor-mode)
		      (null (do-yas-expand)))
		  (if (check-expansion)
		      (company-complete-common)
		    (indent-for-tab-command)))))

  (map! :i "TAB" #'tab-indent-or-complete)
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
      mu4e-compose-mode
      mu4e~update-mail-mode)
    'emacs)
  )
