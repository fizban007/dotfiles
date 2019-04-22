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
(map! :nv "k" #'evil-next-visual-line
      :nv "h" #'evil-previous-visual-line
      :nv "j" #'evil-backward-char
      :nv "C-e" #'evil-end-of-line
      :nv "C-a" #'doom/backward-to-bol-or-indent
      :nv "z m" #'hs-hide-level

      :nvi "C-n" #'evil-next-line
      :nvi "C-p" #'evil-previous-line

      :v "C-k" #'kill-region

      :nvi "C-y" #'evil-paste-before

      :nvi "C-c g" #'magit-status

      :leader (:prefix ("w" . "+window")
                "/" #'evil-window-vsplit
                "d" #'evil-window-delete)

      :leader (:prefix ("p" . "+project")
                "f" #'+ivy/projectile-find-file
                "s" #'+ivy/project-search)
      )

(def-package! evil-nerd-commenter
  :init
  (global-set-key (kbd "M-;") 'comment-dwim)
  (map! :leader
        (:prefix ("c" . "code")
          :desc "Comment or uncomment lines"    "l" #'evilnc-comment-or-uncomment-lines
          :desc "Copy and comment lines"        "y" #'evilnc-copy-and-comment-lines))
  )

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
  )
