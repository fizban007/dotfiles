;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alex Chen"
      user-mail-address "fizban007@gmail.com")

(set-language-environment "English")
(prefer-coding-system 'utf-8)
(setq comp-deferred-compilation t)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Source Code Pro" :size 20))
(setq which-key-idle-delay 0)
;;(setq doom--line-number-style 'normal)
;; (setq inhibit-compacting-font-caches t)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)
;; (setq doom-theme 'doom-solarized-dark)
;; (setq doom-theme 'doom-city-lights)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(use-package all-the-icons
  :if (display-graphic-p))

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
        :nvi "M-n" (lambda () (interactive) (scroll-up 2))
        :nvi "M-p" (lambda () (interactive) (scroll-down 2))

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
            :desc "neotree-toggle" :nv "n" #'neotree-toggle
            :desc "neotree-find-file" :nv "t" #'+neotree/find-this-file)

          (:desc "alternate-buffer" :nv "TAB" #'+spacemacs/alternate-buffer))
        ))


(after! evil-collection
  (defun my-hjkl-rotation (_mode mode-keymaps &rest _rest)
    (evil-collection-translate-key 'normal mode-keymaps
      "k" "j"
      "h" "k"
      "j" "h"))

  ;; called after evil-collection makes its keybindings
  (add-hook 'evil-collection-setup-hook #'my-hjkl-rotation)
  )

(map!
 "C-x p"   #'+popup/other
 "C-`"     #'+popup/toggle
 "C-~"     #'+popup/raise
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
  "M-<left>"  #'sp-backward-barf-sexp)
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

 (:after magit
   :map magit-mode-map
   :nv "k"     #'magit-next-line
   :nv "K"     #'magit-section-forward
   :nv "h"     #'magit-previous-line
   :nv "H"     #'magit-section-forward
   )

 (:when (modulep! :editor multiple-cursors)
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

;; (after! treemacs-evil
;;   (add-hook 'treemacs-mode-hook (lambda ()
;;                                   (define-key! evil-treemacs-state-local-map
;;                                     "k" #'treemacs-next-line
;;                                     "h" #'treemacs-previous-line
;;                                     "j" #'treemacs-root-up
;;                                     "l" #'treemacs-root-down)
;;                                   )))

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

(after! tex-mode
  (map-delete sp-pairs 'LaTeX-mode)
  (map-delete sp-pairs 'latex-mode)
  (map-delete sp-pairs 'tex-mode)
  (map-delete sp-pairs 'plain-tex-mode))

(use-package! evil-nerd-commenter
  :init
  (map!
   (:leader
     (:desc "code" :prefix "c"
       :desc "evilnc-copy-and-comment-lines"         :nv "y" #'evilnc-copy-and-comment-lines
       :desc "evilnc-comment-or-uncomment-lines"     :nv "l" #'evilnc-comment-or-uncomment-lines)))
  )

(use-package! cdlatex
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex))

(use-package! pkgbuild-mode
  :commands (pkgbuild-mode)
  :mode (("PKGBUILD\\'" . pkgbuild-mode)))

(defsubst cc-bytecomp-is-compiling ()
  "Return non-nil if eval'ed during compilation."
  (eq (cc-bytecomp-compiling-or-loading) 'compiling))

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
  ;; (map! :i "TAB" '+my-tab-indent-or-complete)
  (setq company-idle-delay 0.2)
  )

(use-package! google-c-style
  :init
  (add-hook 'c-mode-common-hook (lambda () (google-set-c-style))))

;; disable cuda-nvcc for flycheck
(use-package! flycheck
  :init
  (setq-default flycheck-disabled-checkers '(cuda-nvcc c/c++-clang))
  ;; (setq-default flycheck-enabled-checkers '(lsp ))
  )

(use-package! flycheck-clang-tidy
  :after flycheck
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup)
  )

;; configure lsp for cuda
(after! lsp-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(cuda-mode . "cuda"))
  (setq lsp-use-plists t)
  (setq lsp-enable-links t)
  (setq lsp-idle-delay 0.500)
  (setq lsp-lens-enable nil)
  (setq lsp-log-io nil)
  ;; (setq lsp-semantic-tokens-enable t)
  ;; (:after cuda-mode
  ;;        (add-hook 'cuda-mode-hook
  ;;                  (lambda ()
  ;;                    (lsp))))
  (map!
   (:leader
     (:desc "code" :prefix "c"
       :desc "evilnc-comment-or-uncomment-lines"     :nv "l" #'evilnc-comment-or-uncomment-lines)))
  )

(setq +cc-default-header-file-mode 'c++-mode)

(use-package! cuda-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))
  (add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-mode))
  (add-hook 'cuda-mode-hook (lambda ()
                              (yas-minor-mode)
                              (lsp)
                              (display-line-numbers-mode)
                              (setq +format-with 'clang-format)))
  )

(after! projectile
  :config
  (add-to-list 'projectile-other-file-alist '("cu" "cuh" "h" "hpp" "cpp"))
  (add-to-list 'projectile-other-file-alist '("cuh" "cu" "cpp" "c"))
  )

;; Define rust mode
;; (use-package! rust-mode
;;   :hook (rust-mode . lsp))

;; Add keybindings for interacting with Cargo
;; (use-package! cargo
;;   :hook (rust-mode . cargo-minor-mode))
;; (after! lsp-rust
;;   (setq lsp-rust-server 'rust-analyzer))
(setq rustic-lsp-server 'rust-analyzer)

;; (use-package! flycheck-rust
;;   :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Handle email
;; (setq +mu4e-backend 'offlineimap)
;; (use-package! mu4e
;;   :init
;;   (setq mu4e-root-maildir "~/mail"
;;         mu4e-attachment-dir "~/mail/.attachments"
;;         mu4e-update-interval 600)
;;   (setq my-mu4e-account-alist
;;         '(("Gmail"
;;            (mu4e-sent-folder "/Gmail/Sent_Mail")
;;            (mu4e-drafts-folder "/Gmail/Drafts")
;;            (user-mail-address "fizban007@gmail.com")
;;            (smtpmail-default-smtp-server "smtp.gmail.com")
;;            (smtpmail-local-domain "gmail.com")
;;            (smtpmail-smtp-user "fizban007")
;;            (smtpmail-smtp-server "smtp.gmail.com")
;;            (smtpmail-stream-type starttls)
;;            (smtpmail-smtp-service 587))
;;           ("Columbia"
;;            (mu4e-sent-folder "/Columbia/Sent_Mail")
;;            (mu4e-drafts-folder "/Columbia/Drafts")
;;            (user-mail-address "yuran.chen@columbia.edu")
;;            (smtpmail-default-smtp-server "smtp.gmail.com")
;;            (smtpmail-local-domain "columbia.edu")
;;            (smtpmail-smtp-user "yc2627@columbia.edu")
;;            (smtpmail-smtp-server "smtp.gmail.com")
;;            (smtpmail-stream-type starttls)
;;            (smtpmail-smtp-service 587))
;;           ("Princeton"
;;            (mu4e-sent-folder "/Princeton/Sent_Mail")
;;            (mu4e-drafts-folder "/Princeton/Drafts")
;;            (user-mail-address "yuran.chen@princeton.edu")
;;            (smtpmail-default-smtp-server "smtp.princeton.edu")
;;            (smtpmail-local-domain "princeton.edu")
;;            (smtpmail-smtp-user "yuranc")
;;            (smtpmail-smtp-server "smtp.princeton.edu")
;;            (smtpmail-stream-type starttls)
;;            (smtpmail-smtp-service 587))
;;           ("Astro"
;;            (mu4e-sent-folder "/Astro/Sent_Mail")
;;            (mu4e-drafts-folder "/Astro/Drafts")
;;            (user-mail-address "alexc@astro.princeton.edu")
;;            (smtpmail-default-smtp-server "mail.astro.princeton.edu")
;;            (smtpmail-local-domain "astro.princeton.edu")
;;            (smtpmail-smtp-user "alexc")
;;            (smtpmail-smtp-server "mail.astro.princeton.edu")
;;            (smtpmail-stream-type starttls)
;;            (smtpmail-smtp-service 587))
;;           ("DPSE"
;;            (mu4e-sent-folder "/dpse/Sent")
;;            (mu4e-drafts-folder "/dpse/Draft")
;;            (user-mail-address "alex.c@deepsensing.cn")
;;            (smtpmail-default-smtp-server "smtp.mxhichina.com")
;;            (smtpmail-local-domain "deepsensing.cn")
;;            (smtpmail-smtp-user "alex.c@deepsensing.cn")
;;            (smtpmail-smtp-server "smtp.mxhichina.com")
;;            (smtpmail-stream-type ssl)
;;            (smtpmail-smtp-service 465))))
;;   (defun my-mu4e-set-account ()
;;     "Set the account for composing a message."
;;     (let* ((account
;;             (if mu4e-compose-parent-message
;;                 (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
;;                   (string-match "/\\(.*?\\)/" maildir)
;;                   (match-string 1 maildir))
;;               (completing-read (format "Compose with account: (%s) "
;;                                        (mapconcat #'(lambda (var) (car var))
;;                                                   my-mu4e-account-alist "/"))
;;                                (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
;;                                nil t nil nil (caar my-mu4e-account-alist))))
;;            (account-vars (cdr (assoc account my-mu4e-account-alist))))
;;       (if account-vars
;;           (mapc #'(lambda (var)
;;                     (set (car var) (cadr var)))
;;                 account-vars)
;;         (error "No email account found"))))
;;   (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
;;   (add-hook 'mu4e-compose-mode-hook #'turn-on-auto-fill)

;;   (setq mu4e-maildir-shortcuts
;;         '( ("/Gmail/Primary"       . ?g)
;;            ("/Gmail/Updates"       . ?u)
;;            ("/Columbia/INBOX"      . ?c)
;;            ("/Princeton/INBOX"     . ?p)
;;            ("/Astro/INBOX"         . ?a)))
;;   (setq browse-url-browser-function 'browse-url-firefox)

;;   (set-evil-initial-state!
;;     '(mu4e-main-mode
;;       mu4e-view-mode
;;       mu4e-headers-mode
;;       ;; mu4e-compose-mode
;;       mu4e~update-mail-mode)
;;     'emacs)
;;   (setq mu4e-compose-format-flowed nil)
;;   )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(describe-char-unidata-list
   '(name old-name general-category decomposition digit-value iso-10646-comment))
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
