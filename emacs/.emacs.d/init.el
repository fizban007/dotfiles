;; -*- lexical-binding: t -*-

(setq user-full-name "Alex Chen")
(setq user-mail-address "fizban007@gmail.com")
(setq user-organization "Princeton University")

(set-language-environment "English")

(prefer-coding-system 'utf-8)

;; Change the yes or no dialog to y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't create backup-files
(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #auto save# files

;; Disable annoying startup-stuff
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Isearch put you at the beginning of the word
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning () 
  (when isearch-forward (goto-char isearch-other-end)))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)

;; when pasting with middle click in Linux X11, set to paste at cursor
;; position, not at click position
(setq mouse-yank-at-point t)

;; Find file as root
(defun find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
  root-privileges (using tramp/sudo), if the file is not writable by
  user."
  (interactive)
  (let ((file (read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))
;; give it some keybinding...
(global-set-key (kbd "C-x F") 'find-file-as-root)

;; Use C-x C-e to evaluate region
(global-set-key (kbd "C-x C-e") 'eval-region)

;; Pane manipulation, taken from the advice on Xah's blog
;; (global-set-key (kbd "M-1") 'delete-other-windows) ; expand current pane
;; (global-set-key (kbd "M-5") 'split-window-vertically) ; split pane top/bottom
;; (global-set-key (kbd "M-4") 'split-window-horizontally) ; split pane top/bottom
;; (global-set-key (kbd "M-3") 'delete-window) ; close current pane

;; ;; Garbage collection, prevent gc for a longer period of time
(setq gc-cons-threshold 10000000)

;; Auto revert buffers which are modified elsewhere
(global-auto-revert-mode 1)

;; Don't save kill-ring to the x clipboard. This avoids some error at exit
(setq x-select-enable-clipboard-manager nil) 

;; Don't produce an additional window for Ediff, but keep it in the same frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
;; When you quit an Ediff session with q, it just leaves the two diff
;; windows around, instead of restoring the window configuration from
;; when Ediff was started. Here's the (slightly hacky) code to restore
;; the old window configuration:
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; wrap email body
(add-hook 'mail-mode-hook 'turn-on-auto-fill)

;; Searches ignore case
(setq case-fold-search t)

(require 'ibuffer)
;; iBuffer binding
(global-set-key (kbd "C-x C-b") 'ibuffer)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

(setq redisplay-dont-pause t)

;; Perform a perl-like chomp
(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

;; code review
(defun code-review-region (beg end)
  (interactive "r")
  (let* ((text (chomp (buffer-substring-no-properties beg end)))
         (line-number (line-number-at-pos))
         (file (buffer-file-name))
         (path (replace-regexp-in-string "^.*branches/" ""
                                         (replace-regexp-in-string 
                                          "^.*trunk/" "" file))))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "^" nil t)
	(replace-match "| " nil nil))
      (goto-char (point-min))
      (insert (format "+---[%s:%s]\n" path line-number))
      (goto-char (point-max))
      (insert "\n+---\n")
      (kill-region (point-min) (point-max)))))

(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)
(require 'cl)

;; The first line is to prevent problems with use-package
(require 'ert)
(when (not (package-installed-p 'use-package))
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; (add-to-list 'initial-frame-alist '(font . "Monaco-10"))
;; (add-to-list 'default-frame-alist '(font . "Monaco-10"))
(add-to-list 'initial-frame-alist '(font . "Source Code Pro-10"))
(add-to-list 'default-frame-alist '(font . "Source Code Pro-10"))
;; (defvar my-font-family "Monaco")
(defvar my-font-family "Source Code Pro")
(defvar my-font-size 90)

(global-display-line-numbers-mode)

(use-package spaceline
  :ensure t
  :init
  (progn
    (require 'spaceline-config)
    (spaceline-spacemacs-theme)
    (setq spaceline-minor-modes-separator "")))

(use-package diminish
  :ensure t
  :init
  (eval-after-load "abbrev" '(diminish 'abbrev-mode))
  (eval-after-load "hideshow" '(diminish 'hs-minor-mode))
  (eval-after-load "eldoc" '(diminish 'eldoc-mode "Ⓔ"))
  )

;; Load doom-one-theme
(use-package doom-themes
  :ensure t
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
		(lambda (frame)
		  (when (eq (length (frame-list)) 2)
		    (progn
		      (select-frame frame)
		      (load-theme 'doom-one t)))))
    (load-theme 'doom-one 1))
  )

(setq compilation-scroll-output 'first-error)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Visualize icons
(use-package all-the-icons :ensure t)
;; Hide the scroll bar
(scroll-bar-mode -1)

;; Hide the menu bar
(menu-bar-mode -1)

;; Hide the toolbar
(tool-bar-mode -1)

;; Display time in mode line
(display-time)

;; Show matching brackets. When smart-parens-mode is turned on, this
;; will be disabled.
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; Highlighting TODO, FIXME and BUG in programming modes
(add-hook 'prog-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
	    ))

;; Set fill-column to 70 which is the size of column in equal 3 panel
(setq-default fill-column 72)
(setq undo-tree-auto-save-history t)
(global-visual-line-mode)
(diminish 'visual-line-mode "Ⓦ")
;; (use-package visual-fill-column :ensure t
;;   :init
;;   (global-visual-fill-column-mode))

(use-package which-key
  :ensure t
  :diminish nil
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
	which-key-side-window-max-width 0.33
	which-key-idle-delay 0.05))
;; Set C-u to scroll up rather than a prefix like default emacs
(setq evil-want-C-u-scroll t)
(use-package evil
  :ensure t
  :init
  (evil-mode 1)
  :config
  (progn 
    (diminish 'undo-tree-mode nil)
    (add-hook 'prog-mode-hook 'hs-minor-mode)

    (use-package general :ensure t
      :init
      (general-evil-setup t))

    ;; This is supposed to be a great thing, but I seldom use it!
    (use-package evil-surround
      :ensure t
      :init
      (progn 
	(global-evil-surround-mode 1)
	(add-hook 'emacs-lisp-mode-hook (lambda ()
					  (push '(?` . ("`" . "'")) evil-surround-pairs-alist)))
	(add-hook 'c++-mode-hook (lambda ()
				   (push '(?< . ("< " . " >")) evil-surround-pairs-alist)))))
    (defun evil-undefine ()
      (interactive)
      (let (evil-mode-map-alist)
	(call-interactively (key-binding (this-command-keys)))))

    (add-to-list 'evil-emacs-state-modes 'arxiv-mode)
    (add-to-list 'evil-emacs-state-modes 'eww-mode)
    ;; Define general motion keys for colemak
    (general-define-key
     :states '(normal visual)
     "C-e" 'evil-end-of-line
     "k" 'evil-next-visual-line
     "h" 'evil-previous-visual-line
     "j" 'evil-backward-char)

    (general-define-key
     :states '(normal visual insert)
     "C-f" 'evil-forward-char
     "C-b" 'evil-backward-char
     "C-n" 'evil-next-line
     "C-p" 'evil-previous-line
     "C-w" 'backward-kill-word
     "C-y" 'yank
     "C-k" 'kill-line
     "C-a" 'smarter-move-beginning-of-line
     )
    (general-define-key
     :states '(insert emacs)
     "C-e" 'end-of-line
     "C-d" 'delete-forward-char)

    (define-key evil-visual-state-map "\C-k" 'kill-region)
    (define-key evil-normal-state-map "Q" 'call-last-kbd-macro)
    (define-key evil-visual-state-map "Q" 'call-last-kbd-macro)
    ;; (define-key evil-normal-state-map (kbd "TAB") 'evil-undefine)
    ;; (define-key evil-normal-state-map "\M-." 'evil-undefine)
    ;; (define-key evil-normal-state-map "\C-t" 'evil-undefine)
    ;; (define-key evil-insert-state-map "\C-t" 'evil-undefine)
    (define-key evil-normal-state-map "\C-v" 'evil-scroll-down)
    (define-key evil-visual-state-map "\C-v" 'evil-scroll-down)
    (define-key evil-normal-state-map "\M-v" 'evil-scroll-up)
    (define-key evil-visual-state-map "\M-v" 'evil-scroll-up)
    (define-key evil-normal-state-map "\C-\M-v" 'scroll-other-window)
    (define-key evil-visual-state-map "\C-\M-v" 'scroll-other-windown)
    (define-key evil-normal-state-map (kbd "DEL") 'evil-scroll-up)
    (define-key evil-visual-state-map (kbd "DEL") 'evil-scroll-up)
    (define-key evil-normal-state-map "zO" 'evil-open-folds)

    ;; Gonna define folding keys, so enabling hs-minor-mode for all
    ;; programming modes
    ;; (add-hook 'prog-mode-hook #'hs-minor-mode)

    ;; Redefine close all folds to close all levels
    (define-key evil-normal-state-map (kbd "z m") 'hs-hide-level)
    (define-key evil-visual-state-map (kbd "z m") 'hs-hide-level)

    ;; <<<ace-jump-evil>>> Ace-jump bindings in evil-mode
    ;; (define-key evil-normal-state-map " " 'ace-jump-char-mode)
    ;; (define-key evil-visual-state-map " " 'ace-jump-char-mode)

	;;; esc quits everything just like vim
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

    ;; (define-key evil-normal-state-map (kbd "SPC") my-leader-map)

    (use-package evil-nerd-commenter
      :ensure t
      :init
      (progn
	(global-set-key (kbd "M-;") 'comment-dwim)
	(general-nvmap :prefix "SPC"
	  "c SPC" 'evilnc-comment-or-uncomment-lines
	  "cc" 'evilnc-copy-and-comment-lines
	  "cl" 'evilnc-comment-or-uncomment-lines
	  )))

    ;; (evil-declare-key 'normal org-mode-map

    ;; ECB compatibility settings
    ;; (add-hook 'ecb-history-buffer-after-create-hook 'evil-motion-state)
    ;; (add-hook 'ecb-directories-buffer-after-create-hook 'evil-motion-state)
    ;; (add-hook 'ecb-methods-buffer-after-create-hook 'evil-motion-state)
    ;; (add-hook 'ecb-sources-buffer-after-create-hook 'evil-motion-state)

    ;; Start specific modes in specific evil modes
    (loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
				  (nrepl-mode . insert)
				  (pylookup-mode . emacs)
				  (comint-mode . normal)
				  (shell-mode . insert)
				  (git-commit-mode . normal)
				  (paradox-menu-mode . emacs)
				  ;; (git-rebase-mode . emacs)
				  (term-mode . emacs)
				  (help-mode . emacs)
				  (helm-grep-mode . emacs)
				  (grep-mode . emacs)
				  (wgrep-mode . emacs)
				  (bc-menu-mode . emacs)
				  (magit-branch-manager-mode . emacs)
				  (rdictcc-buffer-mode . emacs)
				  (dired-mode . emacs)
				  (compilation-mode . emacs)
				  (wdired-mode . normal))
	  do (evil-set-initial-state mode state))

    ))
;; (evilnc-default-hotkeys)

;; smex provides counsel a way to sort candidates
(use-package smex :ensure t)

(use-package ivy :ensure t
  :diminish (ivy-mode . "") ; does not display ivy in the modeline
  :init (ivy-mode 1)        ; enable ivy globally at startup
  :bind (:map ivy-mode-map  ; bind in the ivy buffer
	      ("C-'" . ivy-avy)) ; C-' to ivy-avy
  :config
  (setq ivy-use-virtual-buffers t)   ; extend searching to bookmarks and …
  (setq ivy-height 20)               ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
  (setq ivy-use-selectable-prompt t) ; Make the prompt line selectable
  (ivy-set-occur 'swiper 'swiper-occur)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)

  ;; (use-package flx :ensure t
  ;;   :init
  ;;   (setq ivy-re-builders-alist
  ;;     '((t . ivy--regex-fuzzy))))
  )

;; ivy-rich is a package that makes ivy buffers display more information
(use-package ivy-rich
  :ensure t
  :after ivy
  :init
  ;; Display icons for buffer switch C-x b
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
	(get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
	(if (symbolp icon)
	    (all-the-icons-icon-for-mode 'fundamental-mode)
	  icon))))
  (setq ivy-rich--display-transformers-list
	'(ivy-switch-buffer
	  (:columns
	   ((ivy-rich-switch-buffer-icon :width 2)
	    (ivy-rich-candidate (:width 30))
	    (ivy-rich-switch-buffer-size (:width 7))
	    (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
	    (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
	    (ivy-rich-switch-buffer-project (:width 15 :face success))
	    (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
	   :predicate
	   (lambda (cand) (get-buffer cand)))))
  (ivy-rich-mode)
  )

(use-package counsel :ensure t
  :bind*                           ; load counsel when pressed
  (("M-x"     . counsel-M-x)       ; M-x use counsel
   ("C-x C-f" . counsel-find-file) ; C-x C-f use counsel-find-file
   ("C-x C-r" . counsel-recentf)   ; search recently edited files
   ("C-c f"   . counsel-git)       ; search for files in git repo
   ("C-c s"   . counsel-git-grep)  ; search for regexp in git repo
   ("C-c /"   . counsel-ag)        ; search for regexp in git repo using ag
   ("C-c l"   . counsel-locate)   ; search for files or else using locate
   ("C-s"     . counsel-grep-or-swiper) ; search for something in the buffer
   )
  :init
  (general-nvmap
    :prefix "SPC x"
    "s" 'save-buffer
    "c" 'save-buffers-kill-terminal
    "F2" 'counsel-linux-app)
  (general-nvmap
    :prefix "SPC f"
    "f" 'counsel-find-file
    "r" 'counsel-recentf
    "b" 'ivy-switch-buffer))

(use-package yasnippet 
  :ensure t
  :diminish (yas-minor-mode . "ⓨ")
  :init
  (progn
    ;; Load the snippets
    (use-package yasnippet-snippets :ensure t)
    (defun yas-advise-indent-function (function-symbol)
      (eval `(defadvice ,function-symbol (around yas/try-expand-first activate)
	       ,(format
		 "Try to expand a snippet before point, then call `%s' as usual"
		 function-symbol)
	       (let ((yas-fallback-behavior nil))
		 (unless (and (interactive-p)
			      (yas-expand))
		   ad-do-it)))))

    (defun yas-my-initialize ()
      (setq yas-indent-line 'auto)
      (yas-advise-indent-function 'cdlatex-tab)
      (yas-advise-indent-function 'org-cycle)
      (yas-advise-indent-function 'org-try-cdlatex-tab)
      (yas-load-directory "~/.emacs.d/snippets")
      (yas-minor-mode-on))

    (defalias 'yas/current-snippet-table 'yas--get-snippet-tables)
    ;; Only turn on yasnippet for these modes
    (add-hook 'org-mode-hook 'yas-my-initialize)
    (add-hook 'c-mode-common-hook 'yas-my-initialize)
    (add-hook 'python-mode-hook 'yas-my-initialize)
    (add-hook 'haskell-mode-hook 'yas-my-initialize)
    ))

(use-package company
  :ensure t
  :diminish (company-mode . "Ⓒ")
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (progn
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

    (global-set-key (kbd "TAB") 'tab-indent-or-complete)
    (defun my-setup-company ()
      ;; (setq company-backends (delete 'company-semantic company-backends))
      ;; (setq company-backends (delete 'company-eclim company-backends))
      ;; (add-to-list 'company-backends 'company-elisp)
      (use-package company-c-headers :ensure t)
      (add-to-list 'company-backends 'company-c-headers)
      ;; (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.9.2/")
      (setq company-idle-delay 0)
      (define-key company-active-map (kbd "C-n") 'company-select-next)
      (define-key company-active-map (kbd "C-p") 'company-select-previous)
      )
    (add-hook 'company-mode-hook 'my-setup-company)
    ))

(use-package magit
  :ensure t
  :commands magit-status
  :init
  (progn
    (global-set-key (kbd "C-c g") 'magit-status)
    ;; This line is to avoid setup warning message from magit
    ;; (setq magit-last-seen-setup-instructions "1.4.0")
    ))

(use-package projectile
  :ensure t
  ;; :diminish (projectile-mode . nil)
  :diminish "P"
  :init
  (progn
    (projectile-global-mode)
    )
  :config
  (progn
    (use-package counsel-projectile :ensure t)
    (use-package ag :ensure t)
    (general-define-key
     :states '(normal visual)
     :prefix "SPC p"
     "a" 'projectile-find-other-file
     "s" 'counsel-projectile-ag
     "f" 'counsel-projectile-find-file
     "p" 'counsel-projectile-switch-project
     "g" 'counsel-projectile-find-file-dwim
     "b" 'counsel-projectile-switch-to-buffer
     "SPC" 'counsel-projectile
     )
    (setq projectile-enable-caching t)
    ;; The following are commented out because they are in the custom file now
    ;; (add-to-list 'projectile-other-file-alist '("C" "h" "hpp"))
    ;; (add-to-list 'projectile-other-file-alist '("cu" "h" "cuh"))
    ;; (add-to-list 'projectile-other-file-alist '("cuh" "cu"))
    ))

(use-package auctex :ensure t ;; use LaTeX mode for .tex files 
  :mode
  ("\\.tex\\'" . LaTeX-mode) ;; Delay the configuratio until LaTeX mode is loaded 
  :init
  (progn ;; These are my customary settings 
    (defun my-initialize-latex ()
      ;; Zotelo is a package which interacts with Zotero through the
      ;; MozRepl console in firefox. It can directly draw references 
      ;; from your zotero collection.
      (use-package zotelo :ensure t
	:init
	(add-hook 'LaTeX-mode-hook 'zotelo-minor-mode))


      (setq TeX-auto-save t)
      (setq TeX-parse-self t)
      (setq-default TeX-master nil)

      (setq reftex-plug-into-AUCTeX t)
      (setq TeX-newline-function 'newline-and-indent)
      ;; (setq TeX-engine 'xetex)
      (setq TeX-PDF-mode t)

      ;; Only change sectioning colour
      (setq font-latex-fontify-sectioning 'color)

      ;; super-/sub-script on baseline
      (setq font-latex-script-display (quote (nil)))

      (setq LaTeX-indent-level 4)

      (setq TeX-auto-untabify t) ; Automatically remove all tabs from a file before saving it.

      (setq TeX-math-close-double-dollar t)

      (setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))
      (global-set-key (kbd "C-c +") 'cdlatex-item)

      ;; These settings make evil folding work better with LaTeX mode
      (defun evil-outline-folding-latex ()
	(evil-define-command latex-evil-open-fold ()
	  "Open one fold under the cursor."
	  (outline-minor-mode)
	  (show-children))
	(evil-define-command latex-evil-close-fold ()
	  "Close one fold under the cursor."
	  (outline-minor-mode)
	  (hide-children))
	(evil-define-command latex-evil-open-folds-at-point ()
	  "Open all folds under the cursor recursively."
	  (outline-minor-mode)
	  (show-subtree))
	(evil-define-command latex-evil-close-folds-at-point ()
	  "Close all folds under the cursor recursively."
	  (outline-minor-mode)
	  (hide-subtree))
	(evil-define-command latex-evil-close-all-folds ()
	  "Close all folds."
	  (outline-minor-mode)
	  (hide-sublevels 1))
	(evil-define-command latex-evil-open-all-folds ()
	  "Open all folds."
	  (outline-minor-mode)
	  (show-all))
	(evil-define-command latex-evil-fold-more ()
	  "Fold more."
	  (outline-minor-mode)
	  (when (> evil-fold-level 0)
	    (decf evil-fold-level)
	    (hide-sublevels (+ evil-fold-level 1))))
	(evil-define-command latex-evil-fold-less ()
	  "Reduce folding."
	  (outline-minor-mode)
	  (incf evil-fold-level)
	  (hide-sublevels (+ evil-fold-level 1)))

	(general-nmap 
	  :keymaps 'LaTeX-mode-map
	  :prefix "z"
	  "o" #'latex-evil-open-folds-at-point
	  "O" #'latex-evil-open-fold
	  "c" #'latex-evil-close-folds-at-point
	  "C" #'latex-evil-close-fold
	  "m" #'latex-evil-close-all-folds
	  "M" #'latex-evil-open-all-folds
	  "r" #'latex-evil-fold-less
	  "R" #'latex-evil-fold-more))
      (evil-outline-folding-latex)

      )

    (add-hook 'LaTeX-mode-hook 'my-initialize-latex)

    (add-hook 'LaTeX-mode-hook (lambda ()
				 (TeX-fold-mode 1)))
    ;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (add-hook 'LaTeX-mode-hook 'zotelo-minor-mode)
    ;; Here we configure synctex which provides bi-directional mapping
    ;; between the pdf file and the latex source file. Clicking on the
    ;; pdf file will allow you to jump to the corresponding line in
    ;; the latex source, and vice versa.
    (defun setup-synctex-latex ()
      (setq TeX-source-correlate-method (quote synctex))
      (setq TeX-source-correlate-mode t)
      (setq TeX-source-correlate-start-server t)
      (setq TeX-view-program-list
	    (quote
	     (("Okular" "okular --unique \"%o#src:%n$(pwd)/./%b\""))))
      (setq TeX-view-program-selection
	    (quote
	     (((output-dvi style-pstricks)
	       "dvips and gv")
	      (output-dvi "xdvi")
	      (output-pdf "Okular")
	      (output-html "xdg-open")))))
    (add-hook 'LaTeX-mode-hook 'setup-synctex-latex)

    ;; Enable latexmk
    (use-package auctex-latexmk :ensure t
      :init
      (auctex-latexmk-setup))
    ))

(use-package cdlatex :ensure t
  :init
  (progn
    (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
    ;; (add-hook 'latex-mode-hook 'turn-on-cdlatex)   ; with Emacs latex mode
    ))

;; pdf-tools is the best way to view pdf files in Emacs
(use-package pdf-tools :ensure t
  :init
  (pdf-tools-install))

(use-package smartparens
  :ensure t
  :diminish "ⓟ"
  :init
  ;; smartparens-config is the bundled sane default configuration
  (require 'smartparens-config)
  (smartparens-global-mode t)

  ;; Hightlight matching pairs
  (show-smartparens-global-mode t)

  ;; In C++ mode, automatically insert a new line when pressing return
  ;; in between a pair of curly braces
  (sp-local-pair '(c-mode c++-mode cuda-mode) "{" nil :post-handlers 
		 '((my-create-newline-and-enter-sexp "RET")))

  (defun my-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  ;; also enable rainbow-delimiter mode
  (use-package rainbow-delimiters :ensure t
    :init
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
  )

(use-package flycheck :ensure t
  :diminish "Ⓕ"
  :init
  (progn
    (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
    (add-hook 'c-mode-common-hook 'flycheck-mode)))

;; Treat all .h files as c++ files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(use-package google-c-style :ensure t)
;; (use-package cmake-ide :ensure t)
(use-package ggtags :ensure t :diminish nil)
(add-hook 'c-mode-common-hook 
	  (lambda () (progn
		       (google-set-c-style)
		       (ggtags-mode 1)
		       (hs-minor-mode)
		       ;; (cmake-ide-setup)
		       (setq ggtags-enable-navigation-keys nil)
		       (setq company-backends (remove 'company-semantic company-backends))
		       (setq company-backends (remove 'company-clang company-backends))
		       (setq company-backends (remove 'company-xcode company-backends))
		       (setq company-backends (remove 'company-eclim company-backends))
		       )))

(use-package cuda-mode :ensure t
  :init
  (autoload 'cuda-mode "cuda-mode" "Cuda Programming Mode." t)
  (add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))
  (add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-mode)))

(use-package highlight-doxygen :ensure t
  :init
  (highlight-doxygen-global-mode 1))

(use-package modern-cpp-font-lock :ensure t
  :diminish (modern-c++-font-lock-mode . "")
  :init
  (add-hook 'c++-mode-hook (lambda () (modern-c++-font-lock-global-mode t)))
  (add-hook 'cuda-mode-hook (lambda () (modern-c++-font-lock-global-mode t)))
  )

(use-package lsp-mode :ensure t :commands lsp
  :config
  (add-to-list 'lsp-language-id-configuration '(cuda-mode . "cuda"))
  (setq lsp-enable-file-watchers nil))
(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package company-lsp :ensure t :commands company-lsp)

(use-package ccls :ensure t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda ()
	   (require 'ccls)
	   (lsp)))
  :init
  (setq ccls-args '("--log-file=/tmp/ccls.log" "-v=1"))
  )

(load "/usr/share/clang/clang-format.el")
(add-hook 'c-mode-common-hook (lambda ()
				(general-nmap "SPC c f" 'clang-format-buffer)
				(general-vmap "SPC c f" 'clang-format-region)
				))

;; cmake mode
(use-package cmake-mode :ensure t
  :commands cmake-mode
  :mode (("CMakelists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)))

(use-package elpy :ensure t
  :diminish "E"
  :init
  (progn
    (elpy-enable)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

(use-package org :ensure t
  :init
  (progn
    (setq org-directory "~/.org/")

    ;; Org-babel hook
    (add-hook 'org-mode-hook (lambda ()
			       ;; active Babel languages
			       (org-babel-do-load-languages
				'org-babel-load-languages
				'((haskell . t)
				  (python . t)
				  (shell . t)
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
    (add-hook 'org-cdlatex-mode-hook (lambda () (diminish 'org-cdlatex-mode "ⓛ")))

    ;; Add markdown export
    ;; (add-to-list 'org-export-backends 'md)

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
    (use-package ox-html)
    (use-package htmlize :ensure t)
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
	))

    ;; setup pandoc
    (use-package ox-pandoc :ensure t
      :config
      (progn
	(setq org-pandoc-options-for-beamer-pdf '((latex-engine . "xelatex")))
	(setq org-pandoc-options-for-latex-pdf '((latex-engine . "xelatex")))
	))

    ;; Misc settings
    (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))
    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-cc" 'org-capture)
    (global-set-key "\C-ca" 'org-agenda)
    (global-set-key "\C-cb" 'org-iswitchb)

    (general-nmap 
      :keymaps 'org-mode-map
      :prefix "z"
      "a" 'org-cycle
      "A" 'org-shifttab
      "c" 'hide-subtree
      "C" 'org-hide-block-all
      "m" 'hide-body
      "o" 'show-subtree
      "O" 'show-all
      "r" 'show-all)
    (general-define-key
     :states '(normal visual insert emacs)
     :keymaps 'org-mode-map
     "M-j" 'org-shiftleft
     "M-l" 'org-shiftright
     "M-J" 'org-metaleft
     "M-K" 'org-metadown
     "M-H" 'org-metaup
     "M-L" 'org-metaright)
    ))

(use-package web-mode :ensure t
  :commands (web-mode)
  :mode (("\\.html?\\'" . web-mode)
	 ("\\.tpl\\.php\\'" . web-mode))
  :config
  (use-package company-web :ensure t
    :init
    (require 'company-web-html)
    (add-hook 'web-mode-hook (lambda ()
			       (set (make-local-variable 'company-backends) '(company-web-html)))))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  )

(use-package mu4e
  :init
  (setq mu4e-maildir "/home/alex/mail")
  (setq mu4e-get-mail-command "offlineimap"
        mu4e-update-interval 300)
  (setq
   mu4e-index-cleanup nil      ;; don't do a full cleanup check
   mu4e-index-lazy-check t)    ;; don't consider up-to-date dirs
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

(use-package mu4e-maildirs-extension :ensure t
  :init
  (mu4e-maildirs-extension)
  (setq mu4e-maildirs-extension-insert-before-str "\n  Basics")
  (setq mu4e-maildirs-extension-hide-empty-maildirs t))

(use-package neotree :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-vc-integration '(face))
  (setq neo-window-width 35)
  (general-nvmap
    :prefix "SPC f"
    "t" 'neotree-toggle)
  (general-nmap
    :keymaps 'neotree-mode-map
    "TAB" 'neotree-enter
    "SPC" 'neotree-quick-look
    "q" 'neotree-hide
    "RET" 'neotree-enter
    "g" 'neotree-refresh
    "n" 'neotree-next-line
    "p" 'neotree-previous-line
    "A" 'neotree-stretch-toggle
    "H" 'neotree-hidden-file-toggle)
  )

;; Exit emacs
(general-nvmap
  :prefix "SPC q"
  "q" 'kill-emacs
  "s" 'save-buffers-kill-emacs
  )

(defun package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
			   (let ((pkg (cadr (assq name where))))
			     (when pkg
			       (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))

;; Update packages and show package list
(general-nvmap
  :prefix "SPC x"
  "p" 'package-list-packages
  "u" 'package-upgrade-all)

;; Evil avy jump
(defun pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
    Call this repeatedly will cycle all positions in `mark-ring'.
    URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
    Version 2016-04-04"
  (interactive)
  (set-mark-command t))
(general-nvmap
  :prefix "SPC j"
  "b" 'pop-global-mark
  "j" 'evil-avy-goto-char
  "w" 'evil-avy-goto-word-1
  "l" 'evil-avy-goto-line)

(general-define-key "C-x C-j" 'dired-jump)

(use-package wgrep-ag :ensure t
  :init
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)
  )

;; This is taken from
;; https://azer.bike/journal/ia-writer-mode-for-emacs/, which tries to
;; mimic iA writer with Emacs and gets pretty close
(defun writing-mode ()
  (interactive)
  (setq buffer-face-mode-face '(:family "dejavu sans mono" :height 100))
  (buffer-face-mode)
  (linum-mode 0)
  (use-package writeroom-mode :ensure t)
  (writeroom-mode 1)
  (blink-cursor-mode)
  (visual-line-mode 1)
  (setq truncate-lines nil)
  (setq line-spacing 5)
  ;; (setq global-hl-line-mode nil)
  )


(use-package markdown-mode :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook 'writing-mode)
  (add-hook 'gfm-mode-hook 'writing-mode)
  (add-hook 'gfm-mode-hook
	    (lambda ()
	      (setq markdown-command "marked"))))

(use-package lua-mode :ensure t
  :commands (lua-mode)
  :mode (("\\.lua\\'" . lua-mode)))

(use-package pkgbuild-mode :ensure t
  :commands (pkgbuild-mode)
  :mode (("PKGBUILD\\'" . pkgbuild-mode)))

(add-to-list 'auto-mode-alist '(".offlineimaprc\\'" . conf-mode))

;; From Spacemacs:
(defun spacemacs/window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((window-tree (car (window-tree)))
	     (current-split-vertical-p (car window-tree))
	     (first-window (nth 2 window-tree))
	     (second-window (nth 3 window-tree))
	     (second-window-state (window-state-get second-window))
	     (splitter (if current-split-vertical-p
			   #'split-window-horizontally
			 #'split-window-vertically)))
	(delete-other-windows first-window)
	;; `window-state-put' also re-selects the window if needed, so we don't
	;; need to call `select-window'
	(window-state-put second-window-state (funcall splitter)))
    (error "Can't toggle window layout when the number of windows isn't two.")))

(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

(defun spacemacs/alternate-window ()
  "Switch back and forth between current and last window in the
current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found."))
    (select-window prev-window)))

(general-nvmap
  :prefix "SPC w"
  "TAB" '(spacemacs/alternate-window :which-key "alternate-window")
  "+" '(spacemacs/window-layout-toggle :which-key "window-layout-toggle")
  "=" 'balance-windows
  "/" 'split-window-right
  "-" 'split-window-below
  "d" 'delete-window
  "1" 'delete-other-windows
  "w" 'ace-window
  )

(general-nvmap
  :prefix "SPC"
  "TAB" '(spacemacs/alternate-buffer :which-key "alternate-buffer"))

(use-package ace-window :ensure t
  :init
  (general-nvmap
    "C-c w" 'ace-window)
  (setq aw-scope 'frame)
  (setq aw-ignore-current t)
  (setq aw-dispatch-always t)
  )

;; Set customized variables here
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(use-package session
  :ensure t)  
(add-hook 'after-init-hook 'session-initialize)
(load "server")
(unless (server-running-p)
  (server-start))
;; Maintain a list of recently opened files
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(put 'dired-find-alternate-file 'disabled nil)
