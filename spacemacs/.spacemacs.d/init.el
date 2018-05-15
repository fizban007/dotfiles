;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     typescript
     python
     javascript
     html
     octave
     lua
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; helm
     ivy
     ;; arxiv
     auto-completion
     better-defaults
     emacs-lisp
     git
     github
     markdown
     org
     ;; neotree
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     spell-checking
     syntax-checking
     version-control
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t)
     (c++-ide :variables
              c++-ide-use-google-c-style t)
     latex
     latex-custom
     ;; python
     (colors :variables colors-enable-rainbow-identifiers nil)
     ;; eyebrowse
     haskell
     alex
     org-custom
     rust
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '()

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
   ;; dotspacemacs-default-font '("xos4 Terminus"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil

   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t

   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil

   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil

   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil

   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom

   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always

   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers t

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup t

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost
any user code here.  The exception is org related code, which should be placed
in `dotspacemacs/user-config'."
  (setq x-select-enable-clipboard-manager nil)
  (setq evil-want-Y-yank-to-eol t)

  (defun hs-hide-leafs-recursive (minp maxp)
    "Hide blocks below point that do not contain further blocks in
    region (MINP MAXP)."
    (when (hs-find-block-beginning)
      (setq minp (1+ (point)))
      (funcall hs-forward-sexp-func 1)
      (setq maxp (1- (point))))
    (unless hs-allow-nesting
      (hs-discard-overlays minp maxp))
    (goto-char minp)
    (let ((leaf t))
      (while (progn
               (forward-comment (buffer-size))
               (and (< (point) maxp)
                    (re-search-forward hs-block-start-regexp maxp t)))
        (setq pos (match-beginning hs-block-start-mdata-select))
        (if (hs-hide-leafs-recursive minp maxp)
            (save-excursion
              (goto-char pos)
              (hs-hide-block-at-point t)))
        (setq leaf nil))
      (goto-char maxp)
      leaf))

  (defun hs-hide-leafs ()
    "Hide all blocks in the buffer that do not contain subordinate
    blocks.  The hook `hs-hide-hook' is run; see `run-hooks'."
    (interactive)
    (hs-life-goes-on
     (save-excursion
       (message "Hiding blocks ...")
       (save-excursion
         (goto-char (point-min))
         (hs-hide-leafs-recursive (point-min) (point-max)))
       (message "Hiding blocks ... done"))
     (run-hooks 'hs-hide-hook)))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  ;; Personal Settings
  (setq user-full-name "Alexander, Yuran Chen")
  ;; (setq user-mail-address "fizban007@gmail.com")
  (setq user-organization "Princeton University")

  (set-language-environment "English")

  (prefer-coding-system 'utf-8)

  ;; when pasting with middle click in Linux X11, set to paste at cursor
  ;; position, not at click position
  (setq mouse-yank-at-point t)

  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

  (add-hook 'c++-mode-hook '(lambda ()
                              (define-key c++-mode-map (kbd "<f7>") 'clang-format-buffer)))

  (setq exec-path-from-shell-check-startup-files nil)

  (setq cmake-ide-flags-c++ '("-I/usr/lib/gcc/x86_64-unknown-linux-gnu/5.3.0/../../../../include/c++/5.3.0"
                              "-I/usr/lib/gcc/x86_64-unknown-linux-gnu/5.3.0/../../../../include/c++/5.3.0/x86_64-unknown-linux-gnu"
                              "-I/usr/lib/gcc/x86_64-unknown-linux-gnu/5.3.0/../../../../include/c++/5.3.0/backward"
                              "-I/usr/lib/gcc/x86_64-unknown-linux-gnu/5.3.0/include"
                              "-I/usr/local/include"
                              "-I/usr/lib/gcc/x86_64-unknown-linux-gnu/5.3.0/include-fixed"
                              "-I/usr/include"
                              ))
  (setq winum-scope 'frame-local)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-list
   (quote
    ((54 "partial" "" nil)
     (113 "theta" "" nil)
     (81 "Theta" "" nil)
     (47 "frac" "" nil)
     (95 "bar" "" nil)
     (99 "chi" "" nil)
     (50 "sqrt" "" nil)
     (56 "infty" "" nil))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Okular")
     (output-html "xdg-open"))))
 '(evil-disable-insert-state-bindings t)
 '(evil-want-Y-yank-to-eol nil)
 '(irony-additional-clang-options (quote ("-std=c++14")))
 '(irony-lighter " I")
 '(irony-server-install-prefix "~/.emacs.d/private/alex/irony/")
 '(irony-user-dir "~/.emacs.d/private/alex/irony/")
 '(magit-pull-arguments nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (company-ycmd tide typescript-mode web-mode web-beautify wanderlust semi flim apel tagedit slim-mode scss-mode sass-mode pug-mode org-category-capture org-mime mu4e-maildirs-extension lua-mode livid-mode skewer-mode simple-httpd less-css-mode json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc haml-mode glsl-mode flycheck-ycmd ycmd request-deferred ghub let-alist emmet-mode dropbox oauth company-web web-completion-data company-tern dash-functional tern coffee-mode bbdb wgrep-ag winum unfill fuzzy pdf-tools wgrep smex ivy-hydra flyspell-correct-ivy counsel-projectile counsel swiper ivy orgit ox-pandoc org org-plus-contrib nlinum gradle-mode company-emacs-eclim eclim rtags zotelo hide-comnt origami magit-gh-pulls github-search github-clone github-browse-file gist gh marshal logito pcache spinner log4e gntp parent-mode gitignore-mode fringe-helper git-gutter+ seq pos-tip pkg-info epl flx goto-chg undo-tree diminish bind-key pythonic request powerline rust-mode package-build tablist hydra ht alert markdown-mode projectile git-gutter magit magit-popup iedit levenshtein packed anaconda-mode avy auto-complete auctex yasnippet ghc haskell-mode company highlight anzu smartparens bind-map irony flycheck git-commit with-editor helm helm-core popup async f dash s evil company-racer deferred yapfify py-isort org-projectile mwim intero hlint-refactor git-link flyspell-correct-helm flyspell-correct evil-unimpaired dumb-jump company-ghci color-identifiers-mode cargo zenburn-theme xterm-color ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org spacemacs-theme spaceline solarized-theme smooth-scrolling smeargle shm shell-pop restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters racer quelpa pyvenv pytest pyenv-mode py-yapf popwin pip-requirements persp-mode pcre2el paradox page-break-lines org-repo-todo org-present org-pomodoro org-download org-bullets open-junk-file neotree multi-term move-text monokai-theme mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum live-py-mode linum-relative link-hint leuven-theme info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flyspell helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate google-c-style golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe git-gutter-fringe+ gh-md flycheck-rust flycheck-pos-tip flycheck-irony flycheck-haskell flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help elisp-slime-nav disaster diff-hl define-word cython-mode cuda-mode company-statistics company-quickhelp company-irony-c-headers company-irony company-ghc company-cabal company-c-headers company-auctex company-anaconda column-enforce-mode cmm-mode cmake-mode cmake-ide clean-aindent-mode clang-format buffer-move bracketed-paste auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk aggressive-indent ag adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-automatically-star t)
 '(paradox-github-token t)
 '(racer-cmd "/usr/bin/racer")
 '(racer-rust-src-path "/usr/src/rust/src")
 '(reftex-cite-format (quote natbib))
 '(safe-local-variable-values
   (quote
    ((cmake-ide-build-dir . "/home/alex/Projects/Aperture2/ninja")
     (cmake-ide-dir . "/home/alex/Projects/Aperture2/ninja")
     (cmake-ide-build-dir . "/home/alex/Projects/Aperture/ninja")
     (cmake-ide-dir . "/home/alex/Projects/Aperture/ninja")
     (cmake-ide-build-dir . "/home/alex/Projects/1Dpic/ninja")
     (cmake-ide-dir . "/home/alex/Projects/1Dpic/ninja")
     (zotero-collection . "20")
     (zotero-collection .
                        #("1" 0 1
                          (name "Numerical")))
     (TeX-engine quote luatex)
     (zotero-collection .
                        #("16" 0 2
                          (name "Thesis")))
     (zotero-collection
      #("18" 0 2
        (name "Thesis")))
     (reftex-default-bibliography "thesis.bib")
     (zotero-collection .
                        #("10" 0 2
                          (name "PICpaper")))
     (cmake-ide-build-dir . "/home/alex/Programs/Aperture/ninja")
     (cmake-ide-build-dir . "/home/alex/Programs/Aperture2/ninja")
     (cmake-ide-dir . "/home/alex/Programs/Aperture/ninja")
     (cmake-ide-dir . "/home/alex/Programs/Aperture2/ninja"))))
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-list
   (quote
    ((54 "partial" "" nil)
     (113 "theta" "" nil)
     (81 "Theta" "" nil)
     (47 "frac" "" nil)
     (95 "bar" "" nil)
     (99 "chi" "" nil)
     (50 "sqrt" "" nil)
     (56 "infty" "" nil))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Okular")
     (output-html "xdg-open"))))
 '(evil-disable-insert-state-bindings t)
 '(evil-want-Y-yank-to-eol nil)
 '(irony-additional-clang-options (quote ("-std=c++14")))
 '(irony-lighter " I")
 '(irony-server-install-prefix "~/.emacs.d/private/alex/irony/")
 '(irony-user-dir "~/.emacs.d/private/alex/irony/")
 '(magit-pull-arguments nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (wgrep-ag winum unfill fuzzy pdf-tools wgrep smex ivy-hydra flyspell-correct-ivy counsel-projectile counsel swiper ivy orgit ox-pandoc org org-plus-contrib nlinum gradle-mode company-emacs-eclim eclim rtags zotelo hide-comnt origami magit-gh-pulls github-search github-clone github-browse-file gist gh marshal logito pcache spinner log4e gntp parent-mode gitignore-mode fringe-helper git-gutter+ seq pos-tip pkg-info epl flx goto-chg undo-tree diminish bind-key pythonic request powerline rust-mode package-build tablist hydra ht alert markdown-mode projectile git-gutter magit magit-popup iedit levenshtein packed anaconda-mode avy auto-complete auctex yasnippet ghc haskell-mode company highlight anzu smartparens bind-map irony flycheck git-commit with-editor helm helm-core popup async f dash s evil company-racer deferred yapfify py-isort org-projectile mwim intero hlint-refactor git-link flyspell-correct-helm flyspell-correct evil-unimpaired dumb-jump company-ghci color-identifiers-mode cargo zenburn-theme xterm-color ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org spacemacs-theme spaceline solarized-theme smooth-scrolling smeargle shm shell-pop restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters racer quelpa pyvenv pytest pyenv-mode py-yapf popwin pip-requirements persp-mode pcre2el paradox page-break-lines org-repo-todo org-present org-pomodoro org-download org-bullets open-junk-file neotree multi-term move-text monokai-theme mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum live-py-mode linum-relative link-hint leuven-theme info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flyspell helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate google-c-style golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe git-gutter-fringe+ gh-md flycheck-rust flycheck-pos-tip flycheck-irony flycheck-haskell flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help elisp-slime-nav disaster diff-hl define-word cython-mode cuda-mode company-statistics company-quickhelp company-irony-c-headers company-irony company-ghc company-cabal company-c-headers company-auctex company-anaconda column-enforce-mode cmm-mode cmake-mode cmake-ide clean-aindent-mode clang-format buffer-move bracketed-paste auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk aggressive-indent ag adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-automatically-star t)
 '(paradox-github-token t)
 '(racer-cmd "/usr/bin/racer")
 '(racer-rust-src-path "/usr/src/rust/src")
 '(reftex-cite-format (quote natbib))
 '(safe-local-variable-values
   (quote
    ((cmake-ide-build-dir . "/home/alex/Projects/1Dpic/ninja")
     (cmake-ide-dir . "/home/alex/Projects/1Dpic/ninja")
     (zotero-collection . "20")
     (zotero-collection .
                        #("1" 0 1
                          (name "Numerical")))
     (TeX-engine quote luatex)
     (zotero-collection .
                        #("16" 0 2
                          (name "Thesis")))
     (zotero-collection
      #("18" 0 2
        (name "Thesis")))
     (reftex-default-bibliography "thesis.bib")
     (zotero-collection .
                        #("10" 0 2
                          (name "PICpaper")))
     (cmake-ide-build-dir . "/home/alex/Programs/Aperture/ninja")
     (cmake-ide-build-dir . "/home/alex/Programs/Aperture2/ninja")
     (cmake-ide-dir . "/home/alex/Programs/Aperture/ninja")
     (cmake-ide-dir . "/home/alex/Programs/Aperture2/ninja")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
)
