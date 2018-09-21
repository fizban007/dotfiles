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
 '(aw-ignore-current t)
 '(aw-scope (quote frame))
 '(cdlatex-command-alist
   (quote
    (("eqa" "Insert an EQNARRAY environment template" "" cdlatex-environment
      ("eqnarray")
      t nil)
     ("eqn" "Insert an EQUATION environment template" "" cdlatex-environment
      ("equation")
      t nil))))
 '(cdlatex-math-modify-alist (quote ((66 "\\boldsymbol" "" t nil nil))))
 '(cdlatex-math-symbol-alist
   (quote
    ((54
      ("\\partial"))
     (120
      ("\\xi"))
     (99
      ("\\chi"))
     (70
      ("\\Phi")))))
 '(company-lighter-base "C")
 '(custom-safe-themes
   (quote
    ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(doxymacs-doxygen-style "C++")
 '(eclim-executable "/opt/cuda/libnsight/plugins/org.eclim_1.7.19/bin/eclim")
 '(eclimd-default-workspace "~/Programs")
 '(hideshowvis-ignore-same-line nil)
 '(linum-disabled-modes-list
   (quote
    (eshell-mode wl-summary-mode compilation-mode text-mode dired-mode)))
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(org-export-backends (quote (ascii html icalendar latex md)))
 '(package-selected-packages
   (quote
    (visual-fill-column-mode visual-fill-column flx doom-themes modern-cpp-font-lock yasnippet-snippets neotree ivy-rich all-the-icons-ivy spaceline-all-the-icons delight all-the-icons rainbow-delimiters material-theme flycheck-ycmd flycheck elpy cmake-mode smex markdown-mode wgrep-ag ag ace-window pdf-tools htmlize ox-pandoc company-ycmd ycmd cuda-mode cdlatex auctex smartparens smartparens-mode counsel-projectile projectile ghub magit company-c-headers company yasnippet spacemacs-theme atom-one-dark atom-one-dark-theme moe-theme session use-package paradox evil-surround evil-nerd-commenter)))
 '(projectile-mode-line (quote (:eval (format " P[%s]" (projectile-project-name)))))
 '(projectile-other-file-alist
   (quote
    (("cuh" "cu")
     ("cu" "h" "cuh")
     ("C" "h" "hpp")
     ("cpp" "h" "hpp" "ipp")
     ("ipp" "h" "hpp" "cpp")
     ("hpp" "h" "ipp" "cpp")
     ("cxx" "hxx" "ixx")
     ("ixx" "cxx" "hxx")
     ("hxx" "ixx" "cxx")
     ("c" "h")
     ("m" "h")
     ("mm" "h")
     ("h" "c" "cpp" "ipp" "hpp" "m" "mm" "cu" "C")
     ("cc" "hh")
     ("hh" "cc")
     ("vert" "frag")
     ("frag" "vert")
     (nil "lock" "gpg")
     ("lock" "")
     ("gpg" ""))))
 '(safe-local-variable-values
   (quote
    ((cmake-ide-build-dir . "/home/alex/Projects/1Dpic/ninja")
     (cmake-ide-dir . "/home/alex/Projects/1Dpic/ninja")
     company-clang-arguments "-std=c++11" "-I/home/alex/Programs/Pulsar/include/" "-I/home/alex/Programs/Pulsar/CudaLE/include/")))
 '(send-mail-function (quote smtpmail-send-it))
 '(session-use-package t nil (session))
 '(writeroom-global-effects
   (quote
    (writeroom-toggle-fullscreen writeroom-toggle-alpha writeroom-toggle-menu-bar-lines writeroom-toggle-tool-bar-lines writeroom-toggle-vertical-scroll-bars writeroom-toggle-internal-border-width)))
 '(writeroom-restore-window-config t)
 '(ycmd-file-type-map
   (quote
    ((cuda-mode "cuda")
     (c++-mode "cpp")
     (c-mode "c")
     (caml-mode "ocaml")
     (csharp-mode "cs")
     (d-mode "d")
     (erlang-mode "erlang")
     (emacs-lisp-mode "elisp")
     (go-mode "go")
     (js-mode "javascript")
     (js2-mode "javascript")
     (lua-mode "lua")
     (objc-mode "objc")
     (perl-mode "perl")
     (cperl-mode "perl")
     (php-mode "php")
     (python-mode "python")
     (ruby-mode "ruby")
     (rust-mode "rust")
     (swift-mode "swift")
     (scala-mode "scala")
     (tuareg-mode "ocaml")
     (typescript-mode "typescript")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
