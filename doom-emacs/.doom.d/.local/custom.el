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
 '(company-lsp-enable-snippet t)
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(doom-treemacs-enable-variable-pitch nil)
 '(doom-treemacs-use-generic-icons nil)
 '(doxymacs-doxygen-style "C++")
 '(hideshowvis-ignore-same-line nil)
 '(linum-disabled-modes-list
   (quote
    (eshell-mode wl-summary-mode compilation-mode text-mode dired-mode)))
 '(lsp-enable-completion-at-point nil)
 '(lsp-prefer-flymake nil)
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(org-format-latex-options
   (quote
    (:foreground default :background "#282c34" :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
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
    ((magit-todos-exclude-globs "deps/*")
     (magit-todos-exclude-globs . deps/*)
     (cmake-ide-build-dir . "/home/alex/Projects/Aperture/ninja")
     (cmake-ide-dir . "/home/alex/Projects/Aperture/ninja")
     (cmake-ide-build-dir . "/home/alex/Projects/Aperture2/ninja")
     (cmake-ide-dir . "/home/alex/Projects/Aperture2/ninja")
     (cmake-ide-build-dir . "/home/alex/Projects/1Dpic/ninja")
     (cmake-ide-dir . "/home/alex/Projects/1Dpic/ninja")
     (zotero-collection . "20")
     ((cmake-ide-build-dir . "/home/alex/Projects/Aperture/ninja")
      (cmake-ide-dir . "/home/alex/Projects/Aperture/ninja")
      (cmake-ide-build-dir . "/home/alex/Projects/1Dpic/ninja")
      (cmake-ide-dir . "/home/alex/Projects/1Dpic/ninja")
      company-clang-arguments "-std=c++11" "-I/home/alex/Programs/Pulsar/include/" "-I/home/alex/Programs/Pulsar/CudaLE/include/"))))
 '(send-mail-function (quote smtpmail-send-it))
 '(sp-ignore-modes-list (quote (minibuffer-inactive-mode org-mode)))
 '(writeroom-global-effects
   (quote
    (writeroom-toggle-fullscreen writeroom-toggle-alpha writeroom-toggle-menu-bar-lines writeroom-toggle-tool-bar-lines writeroom-toggle-vertical-scroll-bars writeroom-toggle-internal-border-width)))
 '(writeroom-restore-window-config t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
