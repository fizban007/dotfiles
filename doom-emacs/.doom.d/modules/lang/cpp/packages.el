;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! google-c-style)
(package! cuda-mode)
(package! opencl-mode)
(package! highlight-doxygen)
;; (package! demangle-mode)
;; (package! disaster)
(package! modern-cpp-font-lock)
(package! cmake-mode)

(package! ccls)

(when (package! glsl-mode)
  (when (featurep! :completion company)
    (package! company-glsl :recipe (:fetcher github :repo "Kaali/company-glsl"))))

;; (if (featurep! +lsp)
;;     (package! ccls)
;;   (when (package! irony)
;;     (package! irony-eldoc)
;;     (when (featurep! :tools flycheck)
;;       (package! flycheck-irony))
;;     (when (featurep! :completion company)
;;       (package! company-irony)
;;       (package! company-irony-c-headers)))
;;   (when (package! rtags)
;;     (when (featurep! :completion ivy)
;;       (package! ivy-rtags))
;;     (when (featurep! :completion helm)
;;       (package! helm-rtags))))
