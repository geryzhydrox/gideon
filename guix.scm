(use-modules
 (guix build-system guile)
 (gnu packages guile)
 (guix packages)
 (guix download)
 (guix gexp)
 (guix git-download)
 (guix licenses))

(package
 (name "zeta")
 (version "0.1")
 (source
  (origin
   (method git-fetch)
   (uri (git-reference
	 (url "https://github.com/geryzhydrox/zeta")
	 (commit "900b0b908fec2d53f8b2bbb8f377f19d433fc9d4")))
   (sha256
    (base32 "0p6qyrrrnlqb0i16w7lym0xnwyc4dcz2srb5gm57v9ckzffv7rsq"))))
 (build-system guile-build-system)
 (arguments
  '(#:phases (modify-phases %standard-phases
			    (add-after 'build 'install
				       (lambda* (#:key outputs #:allow-other-keys)
					 (let* ((out (assoc-ref outputs "out"))
						(bin (string-append out "/bin")))
					   (install-file "zeta" bin)
					   ))))))
 (native-inputs (list
		 guile-3.0
		 guile-readline))
 (propagated-inputs (list
		     guile-3.0
		     guile-readline))
 (synopsis "Imperative `guix` wrapper.")
 (description "")
 (home-page "https://github.com/geryzhydrox/zeta")
 (license gpl3))
