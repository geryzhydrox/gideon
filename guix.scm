(use-modules
 (guix build-system guile)
 (gnu packages guile)
 (guix packages)
 (guix download)
 (guix gexp)
 (guix git-download)
 (guix licenses))

(package
 (name "gideon")
 (version "0.1")
 (source
  (origin
   (method git-fetch)
   (uri (git-reference
	 (url "https://github.com/geryzhydrox/gideon")
	 (commit "f5f0f7a3395d77d3f4627637d04c324e34597ff0")))
   (sha256
    (base32 "132la0dx2wz2004aig6yrj18l7v46sflxbky8nkhfqgsxxhwr2q9"))))
 (build-system guile-build-system)
 (arguments
  '(#:source-directory "src"
    #:phases (modify-phases %standard-phases
			    (add-after 'build 'install
				       (lambda* (#:key outputs #:allow-other-keys)
					 (let* ((out (assoc-ref outputs "out"))
						(bin (string-append out "/bin")))
					   (install-file "src/gideon" bin)
					   ))))))
 (native-inputs (list
		 guile-3.0
		 guile-readline))
 (propagated-inputs (list
		     guile-3.0
		     guile-readline))
 (synopsis "Imperative `guix` wrapper.")
 (description "")
 (home-page "https://github.com/geryzhydrox/gideon")
 (license gpl3))
