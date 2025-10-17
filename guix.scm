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
	 (commit "4ae2ed0e18fdfc4ac2b0e5fcf6a8579266f0b894")))
   (sha256
    (base32 "0x1vpv0y5i85x59pj7dh427kdjxy7lzshwadapjil40x0rzlsi2l"))))
 (build-system guile-build-system)
 (arguments
  '(#:source-directory "src"
    #:phases (modify-phases %standard-phases
			    (add-after 'build 'install
				       (lambda* (#:key outputs #:allow-other-keys)
					 (let* ((out (assoc-ref outputs "out"))
						(bin (string-append out "/bin")))
					   (install-file "gideon" bin)
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
